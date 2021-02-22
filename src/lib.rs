#![allow(incomplete_features)]
#![feature(const_generics, const_evaluatable_checked, const_panic, const_mut_refs)]

use {core::ops, core::ptr, core::mem, std::num, gmp_mpfr_sys::{mpfr, gmp}};

/// Across this crate: Const generic parameter S is NOT necessarily a number of
/// 64bit extras, but a number of any and all 64-bit
/// parts that contain the significand of the floating-point number. (Such parts
/// may be used fully or partially.)
/// When 0<=S<=2 Float is implemented by f32/f64/TwoFloat. Then S is less than
/// maximum length of the significand, because f32/f64/TwoFloat also include
/// the sign and the exponent (which are separate in MPFR - when S>2
/// or S==-1 or S==-2)!
/// TODO: If you'd like to use MPFR (rather than default f32/f64/TwoFloat) for
/// S<=2, use negative values instead: S=-1 or S=-2. You may want that if the
/// exponent is outside standard f32/f64 exponent range!

/// Difference to Rust (f32/f64) convention and TwoFloat: Default values are not zero, but NAN.
/// That is compatible with MPFR. It saves extra steps
/// and prevents mistakes with uninitialized values.

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatChoice {
    F32, F64, TwoFloat,
    Mpfr {
        precision_bits_length: usize,
        limb_parts_length: usize
    }
}

impl FloatChoice {
    pub fn precision_bits_length(&self) -> usize {
        match *self {
            FloatChoice::F32 => f32::MANTISSA_DIGITS as usize,
            FloatChoice::F64 => f64::MANTISSA_DIGITS as usize,
            FloatChoice::TwoFloat => 2 * f64::MANTISSA_DIGITS as usize,
            FloatChoice::Mpfr { precision_bits_length, .. } => precision_bits_length
        }
    }

    /// A Float instance based on the returned FloatChoice may also accommodate
    /// values outside the given bounds, but it's guaranteed to fulfill the
    /// given bounds.
    pub const fn for_binary_bounds(
        precision_bits_length: usize,
        min_exponent: i32,
        max_exponent: i32
    ) -> FloatChoice {
        assert!(
            precision_bits_length > 0,
            "MPFR requires the minimum precision (MPFR_PREC_MIN) of 1 bit."
        );

        if precision_bits_length <= f32::MANTISSA_DIGITS as usize
        && min_exponent >= f32::MIN_EXP
        && max_exponent <= f32::MAX_EXP {
            FloatChoice::F32
        } else
        if precision_bits_length <= f64::MANTISSA_DIGITS as usize
        && min_exponent >= f64::MIN_EXP
        && max_exponent <= f64::MAX_EXP {
            FloatChoice::F64
        } else
        if precision_bits_length <= 2* f64::MANTISSA_DIGITS as usize
        && min_exponent >= f64::MIN_EXP
        && max_exponent <= f64::MAX_EXP {
            FloatChoice::TwoFloat
        }
        else {
            FloatChoice::Mpfr {
                precision_bits_length,
                /// Based on mfpr::MPFR_DECL_INIT
                limb_parts_length: (precision_bits_length - 1) / gmp::NUMB_BITS 
                    as usize + 1
            }
        }
    }

    /// for_binary_bounds(...) tells you what FloatChoice you need to cover
    /// your bounds. But how much more precision can you fit in the same memory?
    /// This function gives you FloatChoice describing that.
    ///
    /// Beware that when `self` is FloatChoice::Mpfr, and if the result of this
    /// function indicates wider precision, then using a mix of Float instances
    /// based on both FloatChoice-s are not very compatible! (They involve MPFR
    /// rounding.) Then you don't want to base all related FloatChoice
    /// instances on self, but on the result of this function, instead.
    ///
    /// If `self` is already the most precise for its space, this may
    /// return (a copy of) self, or a new instance.
    pub fn most_precise_for_same_space(&self) -> Self {
        match *self {
            FloatChoice::Mpfr { limb_parts_length, ..} => {
                // Based on reverse of mfpr::MPFR_DECL_INIT
                FloatChoice::Mpfr {
                    precision_bits_length: limb_parts_length
                        * gmp::NUMB_BITS as usize,
                    limb_parts_length
                }
            },
            other => other
        }
    }

    pub const fn f32_parts_length(&self) -> usize {
        match self {
            FloatChoice::F32 => 1,
            _ => 0
        }
    }

    pub const fn f64_parts_length(&self) -> usize {
        match self {
            FloatChoice::F64 => 1,
            _ => 0
        }
    }

    pub const fn two_float_parts_length(&self) -> usize {
        match self {
            FloatChoice::TwoFloat => 1,
            _ => 0
        }
    }

    pub const fn mpfr_fixed_parts_length(&self) -> usize {
        match self {
            FloatChoice::Mpfr{ .. } => 1,
            _ => 0
        }
    }

    pub const fn mpfr_limb_parts_length(&self) -> usize {
        match *self {
            FloatChoice::Mpfr { limb_parts_length, .. } => limb_parts_length,
            _ => 0
        }
    }

    pub const fn float_size(&self) -> usize {
        match *self {
            FloatChoice::F32 => mem::size_of::<Float<{ FloatChoice::F32 }>>(),
            FloatChoice::F64 => mem::size_of::<Float<{ FloatChoice::F64 }>>(),
            FloatChoice::TwoFloat => mem::size_of::<Float<{ FloatChoice::TwoFloat }>>(),
            FloatChoice::Mpfr {limb_parts_length, ..} => {
                const ONE_LIMB_CHOICE: FloatChoice = FloatChoice::for_binary_bounds(1, 0, f64::MAX_EXP + 1);
                type OneLimbMpfr = Float<ONE_LIMB_CHOICE>;

                mem::size_of::<OneLimbMpfr>() + (limb_parts_length - 1)
                    * mem::size_of::<MpfrLimbPart>()
            }
        }
    }
    
}

/// `const fun` functions here whose names end with _parts_length(s: isize) -> usize
/// return the number of entries/slots of the respective type (f32, f64...) to
/// be used by the respective parts. (Not a number of bytes.)
/// Types and const genereric bounds are based on
/// https://github.com/rust-lang/rust/issues/68436. However, not all
/// tips from that discussion work with 1.52.0-nightly.

/// Public, otherwise we were getting "private type `fn(isize) -> usize {f32_parts_length}` in public interface (error E0446)"
pub const fn f32_parts_length(c: FloatChoice) -> usize {
    c.f32_parts_length()
}
type F32Parts<const C: FloatChoice> = [f32; f32_parts_length(C)];

pub const fn f64_parts_length(c: FloatChoice) -> usize {
    c.f64_parts_length()
}
type F64Parts<const C: FloatChoice> = [f64; f64_parts_length(C)];

pub const fn two_float_parts_length(c: FloatChoice) -> usize {
    c.two_float_parts_length()
}
type TwoFloatParts<const C: FloatChoice> = [twofloat::TwoFloat; two_float_parts_length(C)];

pub const fn mpfr_limb_parts_length(c: FloatChoice) -> usize {
    c.mpfr_limb_parts_length()
}
type MpfrLimbPart = mem::MaybeUninit<gmp::limb_t>;
type MpfrLimbParts<const C: FloatChoice> = [MpfrLimbPart; mpfr_limb_parts_length(C)];

pub const fn mpfr_fixed_parts_length(c: FloatChoice) -> usize {
    c.mpfr_fixed_parts_length()
}
// TODO
type MpfrFixedParts<const C: FloatChoice> = [mpfr::mpfr_t;mpfr_fixed_parts_length(C)];

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Float<const C: FloatChoice> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; two_float_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
{
    f32s: F32Parts<C>,
    f64s: F64Parts<C>,
    two_floats: TwoFloatParts<C>,
    mpfr_limbs: MpfrLimbParts<C>,
    mpfr_fixeds: MpfrFixedParts<C>,
    #[cfg(debug_assertions)]
    /// A pointer to Float instance itself. Used for extra .copied() check.
    /// Beneficial for testing the right usage of the API even without FloatChoice::Mpfr.
    float_self: *const Float<C>
}

/// Used internally only while initializing an MPFR float. This is never leaked to the user.
const DUMMY_MPFR_LIMB: i64 = 0;
const DUMMY_MPFR_LIMB_PTR: ptr::NonNull<gmp::limb_t> = unsafe {
    core::ptr::NonNull::new_unchecked(&DUMMY_MPFR_LIMB as *const _ as *mut gmp::limb_t)
};
/// Never leaked to the user.
/// Based on gmp_mpfr_sys::MPFR_DECL_INIT
const INITIAL_MPFR_EXP: mpfr::exp_t = 1-mpfr::exp_t::max_value();

impl <const C: FloatChoice> Default for Float<C> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; two_float_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
{
    fn default() -> Self {
        Self::nan()
    }
}

impl <const C: FloatChoice> Float<C> where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; two_float_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
{
    /// Not-a-Number.
    pub fn nan() -> Self {
        let mut result = Self {
            // The items to be copied into the arrays get evaluated, even if 
            /// array size is zero. However, rustc + LLVM can optimize it away.
            f32s: [f32::NAN; f32_parts_length(C)],
            f64s: [f64::NAN; f64_parts_length(C)],
            two_floats: [twofloat::TwoFloat::NAN; two_float_parts_length(C)],

            mpfr_limbs: unsafe { core::mem::MaybeUninit::uninit().assume_init() },
            
            mpfr_fixeds: [mpfr::mpfr_t {
                prec: 1,
                sign: 1,
                exp: INITIAL_MPFR_EXP,
                d: DUMMY_MPFR_LIMB_PTR
            }; mpfr_fixed_parts_length(C)],
            float_self: ptr::null()
        };
        *result.copied()
    }

    // Based on `gmp_mpfr_sys::MPFR_DECL_INIT`, but here we accept non-mutable
    // &self, because we use this in read-only asserts, too.
    fn mpfr_limps_ptr(&self) -> ptr::NonNull<gmp::limb_t> {
        unsafe {
            ptr::NonNull::new_unchecked(self.mpfr_limbs[..].as_ptr() as *mut gmp::limb_t)
        }
    }

    #[inline]
    fn assert_copy_fixed(&self) {
        assert!(
            if let FloatChoice::Mpfr { .. } = C {
                self.mpfr_fixeds[0].d == self.mpfr_limps_ptr()
            } else {
                true
            } && self.float_self == self,
            "Must call .copied() first, or assign with <<= instead of =.");
    }

    #[inline]
    fn assert_copy_not_fixed(&self) {
        assert!(
            if let FloatChoice::Mpfr { .. } = C {
                self.mpfr_fixeds[0].d != self.mpfr_limps_ptr()
            } else {
                true
            }  && self.float_self != self,
            "Have already called .copied(), or assigned with <<= instead of =. Do not call .copied() now.");
    }

    /// Call this exactly one after a Float value is copied to:
    /// Assigned to, received as a 
    /// parameter, received from a result of a called function, copied from a
    /// struct/tuple/array. Also if was assigned from a result of an expression.
    /// However, do not call this if the value was assigned to with <<= 
    /// operator.
    pub fn copied(&mut self) -> &mut Self {
        self.assert_copy_not_fixed();
        if let FloatChoice::Mpfr { .. } = C {
            self.mpfr_fixeds[0].d = self.mpfr_limps_ptr();
        }
        assert!({
            self.float_self = self as *const _ as *const Float<C>;
            true
        });
        self
    }
}

pub fn copied(floats: &mut [i32]) {

}

/*impl Default for Float<0> {
    fn default() -> Self {
        Self {
            f32_parts: [0.0; 1],
            f64_parts: [],
            two_float_parts: [],
            significand: []
        }
    }
}*/

#[cfg(test)]
mod tests {
    use std::mem;
    use crate::{Float, FloatChoice};
    
    #[test]
    fn misc() {
    }

    #[test]
    fn size() {
        let f0 = <Float<{ FloatChoice::F32 }>>::default();
        let f1: Float<{ FloatChoice::F64 }> = Float::default();
        println!("Size of Float<0>: {}", mem::size_of_val(&f0));
        println!("Size of Float<1>: {}", mem::size_of_val(&f1))
    }
}
