#![allow(incomplete_features)]
#![feature(const_generics, const_evaluatable_checked, const_panic, int_bits_const)]

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
pub struct MpfrBounds {
    /// Intentionally private, to guard integrity.
    precision_bits_length: usize,
    limb_parts_length: usize
}

impl MpfrBounds {
    const fn for_precision_bits_length(precision_bits_length: usize) -> Self {
        Self {
            precision_bits_length,
            /// Based on mfpr::MPFR_DECL_INIT
            limb_parts_length: (precision_bits_length - 1) / gmp::NUMB_BITS 
                as usize + 1
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatChoice {
    F32, F64, TwoFloat,
    Mpfr {
        bounds: MpfrBounds
    }
}

impl FloatChoice {
    pub fn precision_bits_length(&self) -> usize {
        match *self {
            FloatChoice::F32 => f32::MANTISSA_DIGITS as usize,
            FloatChoice::F64 => f64::MANTISSA_DIGITS as usize,
            FloatChoice::TwoFloat => 2 * f64::MANTISSA_DIGITS as usize,
            FloatChoice::Mpfr {bounds: MpfrBounds { precision_bits_length, ..} } => precision_bits_length
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
                bounds: MpfrBounds::for_precision_bits_length(precision_bits_length)
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
    pub const fn most_precise_for_same_space(&self) -> Self {
        match *self {
            FloatChoice::Mpfr { bounds: MpfrBounds { limb_parts_length, .. }} =>
                // Based on reverse of mfpr::MPFR_DECL_INIT
                FloatChoice::Mpfr {
                    bounds: MpfrBounds::for_precision_bits_length(
                        limb_parts_length * gmp::NUMB_BITS as usize
                    )
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
            FloatChoice::Mpfr { bounds: MpfrBounds {limb_parts_length, ..} } => limb_parts_length,
            _ => 0
        }
    }

    pub const fn float_size(&self) -> usize {
        match *self {
            FloatChoice::F32 => mem::size_of::<Float<{ FloatChoice::F32 }>>(),
            FloatChoice::F64 => mem::size_of::<Float<{ FloatChoice::F64 }>>(),
            FloatChoice::TwoFloat => mem::size_of::<Float<{ FloatChoice::TwoFloat }>>(),
            FloatChoice::Mpfr { bounds: MpfrBounds {limb_parts_length, ..}} => {
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
/// These functions are not a part of public API. They are public only because
/// otherwise we were getting "private type `fn(isize) -> usize
/// f32_parts_length}` in public interface (error E0446)".
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
    float_self: * const Float<C>
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

#[cfg(test)]
mod tests {
    use {std::mem, gmp_mpfr_sys::gmp};
    use mem::size_of;

    use crate::{Float, FloatChoice, MpfrBounds};
    
    /// How many limb_t instances may fit into one usize? Normally one
    /// (unless MPFR was compiled with different pointer width than Rust).
    const USIZE_TO_LIMB_T: usize = mem::size_of::<usize>() / mem::size_of::<gmp::limb_t>();
    const ONE_LIMB_PRECISION: usize = usize::BITS as usize / USIZE_TO_LIMB_T;

    type UniF32 = Float<{ FloatChoice::F32 }>;
    type F64 = Float<{ FloatChoice::F64 }>;
    type TwoFloat = Float<{ FloatChoice::TwoFloat }>;
    // Types with names starting with MPFR_LIMB_x use `x` number of limbs.
    type MPFR_LIMB_1_PREC_1 = Float<{ FloatChoice::Mpfr { bounds: MpfrBounds {
        limb_parts_length: 1,
        precision_bits_length: 1,
    }}}>;
    // Types with names ending with _USED use most of the whole precision
    // available for their number of limbs (so they can't fit into a smaller
    // number of limbs), but they don't use the whole available precision.
    type MPFR_LIMB_1_PREC_USED = Float<{ FloatChoice::Mpfr { bounds: MpfrBounds {
        limb_parts_length: 1,
        precision_bits_length: 1 * ONE_LIMB_PRECISION,
    }}}>;
    // Types with names ending with _WHOLE use the whole precision available
    // for their number of limbs.
    type MPFR_LIMB_1_PREC_WHOLE = Float<{ FloatChoice::Mpfr { bounds: MpfrBounds {
        limb_parts_length: 1,
        precision_bits_length: 1 * ONE_LIMB_PRECISION,
    }}}>;
    // This helps when calculating size and alignment of `Float.float_self`
    // pointer. That
    // field exists in debug mode only. When on a 64+ bit platform,
    // `float_self` may increase alignment (and hence the size) of F32. That's
    // OK, since it does not affect release.)
    // (We don't support 16 bit platform, but 32+ bit only.)
    const POINTER_ALIGN: usize = mem::align_of::<* const usize>();
    const POINTER_SIZE: usize = mem::size_of::<* const usize>();

    /// Use assertions, so the checks are run only in debug mode (where `Float`
    /// has field `float_self` - that's why we add `POINTER_SIZE`).
    #[test]
    fn debug_assert_type_sizes() {
        assert!(   mem:: size_of::<f32>() <= POINTER_SIZE ); // On 32+ bit
        assert_eq!(mem::align_of::<UniF32>(), POINTER_ALIGN);
        // POINTER_SIZE is the same as mem::size_of::<f32> (on 32 bit platform),
        // or larger (on 64+ bit platform - then f32 part of F32 aligns to).
        assert_eq!(mem:: size_of::<UniF32>(), 2 * POINTER_SIZE);
        
        const UNI_F64_ALIGN: usize = mem:: size_of::<f64>().max(POINTER_ALIGN);
        assert_eq!(mem::align_of::<F64>(), UNI_F64_ALIGN);
        // Following should work on a 128+ bit platform, too.
        assert_eq!(mem::size_of::<F64>(),
            2* mem::size_of::<f64>().max(POINTER_SIZE) );

        assert_eq!(mem::size_of::<TwoFloat>(),
            mem::size_of::<twofloat::TwoFloat>() + POINTER_SIZE);
        
        //assert_eq!(mem::size_of::<MPFR_LIMB_1_PREC_1>(),
    }

    fn check(expectation: bool) {
        if !expectation {
            panic!();
        }
    }

    /// For non-debug mode (where Float doesn't have field float_self).
    #[test]
    fn non_debug_check_type_sizes() {
        let mut non_debug_run = true;
        assert!( {
            non_debug_run = false;
            true
        });
        if non_debug_run {

        }
    }

    #[test]
    fn misc() {
    }

}
