#![allow(incomplete_features)]
#![feature(const_generics, const_evaluatable_checked, const_panic, int_bits_const, const_maybe_uninit_assume_init, const_fn_floating_point_arithmetic)]
#![no_std]

mod operands;
mod tests;

use {core::ops, core::ptr, core::mem, core::num, gmp_mpfr_sys::{mpfr, gmp}};

pub use operands::{OperandMutated, OperandOwned};

/// Across this crate: Const generic parameter S is NOT necessarily a number of
/// 64bit extras, but a number of any and all 64-bit
/// parts that contain the significand of the floating-point number. (Such parts
/// may be used fully or partially.)
/// When 0<=S<=2 UniFloat is implemented by f32/f64/TwoFloat. Then S is less than
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
    precision_bits: usize,
    limb_parts: usize
}

impl MpfrBounds {
    const fn for_precision_binary(precision_bits: usize) -> Self {
        Self {
            precision_bits,
            /// Based on mfpr::MPFR_DECL_INIT
            limb_parts: (precision_bits - 1) / gmp::NUMB_BITS 
                as usize + 1
        }
    }
    // Once https://github.com/rust-lang/rust/pull/80918 is merged, consider #![feature(int_log)] instead. Then see if you can make this function `const`.
    fn for_precision_decimal(precision_decimal: usize) -> Self {
        let precision_bits = (precision_decimal as f32 * core::f32::consts::LOG10_2).ceil() as usize;
        Self::for_precision_binary(precision_bits)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UniFloatChoice {
    F32, F64, TwoFloat,
    Mpfr {
        bounds: MpfrBounds
    }
}

// Not public. Let's promote as generic solutions as possible.
type UniF32 = UniFloat<{ UniFloatChoice::F32 }>;
type UniF64 = UniFloat<{ UniFloatChoice::F64 }>;
type UniTwoFloat = UniFloat<{ UniFloatChoice::TwoFloat }>;
// Types with names starting with `UniMpfrLimbX` use `X` number of limbs.
type UniMpfrLimb1Prec1 = UniFloat<{ UniFloatChoice::Mpfr { bounds: MpfrBounds {
    limb_parts: 1,
    precision_bits: 1,
}}}>;

const ONE_LIMB_PRECISION: usize = gmp::limb_t::BITS as usize;
// Types with names like UniMpfrLimbxPrecAll use all the precision available
// for their number of limbs.
type UniMpfrLimb2PrecAll = UniFloat<{ UniFloatChoice::Mpfr { bounds: MpfrBounds {
    limb_parts: 2,
    precision_bits: 2 * ONE_LIMB_PRECISION,
}}}>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UniFloatBoundsBase { DECIMAL, BINARY }

/// Definition of bounds guaranteed by a related UniFloat type. BASE is a const generic rather than a field, because binary and decimal bounds are not interchangeable (due to rounding). Prefer BASE being BINARY.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniFloatBounds<const BASE: UniFloatBoundsBase> {
    // Matches gmp::limb_t, which in GMP C library is mp_limb_t, which is unsigned.
    precision: usize,
    min_exponent: isize,
    max_exponent: isize
}

/// Helper for returning constants from a const generic `UniFloatBounds`.
/// The trait itself sot a part of public API. It's public only because of Rust requirements.
pub trait UniFloatBoundsToChoice {
    /// A UniFloat instance based on the returned UniFloatChoice may also accommodate
    /// values outside the given bounds, but it's guaranteed to fulfill the
    /// given bounds. However, for MPFR it uses the exact number of bits, even if there were more
    /// bits available for the same amount of allocated memory. For that use
    /// UniFloatChoice::most_precise_for_same_space().
    /// Prefer `bounds` at binary base.
    fn to_choice(&self) -> UniFloatChoice;
}

impl UniFloatBoundsToChoice for UniFloatBounds<{ UniFloatBoundsBase::BINARY }> {
    fn to_choice(&self) -> UniFloatChoice {
        if F32_BOUNDS_BINARY.covers(self) {
            UniFloatChoice::F32
        } else if F64_BOUNDS_BINARY.covers(self) {
            UniFloatChoice::F64
        } else if TWOFLOAT_BOUNDS_BINARY.covers(self) {
            UniFloatChoice::TwoFloat
        } else {
            UniFloatChoice::Mpfr {
                bounds: MpfrBounds::for_precision_binary(self.precision)
            }
        }
    }
}

impl UniFloatBoundsToChoice for UniFloatBounds<{ UniFloatBoundsBase::DECIMAL }> {
    fn to_choice(&self) -> UniFloatChoice {
        panic!()
    }
}

impl <const BASE: UniFloatBoundsBase> UniFloatBounds<BASE> {
    pub fn new(precision: usize, min_exponent: isize, max_exponent: isize) -> Self {
        Self { precision, min_exponent, max_exponent}
    }

    /// Whether `self` accommodates all needs of `other`. Prefer both `self` and `other` at BINARY base.
    pub const fn covers(&self, other: &Self) -> bool {
        self.precision >= other.precision && self.min_exponent <= other.min_exponent && self.max_exponent >= other.max_exponent
    }
}

const F32_BOUNDS_BINARY: UniFloatBounds<{ UniFloatBoundsBase::BINARY }> = UniFloatBounds::<{ UniFloatBoundsBase::BINARY }> {
    precision: f32::MANTISSA_DIGITS as usize,
    min_exponent: f32::MIN_EXP as isize,
    max_exponent: f32::MAX_EXP as isize
};
const F32_BOUNDS_DECIMAL: UniFloatBounds<{ UniFloatBoundsBase::DECIMAL }> = UniFloatBounds::<{ UniFloatBoundsBase::DECIMAL }> {
    precision: f32::DIGITS as usize,
    min_exponent: f32::MIN_10_EXP as isize,
    max_exponent: f32::MAX_10_EXP as isize
};
const F64_BOUNDS_BINARY: UniFloatBounds<{ UniFloatBoundsBase::BINARY }> = UniFloatBounds::<{ UniFloatBoundsBase::BINARY }> {
    precision: f64::MANTISSA_DIGITS as usize,
    min_exponent: f64::MIN_EXP as isize,
    max_exponent: f64::MAX_EXP as isize
};
const F64_BOUNDS_DECIMAL: UniFloatBounds<{ UniFloatBoundsBase::DECIMAL }> = UniFloatBounds::<{ UniFloatBoundsBase::DECIMAL }> {
    precision: f64::DIGITS as usize,
    min_exponent: f64::MIN_10_EXP as isize,
    max_exponent: f64::MAX_10_EXP as isize
};
const TWOFLOAT_BOUNDS_BINARY: UniFloatBounds<{ UniFloatBoundsBase::BINARY }> = UniFloatBounds::<{ UniFloatBoundsBase::BINARY }> {
    precision: 2* f64::MANTISSA_DIGITS as usize,
    min_exponent: f64::MIN_EXP as isize,
    max_exponent: f64::MAX_EXP as isize
};
const TWOFLOAT_BOUNDS_DECIMAL: UniFloatBounds<{ UniFloatBoundsBase::DECIMAL }> = UniFloatBounds::<{ UniFloatBoundsBase::DECIMAL }> {
    precision: 2 * f64::DIGITS as usize,
    min_exponent: f64::MIN_10_EXP as isize,
    max_exponent: f64::MAX_10_EXP as isize
};

/// Helper so we can return constants from const-generic UniFloatChoice::bounds().
/// Not a part of public API. It's public only because of Rust requirements.
/// Thanks to Kevin Reid https://github.com/kpreid for this pattern.
pub trait UniFloatChoiceToBounds {
    fn to_bounds(choice: &UniFloatChoice) -> Self;
}

impl UniFloatChoiceToBounds for UniFloatBounds<{ UniFloatBoundsBase::BINARY }> {
    fn to_bounds(choice: &UniFloatChoice) -> Self {
        match *choice {
            UniFloatChoice::F32 => F32_BOUNDS_BINARY,
            UniFloatChoice::F64 => F64_BOUNDS_BINARY,
            UniFloatChoice::TwoFloat => TWOFLOAT_BOUNDS_BINARY,
            UniFloatChoice::Mpfr { bounds: MpfrBounds { precision_bits, ..}} => UniFloatBounds::<{ UniFloatBoundsBase::BINARY }> {
                precision: precision_bits,
                min_exponent: isize::MIN,
                max_exponent: isize::MAX
            }
        }
    }
}
impl UniFloatChoiceToBounds for UniFloatBounds<{ UniFloatBoundsBase::DECIMAL }> {
    fn to_bounds(choice: &UniFloatChoice) -> Self {
        match *choice {
            UniFloatChoice::F32 => F32_BOUNDS_DECIMAL,
            UniFloatChoice::F64 => F64_BOUNDS_DECIMAL,
            UniFloatChoice::TwoFloat => TWOFLOAT_BOUNDS_DECIMAL,
            UniFloatChoice::Mpfr { bounds: MpfrBounds { precision_bits, ..}} => UniFloatBounds::<{ UniFloatBoundsBase::DECIMAL }> {
                precision: (precision_bits as f32 * core::f32::consts::LOG10_2).floor() as usize,
                min_exponent: (isize::MIN as f32 * core::f32::consts::LOG10_2).ceil() as isize,
                max_exponent: (isize::MAX as f32 * core::f32::consts::LOG10_2).floor() as isize
            }
        }
    }
}

impl UniFloatChoice {
    pub fn bounds<const BASE: UniFloatBoundsBase>(&self) -> UniFloatBounds::<{ BASE }>
    where
    UniFloatBounds<BASE>: UniFloatChoiceToBounds
    {
        UniFloatChoiceToBounds::to_bounds(self)
    }

    /// Whether `self` accommodates all needs of `other`. Prefer both `self` and `other` at BINARY base.
    pub fn covers(&self, other: &Self) -> bool {
        let mine = self.bounds::<{ UniFloatBoundsBase::BINARY }>();
        let their = other.bounds::<{ UniFloatBoundsBase::BINARY }>();
        mine.covers(&their)
    }

    /// for_binary_bounds(...) tells you what UniFloatChoice you need to cover
    /// your bounds. But how much more precision can you fit in the same memory?
    /// This function gives you UniFloatChoice describing that.
    ///
    /// Beware that when `self` is UniFloatChoice::Mpfr, and if the result of this
    /// function indicates wider precision, then using a mix of UniFloat instances
    /// based on both UniFloatChoice-s are not very compatible! (They involve MPFR
    /// rounding.) Then you don't want to base all related UniFloatChoice
    /// instances on self, but on the result of this function, instead.
    ///
    /// If `self` is already the most precise for its space, this may
    /// return (a copy of) self, or a new instance.
    pub const fn most_precise_for_same_space(&self) -> Self {
        match *self {
            UniFloatChoice::Mpfr { bounds: MpfrBounds { limb_parts: limb_parts_length, .. }} =>
                // Based on reverse of mfpr::MPFR_DECL_INIT
                UniFloatChoice::Mpfr {
                    bounds: MpfrBounds::for_precision_binary(
                        limb_parts_length * gmp::NUMB_BITS as usize
                    )
                },
            other => other
        }
    }

    /// Size of any `UniFloat` instance created for this `UniFloatChoice`, in
    /// bytes. Beware that this involves extra space when in debug mode.
    /// Also, beware that without `f32_only` feature, F32-based UniFloat takes as much space
    /// as F64-based UniFloat (when using either on a 64 bit platform).
    pub const fn unifloat_size(&self) -> usize {
        match *self {
            UniFloatChoice::F32 => mem::size_of::<UniF32>(),
            UniFloatChoice::F64 => mem::size_of::<UniF64>(),
            UniFloatChoice::TwoFloat => mem::size_of::<UniTwoFloat>(),
            UniFloatChoice::Mpfr { bounds: MpfrBounds {limb_parts: limb_parts_length, ..}} => {
                mem::size_of::<UniMpfrLimb1Prec1>()
                    + (limb_parts_length - 1)
                      * (   mem::size_of::<UniMpfrLimb2PrecAll>()
                          - mem::size_of::<UniMpfrLimb1Prec1>())

            }
        }
    }
    
}

// `const fun` functions here whose names end with _parts_length(s: isize) -> usize
// return the number of entries/slots of the respective type (f32, f64...) to
/// Number of `f32` parts in UniFloat. Either 0 or 1.
/// be used by the respective parts. (Not a number of bytes.)
/// Not a part of public API. It's public only because of Rust requirements.
pub const fn f32_parts_length(c: UniFloatChoice) -> usize {
    match c {
        UniFloatChoice::F32 => 1,
        _ => 0
    }

}
type F32Parts<const C: UniFloatChoice> = [f32; f32_parts_length(C)];

/// Number of `f64` parts in UniFloat. Either 0 or 1.
/// Not a part of public API. It's public only because of Rust requirements.
pub const fn f64_parts_length(c: UniFloatChoice) -> usize {
    match c {
        UniFloatChoice::F64 => 1,
        _ => 0
    }

}
#[allow(dead_code)] // not used with f32_only feature.
type F64Parts<const C: UniFloatChoice> = [f64; f64_parts_length(C)];

/// Number of `twofloat::TwoFloat` parts in UniFloat. Either 0 or 1.
/// Not a part of public API. It's public only because of Rust requirements.
pub const fn twofloat_parts_length(c: UniFloatChoice) -> usize {
    match c {
        UniFloatChoice::TwoFloat => 1,
        _ => 0
    }
}
#[allow(dead_code)]
type TwoFloatParts<const C: UniFloatChoice> = [twofloat::TwoFloat; twofloat_parts_length(C)];

/// Number of `gmp::limb_t` parts in UniFloat. Either 0 or a positive number, depending on precision indicated by `c`.
/// Not a part of public API. It's public only because of Rust requirements.
pub const fn mpfr_limb_parts_length(c: UniFloatChoice) -> usize {
    match c {
        UniFloatChoice::Mpfr { bounds: MpfrBounds {limb_parts: limb_parts_length, ..} } => limb_parts_length,
        _ => 0
    }
}
type MpfrLimbPart = mem::MaybeUninit<gmp::limb_t>;
#[allow(dead_code)]
type MpfrLimbParts<const C: UniFloatChoice> = [MpfrLimbPart; mpfr_limb_parts_length(C)];

/// Number of `mpfr::mpfr_t` parts in UniFloat. Either 0 or 1.
/// Not a part of public API. It's public only because of Rust requirements.
pub const fn mpfr_fixed_parts_length(c: UniFloatChoice) -> usize {
    match c {
        UniFloatChoice::Mpfr{ .. } => 1,
        _ => 0
    }
}
#[allow(dead_code)]
type MpfrFixedParts<const C: UniFloatChoice> = [mpfr::mpfr_t;mpfr_fixed_parts_length(C)];

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct UniFloat<const C: UniFloatChoice> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    // When you initialize the arrays with `[item; array_length]`, `item` gets evaluated, even if
    // array_length is zero. However, rustc + LLVM can optimize it away.
    f32s: F32Parts<C>,
    #[cfg(not(feature = "f32_only"))]
    f64s: F64Parts<C>,
    #[cfg(not(feature = "f32_only"))]
    twofloats: TwoFloatParts<C>,

    #[cfg(not(feature = "f32_only"))]
    mpfr_fixeds: MpfrFixedParts<C>,
    #[cfg(not(feature = "f32_only"))]
    mpfr_limbs: MpfrLimbParts<C>,

    #[cfg(debug_assertions)]
    /// A pointer to UniFloat instance itself. Used for extra .copied() check.
    /// Beneficial for testing the right usage of the .copied() and <<= API even without UniFloatChoice::Mpfr.
    unifloat_self: * const UniFloat<C>,
    #[cfg(debug_assertions)]
    /// A (limited) safeguard for confirming that we've applied .mutate() on the same instance only once - until it's cleared with .copied() or <<=.
    /// Or that it's owned by OperandOwned.
    used_as_operand_mutated: bool
}

/// Used internally only while initializing an MPFR float. This is never leaked to the user.
#[allow(dead_code)]
const DUMMY_MPFR_LIMB: i64 = 0;
#[allow(dead_code)]
const DUMMY_MPFR_LIMB_PTR: ptr::NonNull<gmp::limb_t> = unsafe {
    core::ptr::NonNull::new_unchecked(&DUMMY_MPFR_LIMB as *const _ as *mut gmp::limb_t)
};
/// Never leaked to the user.
/// Based on gmp_mpfr_sys::MPFR_DECL_INIT
#[allow(dead_code)]
const INITIAL_MPFR_EXP: mpfr::exp_t = 1-mpfr::exp_t::max_value();

impl <const C: UniFloatChoice> Default for UniFloat<C> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    fn default() -> Self {
        Self::NAN
    }
}

impl <const C: UniFloatChoice> UniFloat<C> where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    /// Not-a-Number.
    pub const NAN: Self = Self {
        f32s: [f32::NAN; f32_parts_length(C)],
        #[cfg(not(feature = "f32_only"))]
        f64s: [f64::NAN; f64_parts_length(C)],
        #[cfg(not(feature = "f32_only"))]
        twofloats: [twofloat::TwoFloat::NAN; twofloat_parts_length(C)],

        #[cfg(not(feature = "f32_only"))]
        mpfr_limbs: unsafe { core::mem::MaybeUninit::uninit().assume_init() },

        #[cfg(not(feature = "f32_only"))]
        mpfr_fixeds: [mpfr::mpfr_t {
            prec: 1,
            sign: 1,
            exp: INITIAL_MPFR_EXP,
            d: DUMMY_MPFR_LIMB_PTR
        }; mpfr_fixed_parts_length(C)],

        #[cfg(debug_assertions)]
        unifloat_self: ptr::null(),
        #[cfg(debug_assertions)]
        used_as_operand_mutated: false
    };

    // Based on `gmp_mpfr_sys::MPFR_DECL_INIT`, but here we accept non-mutable
    // &self, because we use this in read-only asserts, too.
    #[cfg(not(feature = "f32_only"))]
    fn mpfr_limps_ptr(&self) -> ptr::NonNull<gmp::limb_t> {
        unsafe {
            ptr::NonNull::new_unchecked(self.mpfr_limbs[..].as_ptr() as *mut gmp::limb_t)
        }
    }

    /// Assert that an instance is "copy fixed". If it has been used through `OperandMututated`, then it must have been "cleared," too.
    #[inline]
    fn assert_copy_fixed(&self) {
        #[cfg(debug_assertions)] {
            assert!(self.unifloat_self == self,
                "Must call .copied() first, or assign with <<= instead of =. (unifloat_self hasn't been fixed.)");
            assert!(self.used_as_operand_mutated,
                 "Must call .copied() first, or assign with <<= instead of =. (used_as_mut_ref_operand hasn't been cleared.)" );
        }
        #[cfg(not(feature = "f32_only"))]
        assert!(
            if let UniFloatChoice::Mpfr { .. } = C {
                self.mpfr_fixeds[0].d == self.mpfr_limps_ptr()
            } else {
                true
            },
            "MPFR fields indicate that the instance was copied without having called .copied() afterwards, or it was assign to with = instead of <<=. However, unifloat_self guard didn't catch this. Please report this to UniFloat along with how to reproduce it in debug mode.");
    }

    #[inline]
    fn assert_copy_not_fixed(&self) {
        #[cfg(debug_assertions)]
        assert!(self.unifloat_self != self,
            "Have already called .copied(), or assigned with <<= instead of =. Do not call .copied() again.");
        #[cfg(not(feature = "f32_only"))]
        assert!(
            if let UniFloatChoice::Mpfr { .. } = C {
                self.mpfr_fixeds[0].d != self.mpfr_limps_ptr()
            } else {
                true
            },
            "MPFR fields indicate that the code had already called .copied(), or assigned with <<= instead of =. However, unifloat_self guard didn't catch this. Please report this to UniFloat along with how to reproduce it in debug mode.");
    }

    /// Call this exactly one after a UniFloat value is copied to:
    /// Assigned to, received as a 
    /// parameter, received from a result of a called function, copied from a
    /// struct/tuple/array. Also if was assigned from a result of an expression.
    /// However, do not call this if the value was assigned to with <<= 
    /// operator.
    #[inline]
    pub fn copied(&mut self) -> &mut Self {
        self.assert_copy_not_fixed();
        #[cfg(not(feature = "f32_only"))]
        if let UniFloatChoice::Mpfr { .. } = C {
            self.mpfr_fixeds[0].d = self.mpfr_limps_ptr();
        }
        #[cfg(debug_assertions)] {
            self.unifloat_self = self as *const _ as *const UniFloat<C>;
            self.used_as_operand_mutated = false;
        }
        self
    }

    #[inline]
    fn assert_used_as_operand_mutated(&self) {
        #[cfg(debug_assertions)]
        assert!(self.used_as_operand_mutated,
             "Must call .mutate() first. (used_as_mut_ref_operand hasn't been set.)" );
    }

    #[inline]
    pub fn mutate(&mut self) -> OperandMutated<C> {
        OperandMutated::new(self)
    }
}

impl <const C: UniFloatChoice> ops::ShlAssign for UniFloat<C> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    #[inline]
    fn shl_assign(&mut self, rhs: Self) {
        // DO NOT call rhs.assert_copy_fixed() here, because it's passed by value (rather than
        // by reference). So it should have been copy-fixed already.
        *self = rhs;
        self.copied();
    }
}

impl <const C: UniFloatChoice> ops::ShlAssign<&Self> for UniFloat<C> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    fn shl_assign(&mut self, rhs: &Self) {
        rhs.assert_copy_fixed();
        *self = *rhs;
        self.copied();
    }
}

