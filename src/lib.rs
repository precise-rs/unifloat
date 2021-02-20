#![allow(incomplete_features)]
#![feature(const_generics, const_evaluatable_checked, const_panic)]
use {core::ops, gmp_mpfr_sys::mpfr};

/// Across this crate: Const generic parameter S is NOT necessarily a number of
/// 64bit extras, but a number of any and all 64-bit
/// parts that contain the significand of the floating-point number.
/// For S<=2 this is less than the significand length, because then Float is
/// implemented by f32/f64/TwoFloat, whose length also includes the sign and
/// the exponent!
/// For S=0 Float is represented by f32.
/// TODO: If you'd like to use MPFR (rather than default f32/f64/TwoFloat) for
/// S<=2, use negative values instead: S=-1 or S=-2. You may want that if the
/// exponent is outside standard f32/f64 exponent range!

/// Difference to MPFR: Default value of a new MPFR float is NaN.
/// However, we follow Rust's float default, which is 0.

/// `const fun` functions here whose names end with _parts_length(s: isize) -> usize
/// return the number of entries/slots of the respective type (f32, f64...) to
/// be used by the respective parts. (Not a number of bytes.)
/// Types and const genereric bounds are based on
/// https://github.com/rust-lang/rust/issues/68436. However, not all
//// tips from that discussion work with 1.52.0-nightly.

/// Public, otherwise we were getting "private type `fn(isize) -> usize {f32_parts_length}` in public interface (error E0446)"
pub const fn f32_parts_length(s: isize) -> usize {
    if s == 0 {
        1
    } else {
        0
    }
}
type F32Parts<const S: isize> = [f32; f32_parts_length(S)];

pub const fn f64_parts_length(s: isize) -> usize {
    if s == 1 {
        1
    } else {
        0
    }
}
type F64Parts<const S: isize> = [f64; f64_parts_length(S)];

pub const fn two_float_parts_length(s: isize) -> usize {
    if s == 2 {
        1
    } else {
        0
    }
}
type TwoFloatParts<const S: isize> = [twofloat::TwoFloat; two_float_parts_length(S)];

pub const fn mpfr_significand_64b_parts_length(s: isize) -> usize {
    if s > 2 {
        s as usize
    } else if s <0 {
        -s as usize
    } else {
        0
    }
}
type MpfrSignificand64bParts<const S: isize> = [u64; mpfr_significand_64b_parts_length(S)];

pub const fn mpfr_header_parts_length(s: isize) -> usize {
    if s < 0 || s > 2 {
        1
    } else {
        2
    }
}
// TODO
type MpfrHeaderParts<const S: isize> = [u128;/*mpfr::mpfr_t;*/ mpfr_header_parts_length(S)];

/// Always return 0. This asserts that given s is acceptable.
pub const fn assert_any_significand_part_length_is_acceptable(s: isize) -> usize {
    assert!(s >= -2);
    0
}
pub struct ConstAssert<const ASSERT: usize>;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Float<const S: isize> where
[f32; f32_parts_length(S)]: Sized,
[f64; f64_parts_length(S)]: Sized,
[twofloat::TwoFloat; two_float_parts_length(S)]: Sized,
[u64; mpfr_significand_64b_parts_length(S)]: Sized,
[u128/*mpfr::mpfr_t*/; mpfr_header_parts_length(S)]: Sized,
ConstAssert<{assert_any_significand_part_length_is_acceptable(S)}>:,
[u8; assert_any_significand_part_length_is_acceptable(S)]: Sized
//[u64; (S+2) as usize]:
 {
    f32s: F32Parts<S>,
    f64s: F64Parts<S>,
    two_floats: TwoFloatParts<S>,
    mpfr_significands: MpfrSignificand64bParts<S>,
    mpfr_headers: MpfrHeaderParts<S>
}

impl <const S: isize> Default for Float<S> where
[f32; f32_parts_length(S)]: Sized,
[f64; f64_parts_length(S)]: Sized,
[twofloat::TwoFloat; two_float_parts_length(S)]: Sized,
[u64; mpfr_significand_64b_parts_length(S)]: Sized,
[u128; mpfr_header_parts_length(S)]: Sized,
[u8; assert_any_significand_part_length_is_acceptable(S)]: Sized
{
    fn default() -> Self {
        Self {
            f32s: [0.0; f32_parts_length(S)],
            f64s: [0.0; f64_parts_length(S)],
            two_floats: [twofloat::TwoFloat::from(0.0); two_float_parts_length(S)],
            mpfr_significands: [0; mpfr_significand_64b_parts_length(S)], //TODO
            mpfr_headers: [0; mpfr_header_parts_length(S)] // TODO
        }
    }
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
    use crate::{Float};
    #[test]
    fn size() {
        let f0 = <Float<0>>::default();
        let f1: Float<1> = Float::default();
        println!("Size of Float<0>: {}", mem::size_of_val(&f0));
        println!("Size of Float<1>: {}", mem::size_of_val(&f1))
    }
}
