use gmp_mpfr_sys::mpfr;

use crate::{
    f32_parts_length, f64_parts_length, mpfr_fixed_parts_length, mpfr_limb_parts_length,
    twofloat_parts_length, MpfrBounds, MpfrLimbPart, UniFloat, UniFloatChoice,
};
use core::ops;

/// Not a part of public API. Used only for intermediate results.
#[derive(Clone, Copy, Debug)]
pub struct OperandOwned<const C: UniFloatChoice>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    float: UniFloat<C>,
}

impl<'a, const C: UniFloatChoice> OperandOwned<C>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    pub(crate) fn new(float: &UniFloat<C>) -> Self {
        let mut result = Self { float: *float };
        result.float.sanitize();
        result
    }
}

/// Used only for passing variables whose values are not needed anymore, and can be replaced with a result of a (potentially intermediate) operation. NOT for left sides of modify-and-assign operators (+=, -=...) - those operate on (mutable) UniFloats directly.
/// Why don't we just use &ref UniFloat<C>? Because OperandMutated ensures that UniFloat.used_as_operand_mutated is set. That makes the user call .sanitize() or <<= on that UniFloat before reading from it again. That prevents accidental incorrect read access to that UniFloat.
#[derive(Debug)]
pub struct OperandMutated<'a, const C: UniFloatChoice>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    float: &'a mut UniFloat<C>,
}

impl<'a, const C: UniFloatChoice> OperandMutated<'a, C>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    pub(crate) fn new(float: &'a mut UniFloat<C>) -> Self {
        float.assert_copy_fixed();
        float.used_as_operand_mutated = true;
        Self { float }
    }
}

impl<'a, const C: UniFloatChoice> ops::Add<&UniFloat<C>> for OperandMutated<'a, C>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    type Output = Self;
    fn add(self, right: &UniFloat<C>) -> Self::Output {
        //TODO
        self
    }
}

// TODO use "existential" impl UniFloatable<C>, so that this works with
// right hand: &UniFloat, OperandMutated, OperandOwned.
impl<const C: UniFloatChoice> ops::Add<&UniFloat<C>> for &UniFloat<C>
where
    [f32; f32_parts_length(C)]: Sized,
    [f64; f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    type Output = OperandOwned<C>;
    fn add(self, right: &UniFloat<C>) -> Self::Output {
        //TODO
        panic!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        MpfrBounds, UniFloat, UniFloatBounds, UniFloatBoundsBase, UniFloatBoundsToChoice,
        UniFloatChoice,
    };

    const MPFR_100_BITS: UniFloatChoice = UniFloatChoice::Mpfr {
        bounds: MpfrBounds::for_precision_binary(100),
    }
    .most_precise_for_same_space();
    type UniMpfr100bit = UniFloat<{ MPFR_100_BITS }>;

    #[test]
    fn has_to_clear_operand_mutated_before_read() {
        let bounds = UniFloatBounds::<{ UniFloatBoundsBase::BINARY }>::new(100, -30, 30);
        let choice = bounds.to_choice(); //.most_precise_for_same_space();
        assert!(MPFR_100_BITS.covers(&choice));
        let nan_immutable = UniMpfr100bit::NAN;

        let mut workplace = nan_immutable;
        // protected:
        workplace.mutate();
        // TODO now use read-only e.g. workplace + NAN
        // assert that it failed
    }
}
