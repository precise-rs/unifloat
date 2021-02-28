use gmp_mpfr_sys::mpfr;
use crate::{UniFloatChoice, UniFloat, MpfrLimbPart, f32_parts_length, f64_parts_length, twofloat_parts_length, mpfr_fixed_parts_length, mpfr_limb_parts_length};

// deprecated if it involves copy in release.
//#[derive(Clone, Copy, Debug)]
//pub struct Operand {}

// could be called OperandReadOnly
#[derive(Clone, Copy, Debug)]
pub struct OperandRef<'a, const C: UniFloatChoice> where
[f32; f32_parts_length(C)]: Sized,
[f64; f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; mpfr_limb_parts_length(C)]: Sized,
{
    float: &'a UniFloat<C>
}

/// Used for passing variables whose values are not needed anymore, and can be replaced with a result of a (potentially intermediate) operation. NOT for left sides of modify-and-assign operators (+=, -=...) - those operate on UniFloats directly.
// could be called OperandReplace
#[derive(Clone, Copy, Debug)]
pub struct OperandMutRef<const C: UniFloatChoice> {}

// TODO benchmark first. If it does involve extra copy, discourage.
//#[derive(Clone, Copy, Debug)]
//pub struct OperandMut {}