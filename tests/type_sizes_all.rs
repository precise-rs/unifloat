#![feature(const_generics, const_evaluatable_checked)]
use core::mem;
use gmp_mpfr_sys::mpfr;
use unifloat::{MpfrLimbPart, UniFloat, UniFloatChoice};

#[cfg(target_pointer_width = "32")]
fn unif32_aligns_same_as_unif64_on_32arch() {
    if mem::align_of::<UniFloat<{ UniFloatChoice::F32 }>>()
        != mem::align_of::<UniFloat<{ UniFloatChoice::F64 }>>()
    {
        panic!();
    }
}

struct Structy<const C: UniFloatChoice>
where
    [f32; unifloat::f32_parts_length(C)]: Sized,
    [f64; unifloat::f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; unifloat::twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; unifloat::mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; unifloat::mpfr_limb_parts_length(C)]: Sized,
{
    value: UniFloat<{ C }>,
}

impl<const C: UniFloatChoice> Structy<C>
where
    [f32; unifloat::f32_parts_length(C)]: Sized,
    [f64; unifloat::f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; unifloat::twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; unifloat::mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; unifloat::mpfr_limb_parts_length(C)]: Sized,
{
    pub fn genericy<const OTHER: UniFloatChoice>(&self, uni_ref: &UniFloat<{ OTHER }>)
    where
        [f32; unifloat::f32_parts_length(OTHER)]: Sized,
        [f64; unifloat::f64_parts_length(OTHER)]: Sized,
        [twofloat::TwoFloat; unifloat::twofloat_parts_length(OTHER)]: Sized,
        [mpfr::mpfr_t; unifloat::mpfr_fixed_parts_length(OTHER)]: Sized,
        [MpfrLimbPart; unifloat::mpfr_limb_parts_length(OTHER)]: Sized,
    {
    }
}

pub fn standalone_genericy<const C: UniFloatChoice>(uni_ref: &UniFloat<{ C }>)
where
    [f32; unifloat::f32_parts_length(C)]: Sized,
    [f64; unifloat::f64_parts_length(C)]: Sized,
    [twofloat::TwoFloat; unifloat::twofloat_parts_length(C)]: Sized,
    [mpfr::mpfr_t; unifloat::mpfr_fixed_parts_length(C)]: Sized,
    [MpfrLimbPart; unifloat::mpfr_limb_parts_length(C)]: Sized,
{
}
