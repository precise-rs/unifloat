#![feature(const_generics)]
use {core::mem};
use unifloat::{UniFloatChoice, UniFloat, MpfrLimbPart};
use gmp_mpfr_sys::mpfr;

#[cfg(target_pointer_width = "32")]
fn unif32_aligns_same_as_unif64_on_32arch() {
    if mem::align_of::<UniFloat<{UniFloatChoice::F32}>>()
        != mem::align_of::<UniFloat<{UniFloatChoice::F64}>>() {
            panic!();
        }
}   

pub fn genericy<const C: UniFloatChoice>(uni_ref: &UniFloat::<{ C }>)
where
[f32; unifloat::f32_parts_length(C)]: Sized,
[f64; unifloat::f64_parts_length(C)]: Sized,
[twofloat::TwoFloat; unifloat::twofloat_parts_length(C)]: Sized,
[mpfr::mpfr_t; unifloat::mpfr_fixed_parts_length(C)]: Sized,
[MpfrLimbPart; unifloat::mpfr_limb_parts_length(C)]: Sized,

{

}