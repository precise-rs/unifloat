#![cfg(test)]

mod unifloat_bounds;

use crate::{
    MpfrBounds, UniF32, UniF64, UniFloat, UniFloatChoice, UniMpfrLimb1Prec1, UniMpfrLimb2PrecAll,
    UniTwoFloat, ONE_LIMB_PRECISION,
};

#[test]
fn call_sanitize_exactly_where_needed() {
    //let uf = UniFloat::<{ UniFloatChoice::F32 }>::default();
}

#[test]
fn no_need_to_call_sanitize_before_receiving_by_value() {
    //let uf = UniFloat::<{ UniFloatChoice::F32 }>::default();
    let (unfixed_a, mut unfixed_b) = (UniF32::NAN, UniF32::NAN);
    //let mut unfixed_b = UniF32::NAN;
    let (mut fixed_a, mut fixed_b) = (UniF32::NAN, UniF32::NAN);
    fixed_a.sanitize();
    fixed_b.sanitize();

    // Each group of step(s) has two alternatives. First alternative uses unfixed: NOT treated
    // with .copy(). Second: The source uses fixed: treated with .copy().
    let mut copy = UniF32::NAN;
    copy <<= unfixed_a;
    copy <<= fixed_a;

    fn receive_by_value(received: UniF32) {
        let mut copy = UniF32::NAN;
        copy <<= received;
    }
    receive_by_value(unfixed_a);
    receive_by_value(fixed_a);

    fn return_by_value_unfixed() -> UniF32 {
        let orig = UniF32::NAN;
        orig
    }
    copy <<= return_by_value_unfixed();
    fn return_by_value_fixed() -> UniF32 {
        let mut orig = UniF32::NAN;
        *orig.sanitize() // No effect, since the return is by reference - which would require .sanitize() again.
    }
    copy <<= return_by_value_fixed();
}

#[test]
#[should_panic(expected = "Must call .sanitize() first, or assign with <<= instead of =.")]
fn must_call_sanitize_before_receiving_by_reference_debug_mode() {
    //let uf = UniFloat::<{ UniFloatChoice::F32 }>::default();
    let orig = UniF32::NAN;
    let mut copy = UniF32::NAN;
    copy <<= &orig;
    if !cfg!(debug_assertions) {
        panic!("Must call .sanitize() first, or assign with <<= instead of =.");
    }
}

#[test]
#[should_panic(
    expected = "Have already called .sanitize(), or assigned with <<= instead of =. Do not call .sanitize() now."
)]
#[ignore]
fn must_not_call_sanitize_twice() {
    let mut orig = UniF32::NAN;
    orig.sanitize();
    orig.sanitize();
    if !cfg!(debug_assertions) {
        panic!("Have already called .sanitize(), or assigned with <<= instead of =. Do not call .sanitize() now.");
    }
}
