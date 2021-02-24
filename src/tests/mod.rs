#![cfg(test)]

mod type_sizes;

use crate::{MpfrBounds, ONE_LIMB_PRECISION, UniFloat, UniFloatChoice, UniF32,
    UniF64, UniTwoFloat, UniMpfrLimb1Prec1, UniMpfrLimb2PrecAll};


#[test]
fn call_copied() {
    //let uf = UniFloat::<{ UniFloatChoice::F32 }>::default();
    let mut orig = UniF32::nan();
    orig.copied();

    let mut copy = UniF32::nan();
    copy <<= orig;

    let mut auto_copied = UniF32::nan();
    copy <<= copy;
}

#[test]
// @TODO mark this test as supposed to fail
fn must_call_copied() {
    //let uf = UniFloat::<{ UniFloatChoice::F32 }>::default();
    let orig = UniF32::nan();
    let mut copy = UniF32::nan();
    copy <<= orig;
}

#[test]
// @TODO mark this test as supposed to fail
fn must_not_call_copied() {
    let mut orig = UniF32::nan();
    orig.copied();
    orig.copied();
}