use {core::mem, gmp_mpfr_sys::{mpfr, gmp}};
use crate::{MpfrBounds, ONE_LIMB_PRECISION, UniFloat, UniFloatChoice, UniF32,
    UniF64, UniTwoFloat, UniMpfrLimb1Prec1, UniMpfrLimb2PrecAll};

// Types with names like UniMpfrLimbxPrecMost use almost the whole precision
// available for their number of limbs (so they can't fit into a smaller
// number of limbs), but they don't use the whole available precision.
type UniMpfrLimb1PrecMost = UniFloat<{ UniFloatChoice::Mpfr { bounds: MpfrBounds {
    limb_parts_length: 1,
    precision_bits_length: 1 * ONE_LIMB_PRECISION,
}}}>;
type UniMpfrLimb1PrecAll = UniFloat<{ UniFloatChoice::Mpfr { bounds: MpfrBounds {
    limb_parts_length: 1,
    precision_bits_length: 1 * ONE_LIMB_PRECISION,
}}}>; //@TODO test most_precise_for_same_space
// This helps when calculating size and alignment of `UniFloat.unifloat_self`
// pointer. That
// field exists in debug mode only. When on a 64+ bit platform,
// `unifloat_self` may increase alignment (and hence the size) of F32. That's
// OK, since it does not affect release.)
// (We don't support 16 bit platform, but 32+ bit only.)
const POINTER_ALIGN: usize = mem::align_of::<* const usize>();
const POINTER_SIZE: usize = mem::size_of::<* const usize>();
const PRIMITIVE_F32_SIZE: usize = mem::size_of::<f32>();
const PRIMITIVE_F64_SIZE: usize = mem::size_of::<f64>();
const PRIMITIVE_F64_ALIGN: usize = mem::align_of::<f64>();

/// Use assertions, so the checks are run only in debug mode (where `UniFloat`
/// has field `unifloat_self` - that's why we add `POINTER_SIZE`).
/// All calculations are for C representation (`#[repr(C)]`) of UniFloat
/// with auto-generated alignments (not with `packed` nor `align` in
/// `repr` attribute).
#[test]
#[cfg(debug_assertions)]
fn debug_type_sizes() {
    // Following are not real tests, but my clarification
    assert!(   PRIMITIVE_F32_SIZE <= POINTER_SIZE ); // Support 32+ bit only.
    assert_eq!(mem::align_of::<UniF32>(), POINTER_ALIGN);
    assert_eq!(mem:: size_of::<UniF32>(), 2 * POINTER_SIZE);
    
    let uni_f64_align = PRIMITIVE_F64_ALIGN.max(POINTER_ALIGN);
    assert_eq!(mem::align_of::<UniF64>(),     uni_f64_align);
    // Following should work on a 128+ bit platform, too.
    assert_eq!(mem::size_of ::<UniF64>(),
        PRIMITIVE_F64_SIZE.max(uni_f64_align) + POINTER_SIZE.max(uni_f64_align));

    let uni_twofloat_align = mem::align_of::<twofloat::TwoFloat>().max(POINTER_ALIGN);
    assert_eq!(mem::size_of::<UniTwoFloat>(),
        mem::size_of::<twofloat::TwoFloat>().max(uni_twofloat_align)
            + POINTER_SIZE.max(uni_twofloat_align));
}

/// For non-debug mode (where UniFloat doesn't have field unifloat_self).
#[test]
fn non_debug_type_sizes() {
    if true {
        #[cfg(debug_assertions)]
        return
    }
    let mut non_debug_run = true;
    assert!( {
        non_debug_run = false;
        true
    });
    if non_debug_run {
        if mem::size_of::<UniF32>() != PRIMITIVE_F32_SIZE { panic!(); }
        if mem::size_of::<UniF64>() != PRIMITIVE_F64_SIZE { panic!(); }
        if mem::size_of::<UniTwoFloat>() != mem::size_of::<twofloat::TwoFloat>() { panic!(); }
        if mem::size_of::<UniMpfrLimb1Prec1>() != /*fixed part:*/ mem::size_of::<mpfr::mpfr_t>()
            + /* limb part: */ mem::size_of::<mem::MaybeUninit<gmp::limb_t>>() { panic!(); }
        if mem::size_of::<UniMpfrLimb1PrecMost>() != mem::size_of::<UniMpfrLimb1Prec1>() {
            panic!();
        }
        if mem::size_of::<UniMpfrLimb1PrecAll>() != mem::size_of::<UniMpfrLimb1Prec1>() {
            panic!();
        }
        if mem::size_of::<UniMpfrLimb2PrecAll>() != mem::size_of::<mpfr::mpfr_t>()
            + 2* mem::size_of::<mem::MaybeUninit<gmp::limb_t>>() { panic!(); }
        
    }
}
