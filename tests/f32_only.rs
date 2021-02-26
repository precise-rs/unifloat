use {core::mem};
use unifloat::{UniFloatChoice, UniFloat};

#[cfg(target_pointer_width = "64")]
#[cfg(not(debug_assertions))] // release mode - optimized
#[cfg(feature = "f32_only")]
#[test]
fn unif32_aligns_same_as_f32_even_on_64arch() {
    if mem::align_of::<UniFloat<{UniFloatChoice::F32}>>()
    != mem::align_of::<f32>() {
        panic!();
    }
}