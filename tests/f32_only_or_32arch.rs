use {core::mem};
use unifloat::{UniFloatChoice, UniFloat};

#[cfg(target_pointer_width = "64")]
#[cfg(not(debug_assertions))] // release mode - optimized
#[cfg(feature = "f32_only")]
#[test]
fn unif32_uses_32_bits_with_f32_only_feature_even_on_64arch() {
    if mem::size_of::<UniFloat<{UniFloatChoice::F32}>>()
    != mem::size_of::<f32>() {
        panic!();
    }
}

#[cfg(target_pointer_width = "32")]
#[cfg(not(debug_assertions))] // release mode - optimized
#[cfg(not(feature = "f32_only"))]
#[test]
fn unif32_uses_32_bits_on_32arch_even_without_f32_only_feature() {
    if mem::size_of::<UniFloat<{UniFloatChoice::F32}>>()
    != mem::size_of::<f32>() {
        panic!();
    }
}