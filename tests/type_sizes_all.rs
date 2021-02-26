use {core::mem};
use unifloat::{UniFloatChoice, UniFloat};

#[cfg(target_pointer_width = "32")]
fn unif32_aligns_same_as_unif64_on_32arch() {
    if mem::align_of::<UniFloat<{UniFloatChoice::F32}>>()
        != mem::align_of::<UniFloat<{UniFloatChoice::F64}>>() {
            panic!();
        }
}