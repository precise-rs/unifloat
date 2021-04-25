use crate::{UniFloat, UniFloatBounds, UniFloatBoundsBase, UniFloatChoice};

extern crate std;

// Of course, the user can provide the bounds as decimal.
#[test]
fn support_binary_based_floats_only() {
    assert_eq!(f32::RADIX, 2);
    assert_eq!(f64::RADIX, 2);
}

#[test]
fn narrow_to_decimal() {
    std::println!("f64 MAX: {}", f64::MAX);
    let bin_prec_4_min_neg4_max4 = UniFloatBounds::<{ UniFloatBoundsBase::BINARY }> {
        precision: 4,
        min_exponent: -4,
        max_exponent: 4,
    };
    let dec_prec_1_min_neg1_max1 = UniFloatBounds::<{ UniFloatBoundsBase::DECIMAL }> {
        precision: 1,
        min_exponent: -1,
        max_exponent: 1,
    };
    //assert_eq!(bin_prec_4_min_neg4_max4.narrow_to_decimal(), dec_prec_1_min_neg1_max1);
}

#[test]
fn narrow_to_binary() {}
#[test]
fn widen_to_binary() {
    let prec_dec = 1; // 0..10
    let prec_bin = 4; // 0..16
}
