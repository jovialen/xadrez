pub(crate) mod hce;
#[cfg(feature = "nnue")]
mod nnue;

use crate::position::{Position, PositionBitboards};

#[cfg_attr(feature = "nnue", allow(unused_variables))]
pub(crate) fn evaluate_position(position: &Position, bitboards: &PositionBitboards) -> i32 {
    #[cfg(feature = "nnue")]
    return nnue::nnue_evaluation(position);

    #[cfg(not(feature = "nnue"))]
    return hce::hce_evaluation(position, bitboards);
}
