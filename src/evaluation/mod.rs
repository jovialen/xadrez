//! Evaluation of the board.

pub(crate) mod hce;
#[cfg(feature = "nnue")]
mod nnue;
pub mod score;

use crate::board::GameState;
use crate::position::{Position, PositionBitboards};
use score::{PositionPrediction, Score};

#[cfg_attr(feature = "nnue", allow(unused_variables))]
pub(crate) fn evaluate_position(
    state: GameState,
    position: &Position,
    bitboards: &PositionBitboards,
) -> Score {
    #[cfg(feature = "nnue")]
    let score = nnue::nnue_evaluation(position);

    #[cfg(not(feature = "nnue"))]
    let score = hce::hce_evaluation(position, bitboards);

    let prediction = match state {
        GameState::Playing => None,
        GameState::Draw(_) => Some(PositionPrediction::Draw),
        GameState::Checkmate => Some(PositionPrediction::Checkmate),
    };

    Score {
        relative_to: position.side_to_move,
        score,
        prediction,
    }
}
