//! Evaluation of the board.

pub(crate) mod hce;
#[cfg(feature = "nnue")]
mod nnue;
pub mod score;

use crate::board::GameState;
use crate::error::ParseFenError;
use crate::position::Position;
use score::{PositionPrediction, Score};

/// Evaluate a fen position.
///
/// # Arguments
///
/// * `fen` - A valid fen string to evaluate.
///
/// # Errors
///
/// Will return a ParseFenError if the given FEN string is invalid.
pub fn evaluate(fen: &str) -> Result<Score, ParseFenError> {
    let position = Position::from_fen(fen)?;
    Ok(evaluate_position(&position))
}

#[cfg_attr(feature = "nnue", allow(unused_variables))]
pub(crate) fn evaluate_position(position: &Position) -> Score {
    #[cfg(feature = "nnue")]
    let score = nnue::nnue_evaluation(position);

    #[cfg(not(feature = "nnue"))]
    let score = hce::hce_evaluation(position);

    let prediction = match position.game_state() {
        GameState::Playing => None,
        GameState::Draw(_) => Some(PositionPrediction::Draw),
        GameState::Checkmate => Some(PositionPrediction::Checkmate),
    };

    Score {
        relative_to: position.data.side_to_move,
        score,
        prediction,
    }
}
