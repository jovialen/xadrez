//! A chess engine.

#![warn(missing_docs)]
// Enable all clippy checks
#![warn(clippy::suspicious)]
#![warn(clippy::complexity)]
#![warn(clippy::perf)]
#![warn(clippy::style)]
#![warn(clippy::pedantic)]

mod bitboards;
pub mod board;
pub mod builder;
pub mod error;
pub mod evaluation;
pub mod fen;
#[allow(unused)]
mod gen_move_tables;
pub mod r#move;
mod movegen;
pub mod piece;
mod position;
pub mod search;
pub mod square;

/// Quick import for all of the most commonly used data types.
pub mod prelude {
    pub use crate::board::{Chessboard, DrawReason, GameState};
    pub use crate::piece::{Piece, PieceKind, Side};
    pub use crate::r#move::Move;
    pub use crate::square::Square;
}
