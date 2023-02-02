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
pub mod error;
mod evaluation;
pub mod fen;
pub mod r#move;
mod movegen;
pub mod piece;
pub mod search;

/// Quick import for all of the most commonly used data types.
pub mod prelude {
    pub use crate::board::{Chessboard, Square};
    pub use crate::piece::{Piece, PieceKind, Side};
    pub use crate::r#move::Move;
}
