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
pub mod fen;
pub mod r#move;
mod movegen;
pub mod piece;

pub mod prelude {
    pub use crate::{
        board::{Chessboard, Square},
        piece::{Piece, PieceKind, Side},
        r#move::Move,
    };
}
