//! A chess engine.

#![warn(missing_docs)]

mod bitboards;
pub mod board;
pub mod error;
pub mod fen;
pub mod r#move;
mod movegen;
pub mod piece;
