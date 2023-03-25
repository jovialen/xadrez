//! # Xadrez
//!
//! Xadrez is a Rust library for working with chess games. It provides
//! functionality for parsing FEN strings, generating legal moves, searching for
//! the best possible moves, and evaluating the position of the board.
//!
//! # Features
//!
//! * Parsing FEN strings into chess positions
//! * Generating legal moves for a given chess position
//! * Searching for the best possible move using different algorithms
//! * Evaluating the position of the board using different metrics
//! * Supports all standard chess rules, including en passant, castling, and
//!   pawn promotion
//! * Written entirely in Rust, providing a safe and efficient implementation
//! * Well-documented API and easy-to-use interface
//!
//! # Getting Started
//!
//! To use Xadrez in your Rust project, simply add the following to your
//! Cargo.toml file:
//!
//! ```
//! [dependencies]
//! xadrez = "0.1.0"
//! ```
//!
//! Then, import the library in your code using:
//!
//! ```
//! use xadrez::prelude::*;
//! ```
//!
//! Example Usage
//!
//! Here's an example of using Xadrez to generate all legal moves for a given
//! position:
//!
//! ```
//! use xadrez::prelude::*;
//!
//! let chessboard = Chessboard::default();
//! let moves = position.moves();
//!
//! for mv in moves {
//!     println!("{}", mv);
//! }
//! ```
//!
//! This will output the following legal moves:
//!
//! ```
//! a2a3
//! a2a4
//! b2b3
//! b2b4
//! c2c3
//! c2c4
//! d2d3
//! d2d4
//! e2e3
//! e2e4
//! f2f3
//! f2f4
//! g2g3
//! g2g4
//! h2h3
//! h2h4
//! b1a3
//! b1c3
//! f1g3
//! f1h3
//! ```
//!
//! # License
//!
//! Xadrez is licensed under the Apache-v2.0 License.

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

/// The Xadrez prelude module provides a convenient way to bring common Xadrez
/// types and traits into scope. It re-exports the most commonly used items from
/// the library modules.
///
/// Usage:
///
/// ```
/// use xadrez::prelude::*;
/// ```
///
/// This will bring the most commonly used types and traits into scope. Note
/// that not all types and traits are exported by this module, so some items may
/// need to be brought into scope manually.
///
/// For more information on the available types and traits, see the
/// corresponding modules.
pub mod prelude {
    pub use crate::board::{Chessboard, DrawReason, GameState};
    pub use crate::builder::BoardBuilder;
    pub use crate::piece::{Piece, PieceKind, Side};
    pub use crate::r#move::Move;
    pub use crate::square::Square;
}
