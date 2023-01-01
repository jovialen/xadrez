//! Chessboard structures.
//!
//! Provides the structures relating to the chessboard as a whole.

use crate::piece::{Piece, Side, SIDE_COUNT};

/// Count of files on the chessboard.
///
/// The files are the vertical columns on the chessboard, marked by the letters
/// from A to H.
pub const BOARD_FILES: usize = 8;
/// Count of ranks on the chessboard.
///
/// The ranks are the horizontal columns on the chessboard, marked by the
/// numbers from 1 to 8.
pub const BOARD_RANKS: usize = 8;
/// Count of squares on the chessboard in total.
pub const BOARD_SQUARES: usize = BOARD_FILES * BOARD_RANKS;

/// Structure representing a chessboard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Chessboard {
    position: Position,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Position {
    squares: [Option<Piece>; BOARD_SQUARES],
    side_to_move: Side,
    castling: [[bool; 2]; SIDE_COUNT],
    en_passant: Option<Square>,
    halftime: u32,
    fulltime: u32,
}

/// The squares on the chessboard.
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl Default for Chessboard {
    fn default() -> Self {
        Self {
            position: Position::new(),
        }
    }
}

impl Position {
    fn new() -> Self {
        Self {
            squares: [None; BOARD_SQUARES],
            side_to_move: Side::White,
            castling: [[false; 2]; SIDE_COUNT],
            en_passant: None,
            halftime: 0,
            fulltime: 0,
        }
    }
}
