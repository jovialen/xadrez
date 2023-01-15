//! Chess piece structures.
//!
//! Provides the structures used relating to the pieces on the chessboard.

use crate::{board::Direction, error::ParseFenError};
use std::{fmt, ops};

/// Constants for all the types of chess pieces.
pub mod constants {
    #[allow(clippy::enum_glob_use)]
    use super::{Piece, PieceKind::*, Side::*};

    /// The white king.
    pub const WHITE_KING: Piece = Piece {
        kind: King,
        side: White,
    };
    /// The white queen.
    pub const WHITE_QUEEN: Piece = Piece {
        kind: Queen,
        side: White,
    };
    /// The white bishop.
    pub const WHITE_BISHOP: Piece = Piece {
        kind: Bishop,
        side: White,
    };
    /// The white knight.
    pub const WHITE_KNIGHT: Piece = Piece {
        kind: Knight,
        side: White,
    };
    /// The white rook.
    pub const WHITE_ROOK: Piece = Piece {
        kind: Rook,
        side: White,
    };
    /// The white pawn.
    pub const WHITE_PAWN: Piece = Piece {
        kind: Pawn,
        side: White,
    };

    /// The black king.
    pub const BLACK_KING: Piece = Piece {
        kind: King,
        side: Black,
    };
    /// The black queen.
    pub const BLACK_QUEEN: Piece = Piece {
        kind: Queen,
        side: Black,
    };
    /// The black bishop.
    pub const BLACK_BISHOP: Piece = Piece {
        kind: Bishop,
        side: Black,
    };
    /// The dark knight.
    pub const BLACK_KNIGHT: Piece = Piece {
        kind: Knight,
        side: Black,
    };
    /// The black rook.
    pub const BLACK_ROOK: Piece = Piece {
        kind: Rook,
        side: Black,
    };
    /// The black pawn.
    pub const BLACK_PAWN: Piece = Piece {
        kind: Pawn,
        side: Black,
    };
}

/// Struct representing a single chess piece.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Piece {
    /// The type of the chess piece
    pub kind: PieceKind,
    /// The alignment of the chess piece.
    pub side: Side,
}

/// Enum for the possible alignments of a chess piece.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    /// White.
    ///
    /// The side that is first to move.
    White,
    /// Black.
    ///
    /// The side that is second to move.
    Black,
}

pub(crate) const SIDE_COUNT: usize = 2;

/// Enum for the possible types of a chess piece.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PieceKind {
    /// The king piece.
    ///
    /// Can only move one square in any direction.
    King,
    /// The queen piece.
    ///
    /// Can move freely on both the diagonal and straight axis.
    Queen,
    /// The bishop piece.
    ///
    /// Can move freely on the diagonal axis.
    Bishop,
    /// The knight piece.
    ///
    /// Can leap once in an L shape over any pieces between it and its
    /// destination square.
    Knight,
    /// The rook piece.
    ///
    /// Can move freely on the straight axis.
    Rook,
    /// The pawn piece.
    ///
    /// Can move once forwarsd, except on the first move when it can move two
    /// forwards. Captures on the diagonal axis.
    Pawn,
}

pub(crate) const PIECE_KIND_COUNT: usize = 6;

impl Piece {
    /// Create a new chess piece.
    ///
    /// # Arguments
    ///
    /// * `side` - The side the chess piece is on.
    /// * `kind` - The kind of the piece.
    #[must_use]
    pub fn new(side: Side, kind: PieceKind) -> Self {
        Piece { kind, side }
    }
}

impl TryFrom<char> for Piece {
    type Error = ParseFenError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(Self {
            kind: PieceKind::try_from(value)?,
            side: Side::from(value),
        })
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = self.kind.to_string();

        let c = if self.side == Side::White {
            c.to_uppercase()
        } else {
            c.to_lowercase()
        };

        write!(f, "{c}")
    }
}

impl Side {
    pub(crate) fn backward(self) -> Direction {
        match self {
            Self::White => Direction::South,
            Self::Black => Direction::North,
        }
    }
}

impl ops::Not for Side {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

impl From<char> for Side {
    fn from(value: char) -> Self {
        match value {
            'w' => Side::White,
            c if c.is_uppercase() => Side::White,
            _ => Side::Black,
        }
    }
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Side::White => 'w',
                Side::Black => 'b',
            }
        )
    }
}

impl TryFrom<char> for PieceKind {
    type Error = ParseFenError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'K' | 'k' => Ok(PieceKind::King),
            'Q' | 'q' => Ok(PieceKind::Queen),
            'B' | 'b' => Ok(PieceKind::Bishop),
            'N' | 'n' => Ok(PieceKind::Knight),
            'R' | 'r' => Ok(PieceKind::Rook),
            'P' | 'p' => Ok(PieceKind::Pawn),
            _ => Err(ParseFenError::InvalidPiece(value)),
        }
    }
}

impl fmt::Display for PieceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PieceKind::King => "k",
                PieceKind::Queen => "q",
                PieceKind::Bishop => "b",
                PieceKind::Knight => "n",
                PieceKind::Rook => "r",
                PieceKind::Pawn => "p",
            }
        )
    }
}
