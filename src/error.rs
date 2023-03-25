//! Error types of the chess engine.
//!
//! Provides the error types used by the chess engine.

#![allow(clippy::module_name_repetitions)]

#[cfg(doc)]
use crate::board::Chessboard;
use std::error::Error;
use std::fmt;
use std::num::ParseIntError;

/// An error which can be returned while parsing a FEN string.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseFenError {
    /// One or more fields are missing from the FEN string.
    MissingField,
    /// There are one or more extra fields on the FEN string.
    TrailingField,
    /// The FEN board representation is either too big or too small.
    InvalidBoardSize,
    /// The board is missing at least one king.
    MissingKing,
    /// Either the rank or file of the square was out of bounds.
    InvalidSquare,
    /// No piece represented by the given char.
    InvalidPiece,
    /// Castling rights given when either the king or the rook aren't correctly
    /// positioned.
    IllegalCastling,
    /// Failed to parse either the fulltime or halftime clock.
    ParseClockError(ParseIntError),
    /// The input was empty.
    Empty,
}

/// An error which can be returned while parsing long algebraic notation.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseLANError {
    /// Either the to or from square was invalid.
    InvalidSquare,
    /// The promotion of the piece was invalid.
    InvalidPromotion,
    /// The input was empty.
    Empty,
}

/// An error which can be returned when attempting to make a move on a
/// [`Chessboard`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MoveError {
    /// The move was not legal.
    IllegalMove,
}

impl From<ParseIntError> for ParseFenError {
    fn from(err: ParseIntError) -> Self {
        ParseFenError::ParseClockError(err)
    }
}

impl Error for ParseFenError {}

impl fmt::Display for ParseFenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::MissingField => "one or more fields are missing from the FEN string",
            Self::TrailingField => "there are one or more trailing fields in the FEN string",
            Self::InvalidBoardSize => "FEN position is formatted incorrectly",
            Self::MissingKing => "missing king in position",
            Self::InvalidSquare => "invalid square notation",
            Self::InvalidPiece => "unknown piece",
            Self::IllegalCastling => "castling allowed for incorrectly positioned king and rook",
            Self::ParseClockError(_) => "failed to parse clock field",
            Self::Empty => "empty FEN string",
        })
    }
}

impl Error for ParseLANError {}

impl fmt::Display for ParseLANError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::InvalidSquare => "invalid square notation",
            Self::InvalidPromotion => "invalid piece for promotion",
            Self::Empty => "empty LAN string",
        })
    }
}

impl Error for MoveError {}

impl fmt::Display for MoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::IllegalMove => "move is not legal",
        })
    }
}
