//! Error types of the chess engine.
//!
//! Provides the error types used by the chess engine.

use core::fmt;
use std::{error::Error, num::ParseIntError};

/// An error which can be returned while parsing a FEN string.
#[derive(Debug)]
pub enum ParseFenError {
    /// One or more fields are missing from the FEN string.
    MissingField,
    /// There are one or more extra fields on the FEN string.
    TrailingField,
    /// The FEN board representation is either too big or too small.
    InvalidBoardSize,
    /// Either the rank or file of the square was out of bounds.
    InvalidSquare,
    /// No piece represented by the given char.
    InvalidPiece(char),
    /// Failed to parse either the fulltime or halftime clock.
    ParseClockError(ParseIntError),
    /// The input was empty.
    Empty,
}

/// An error which can be returned while parsing long algebraic notation.
#[derive(Debug)]
pub enum ParseLANError {
    /// Either the to or from square was missing.
    MissingField,
    /// Either the to or from square was invalid.
    InvalidSquare,
    /// The promotion of the piece was invalid.
    InvalidPromotion,
    /// The input was either too long or too short to be vaild.
    InvalidSize,
    /// The input was empty.
    Empty,
}

impl Error for ParseFenError {
    fn description(&self) -> &str {
        match self {
            Self::MissingField => "one or more fields are missing from the FEN string",
            Self::TrailingField => "there are one or more trailing fields in the FEN string",
            Self::InvalidBoardSize => "FEN position is formatted incorrectly",
            Self::InvalidSquare => "invalid square notation",
            Self::InvalidPiece(_) => "unknown piece",
            Self::ParseClockError(_) => "failed to parse clock field",
            Self::Empty => "empty FEN string",
        }
    }
}

impl From<ParseIntError> for ParseFenError {
    fn from(err: ParseIntError) -> Self {
        ParseFenError::ParseClockError(err)
    }
}

impl fmt::Display for ParseFenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for ParseLANError {
    fn description(&self) -> &str {
        match self {
            Self::MissingField => "missing either from or to square",
            Self::InvalidSquare => "invalid square notation",
            Self::InvalidPromotion => "invalid piece for promotion",
            Self::InvalidSize => "incorrectly formatted LAN string",
            Self::Empty => "empty LAN string",
        }
    }
}

impl fmt::Display for ParseLANError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}
