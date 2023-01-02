//! Error types of the chess engine.
//!
//! Provides the error types used by the chess engine.

use std::num::ParseIntError;

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
}

impl From<ParseIntError> for ParseFenError {
    fn from(err: ParseIntError) -> Self {
        ParseFenError::ParseClockError(err)
    }
}
