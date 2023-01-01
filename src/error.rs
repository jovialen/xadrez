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

impl From<ParseIntError> for ParseFenError {
    fn from(err: ParseIntError) -> Self {
        ParseFenError::ParseClockError(err)
    }
}
