//! FEN structures.
//!
//! Provides the FEN structures.

use crate::error::ParseFenError;

/// The starting position of a chess game as a FEN string.
pub const FEN_STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
/// An empty chess position as a FEN string.
pub const FEN_EMPTY_POSITION: &str = "8/8/8/8/8/8/8/8 w - - 0 1";

#[derive(Debug)]
pub(crate) struct FenString<'a> {
    pub squares: &'a str,
    pub side_to_move: char,
    pub castling_ability: &'a str,
    pub en_passant_target_square: &'a str,
    pub halftime: &'a str,
    pub fulltime: &'a str,
}

impl<'a> TryFrom<&'a str> for FenString<'a> {
    type Error = ParseFenError;

    fn try_from(fen: &'a str) -> Result<Self, Self::Error> {
        #[allow(clippy::enum_glob_use)]
        use ParseFenError::*;

        let mut iter = fen.split_whitespace();

        let res = Self {
            squares: iter.next().ok_or(MissingField)?,
            // Safety: Since this iterator returns text split by whitespace, there will always be
            // at least one char
            side_to_move: iter.next().ok_or(MissingField)?.chars().next().unwrap(),
            castling_ability: iter.next().ok_or(MissingField)?,
            en_passant_target_square: iter.next().ok_or(MissingField)?,
            halftime: iter.next().ok_or(MissingField)?,
            fulltime: iter.next().ok_or(MissingField)?,
        };

        if iter.count() > 0 {
            Err(TrailingField)
        } else {
            Ok(res)
        }
    }
}
