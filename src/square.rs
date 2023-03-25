//! Chessboard squares.
//!
//! Provides the enum for the chessboard squares as well as constants relating
//! to the squares on a chessboard.

#[allow(clippy::wildcard_imports)]
use crate::bitboards::constants::*;
use crate::bitboards::Bitboard;
use crate::error::ParseFenError;
use crate::piece::PieceKind;
use num_derive::FromPrimitive;
use std::fmt;
#[allow(clippy::wildcard_imports)]
use std::str::FromStr;

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
pub const BOARD_SIZE: usize = BOARD_FILES * BOARD_RANKS;

/// The squares on the chessboard.
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, FromPrimitive, PartialEq, Eq)]
pub(crate) enum Direction {
    North,
    East,
    South,
    West,
    NorthEast,
    NorthWest,
    SouthEast,
    SouthWest,
}

impl Square {
    /// Get the square on the given rank and file.
    ///
    /// # Arguments
    ///
    /// * `rank` - The rank of the square, from 0 to 7.
    /// * `file` - The file of the square, from 0 to 7.
    ///
    /// # Errors
    ///
    /// Returns a [`ParseFenError::InvalidSquare`] error if the given rank and
    /// file are outside of the board bounds.
    pub fn from_rank_file<R, F>(rank: R, file: F) -> Result<Self, ParseFenError>
    where
        usize: From<R> + From<F>,
    {
        let rank = usize::from(rank);
        let file = usize::from(file);
        if rank >= BOARD_RANKS || file >= BOARD_FILES {
            Err(ParseFenError::InvalidSquare)
        } else {
            let i = rank * BOARD_FILES + file;
            Square::try_from(i)
        }
    }

    /// Get the rank and file of the square.
    #[must_use]
    pub const fn to_rank_file(self) -> (usize, usize) {
        let rank = self as usize / BOARD_FILES;
        let file = self as usize % BOARD_FILES;
        (rank, file)
    }

    /// Get the distance between two squares.
    #[must_use]
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub fn distance(&self, rhs: Self) -> f64 {
        let (sr, sf) = self.to_rank_file();
        let (rr, rf) = rhs.to_rank_file();
        let x_diff = f64::from(sf as i32 - rf as i32);
        let y_diff = f64::from(sr as i32 - rr as i32);
        (x_diff.powf(2.0) + y_diff.powf(2.0)).sqrt()
    }

    #[inline]
    pub(crate) fn neighbour(self, direction: Direction) -> Option<Square> {
        match Square::try_from(self as isize + direction.offset()) {
            Ok(dest) if self.distance(dest) <= 2.0 => Some(dest),
            _ => None,
        }
    }

    #[inline]
    pub(crate) const fn rank(self) -> Bitboard {
        let (rank, _) = self.to_rank_file();
        match rank {
            0 => BITBOARD_RANK_1,
            1 => BITBOARD_RANK_2,
            2 => BITBOARD_RANK_3,
            3 => BITBOARD_RANK_4,
            4 => BITBOARD_RANK_5,
            5 => BITBOARD_RANK_6,
            6 => BITBOARD_RANK_7,
            7 => BITBOARD_RANK_8,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(crate) const fn file(self) -> Bitboard {
        let (_, file) = self.to_rank_file();
        match file {
            0 => BITBOARD_FILE_A,
            1 => BITBOARD_FILE_B,
            2 => BITBOARD_FILE_C,
            3 => BITBOARD_FILE_D,
            4 => BITBOARD_FILE_E,
            5 => BITBOARD_FILE_F,
            6 => BITBOARD_FILE_G,
            7 => BITBOARD_FILE_H,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(crate) const fn side(self) -> PieceKind {
        let (_, file) = self.to_rank_file();
        if file >= 4 {
            PieceKind::King
        } else {
            PieceKind::Queen
        }
    }
}

impl From<Square> for usize {
    fn from(value: Square) -> Self {
        value as usize
    }
}

impl TryFrom<usize> for Square {
    type Error = ParseFenError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        num::FromPrimitive::from_usize(value).ok_or(ParseFenError::InvalidSquare)
    }
}

impl TryFrom<isize> for Square {
    type Error = ParseFenError;

    fn try_from(value: isize) -> Result<Self, Self::Error> {
        num::FromPrimitive::from_isize(value).ok_or(ParseFenError::InvalidSquare)
    }
}

impl FromStr for Square {
    type Err = ParseFenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.chars();

        let file = match iter
            .next()
            .ok_or(ParseFenError::InvalidSquare)?
            .to_ascii_uppercase()
        {
            file @ 'A'..='H' => file as u8 - b'A',
            _ => Err(ParseFenError::InvalidSquare)?,
        };

        let rank = iter
            .next()
            .ok_or(ParseFenError::InvalidSquare)?
            .to_digit(10)
            .ok_or(ParseFenError::InvalidSquare)?
            - 1;

        Square::from_rank_file(rank as usize, file)
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Direction {
    #[allow(clippy::cast_possible_wrap)]
    pub(crate) const fn offset(self) -> isize {
        match self {
            Self::North => BOARD_FILES as isize,
            Self::East => 1,
            Self::South => -(BOARD_FILES as isize),
            Self::West => -1,
            Self::NorthEast => BOARD_FILES as isize + 1,
            Self::NorthWest => BOARD_FILES as isize - 1,
            Self::SouthEast => -(BOARD_FILES as isize) + 1,
            Self::SouthWest => -(BOARD_FILES as isize) - 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neighbour_squares() {
        assert_eq!(Square::A1.neighbour(Direction::North), Some(Square::A2));
        assert_eq!(Square::A2.neighbour(Direction::South), Some(Square::A1));
        assert_eq!(Square::A1.neighbour(Direction::East), Some(Square::B1));
        assert_eq!(Square::B1.neighbour(Direction::West), Some(Square::A1));

        assert_eq!(Square::A1.neighbour(Direction::NorthEast), Some(Square::B2));
        assert_eq!(Square::B2.neighbour(Direction::SouthWest), Some(Square::A1));
        assert_eq!(Square::B1.neighbour(Direction::NorthWest), Some(Square::A2));
        assert_eq!(Square::A2.neighbour(Direction::SouthEast), Some(Square::B1));
    }
}
