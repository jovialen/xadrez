//! Chessboard structures.
//!
//! Provides the structures relating to the chessboard as a whole.

use crate::{
    error::ParseFenError,
    fen::{FenString, FEN_STARTING_POSITION},
    piece::{Piece, PieceKind, Side, SIDE_COUNT},
};
use itertools::Itertools;
use num_derive::FromPrimitive;
use std::{fmt, str::FromStr};

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

/// Structure representing a chessboard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Chessboard {
    position: Position,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Position {
    squares: [Option<Piece>; BOARD_SIZE],
    side_to_move: Side,
    castling: [[bool; 2]; SIDE_COUNT],
    en_passant: Option<Square>,
    halftime: u32,
    fulltime: u32,
}

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

impl Chessboard {
    /// Create a new chessboard from a FEN string.
    ///
    /// # Arguments
    ///
    /// * `fen` - The FEN string with the board position.
    ///
    /// # Panics
    ///
    /// If the FEN string is invalid. If the FEN string may be invalid, use
    /// [`Chessboard::from_fen`] instead.
    pub fn new(fen: &str) -> Self {
        Self::from_fen(fen).unwrap()
    }

    /// Create a chessboard from a FEN string.
    ///
    /// # Arguments
    ///
    /// * `fen` - The FEN string to parse.
    pub fn from_fen(fen: &str) -> Result<Self, ParseFenError> {
        Ok(Self {
            position: Position::from_str(fen)?,
        })
    }
}

impl Default for Chessboard {
    fn default() -> Self {
        // Safety: The FEN starting position constant should always be valid.
        Self::from_fen(FEN_STARTING_POSITION).unwrap()
    }
}

impl fmt::Display for Chessboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.position.to_string().as_str())
    }
}

impl FromStr for Position {
    type Err = ParseFenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fen = FenString::try_from(s)?;

        let mut squares = [None; BOARD_SIZE];

        let mut i = 0;
        for rank in fen.squares.split('/').rev() {
            for piece in rank.chars() {
                squares[i] = match piece {
                    '1'..='8' => {
                        // Safety: Already validated that the char is a valid digit.
                        i += piece.to_digit(10).unwrap() as usize - 1;
                        None
                    }
                    _ => Some(Piece::try_from(piece)?),
                };
                i += 1;
            }
        }

        if i != BOARD_SIZE {
            return Err(ParseFenError::InvalidBoardSize);
        }

        let side_to_move = Side::from(fen.side_to_move);

        let mut castling = [[false; 2]; SIDE_COUNT];
        for p in fen
            .castling_ability
            .chars()
            .filter_map(|c| Piece::try_from(c).ok())
            .filter(|piece| piece.kind == PieceKind::King || piece.kind == PieceKind::Queen)
        {
            castling[p.side as usize][p.kind as usize] = true;
        }

        let en_passant = match fen.en_passant_target_square {
            "-" => None,
            target => Some(Square::from_str(target)?),
        };
        let halftime = fen.halftime.parse()?;
        let fulltime = fen.fulltime.parse()?;

        Ok(Self {
            squares,
            side_to_move,
            castling,
            en_passant,
            halftime,
            fulltime,
        })
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, slice) in self.squares.chunks_exact(BOARD_FILES).rev().enumerate() {
            if i != 0 {
                write!(f, "/")?;
            }

            for (key, group) in &slice.iter().group_by(|piece| piece.is_some()) {
                if key {
                    for piece in group {
                        // Safety: If the key is true, the options are all Some
                        write!(f, "{}", piece.unwrap())?;
                    }
                } else {
                    write!(f, "{}", group.count())?;
                }
            }
        }

        write!(f, " {}", self.side_to_move)?;

        write!(
            f,
            " {}",
            if self.castling.iter().flatten().any(|castling| *castling) {
                self.castling
                    .iter()
                    .flatten()
                    .enumerate()
                    .filter(|(_, castling)| **castling)
                    .fold(String::new(), |acc, (i, _)| {
                        acc + match i {
                            0 => "K",
                            1 => "Q",
                            2 => "k",
                            3 => "q",
                            _ => unreachable!(),
                        }
                    })
            } else {
                "-".to_string()
            }
        )?;

        write!(
            f,
            " {}",
            match self.en_passant {
                Some(square) => square.to_string().to_lowercase(),
                None => "-".to_string(),
            }
        )?;

        write!(f, " {}", self.halftime)?;
        write!(f, " {}", self.fulltime)?;

        Ok(())
    }
}

impl Square {
    /// Get the square on the given rank and file.
    ///
    /// # Arguments
    ///
    /// * `rank` - The rank of the square.
    /// * `file` - The file of the square.
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
}

impl TryFrom<usize> for Square {
    type Error = ParseFenError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        num::FromPrimitive::from_usize(usize::from(value)).ok_or(ParseFenError::InvalidSquare)
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
            file @ 'A'..='H' => file as u8 - 'A' as u8,
            _ => Err(ParseFenError::InvalidSquare)?,
        };

        let rank = iter
            .next()
            .ok_or(ParseFenError::InvalidSquare)?
            .to_digit(10)
            .ok_or(ParseFenError::InvalidSquare)?
            - 1;

        Square::from_rank_file(rank as u16, file as u16)
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fen::FEN_EMPTY_POSITION;

    fn test_fen(fen: &str) {
        let chessboard = Chessboard::from_fen(fen);
        assert!(chessboard.is_ok());
        assert_eq!(chessboard.unwrap().to_string(), fen);
    }

    #[test]
    fn fen_parsing() {
        test_fen(FEN_STARTING_POSITION);
        test_fen(FEN_EMPTY_POSITION);
        test_fen("8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P1K/8/8 b - - 99 50");
        test_fen("8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 1 49");
        test_fen("5r1k/6pp/4Qpb1/p7/8/6PP/P4PK1/3q4 b - - 4 37");
        test_fen("8/8/2P5/4B3/1Q6/4K3/6P1/3k4 w - - 5 67");
        test_fen("r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13");
        test_fen("8/pppp1ppp/8/4p3/8/7P/PPPPPPP1/8 w - e6 0 13");
    }
}
