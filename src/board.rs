//! Chessboard structures.
//!
//! Provides the structures relating to the chessboard as a whole.

#[allow(clippy::wildcard_imports)]
use crate::{
    bitboards::{constants::*, Bitboard},
    error::{MoveError, ParseFenError},
    fen::{FenString, FEN_STARTING_POSITION},
    movegen,
    piece::{Piece, PieceKind, Side, SIDE_COUNT},
    r#move::{Move, MoveKind},
};
use itertools::Itertools;
use num_derive::FromPrimitive;
use std::{fmt, ops, str::FromStr};

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
#[derive(Clone, Debug, PartialEq)]
pub struct Chessboard {
    position: Position,
    legal_moves: Vec<Move>,
    history: Vec<(Position, Vec<Move>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Position {
    pub squares: [Option<Piece>; BOARD_SIZE],
    pub side_to_move: Side,
    pub castling: [[bool; 2]; SIDE_COUNT],
    pub en_passant: Option<Square>,
    pub halftime: u32,
    pub fulltime: u32,
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
    #[must_use]
    pub fn new(fen: &str) -> Self {
        Self::from_fen(fen).unwrap()
    }

    /// Create a chessboard from a FEN string.
    ///
    /// # Arguments
    ///
    /// * `fen` - The FEN string to parse.
    ///
    /// # Errors
    ///
    /// Can return a [`ParseFenError`] if the given FEN string is not valid.
    pub fn from_fen(fen: &str) -> Result<Self, ParseFenError> {
        let position = Position::from_str(fen)?;

        Ok(Self {
            position,
            legal_moves: movegen::generate_legal_moves(&position),
            history: Vec::new(),
        })
    }

    /// Set the position of the chessboard with a FEN string.
    ///
    /// # Arguments
    ///
    /// * `fen` - FEN string of the new position.
    ///
    /// # Errors
    ///
    /// Can return a [`ParseFenError`] if the given FEN string is not valid.
    pub fn set_position(&mut self, fen: &str) -> Result<(), ParseFenError> {
        self.save_current_to_history();
        self.position = Position::from_str(fen)?;
        self.legal_moves = movegen::generate_legal_moves(&self.position);
        Ok(())
    }

    /// Get all possible legal moves.
    #[must_use]
    pub fn moves(&self) -> &Vec<Move> {
        &self.legal_moves
    }

    /// Make a move on the chessboard.
    ///
    /// The move must be legal.
    ///
    /// # Arguments
    ///
    /// * `m` - A legal move to make on the board.
    ///
    /// # Errors
    ///
    /// Will return a [`MoveError`] if the given move cannot be made on the
    /// board.
    #[allow(clippy::missing_panics_doc)]
    pub fn make_move(&mut self, m: Move) -> Result<(), MoveError> {
        const ROOK_SOURCES: [[Square; 2]; SIDE_COUNT] =
            [[Square::H1, Square::A1], [Square::H8, Square::A8]];
        const ROOK_DESTS: [[Square; 2]; SIDE_COUNT] =
            [[Square::F1, Square::D1], [Square::F8, Square::D8]];

        // Check if move is legal
        let m = if let Some(legal) = self.legal_moves.iter().find(|&legal| legal == &m) {
            // Filter out Any kind moves with the actual legal equivilant.
            *legal
        } else {
            return Err(MoveError::IllegalMove);
        };

        let friendly = self.position.side_to_move as usize;

        // Save the current position in the history
        self.save_current_to_history();

        // Safety: There will always be a from piece if the move is legal, and that was
        // confirmed above.
        let to_move = self.position[m.from].unwrap();

        // Clear the en passant
        self.position.en_passant = None;

        // Do the move
        self.position[m.to] = self.position[m.from];
        self.position[m.from] = None;

        self.position.halftime += 1;
        self.position.fulltime += 1;

        // Check for any special conditions with the move
        match m.kind {
            MoveKind::EnPassant => {
                assert!(to_move.kind == PieceKind::Pawn);

                let capture_square =
                    m.to.neighbour(to_move.side.backward())
                        .expect("Invalid en-passant target square");
                self.position[capture_square] = None;
            }
            MoveKind::Capture => self.position.halftime = 0,
            MoveKind::Promotion(into) => {
                assert!(to_move.kind == PieceKind::Pawn);

                if let Some(ref mut piece) = self.position[m.to] {
                    piece.kind = into;
                }
            }
            MoveKind::Castling => {
                let side = m.to.side() as usize;

                assert!(self.position.castling[friendly][side]);

                let src = ROOK_SOURCES[friendly][side];
                let dest = ROOK_DESTS[friendly][side];

                assert_eq!(
                    self.position[src],
                    Some(Piece::new(self.position.side_to_move, PieceKind::Rook))
                );
                assert_eq!(self.position[dest], None);

                self.position[dest] = self.position[src];
                self.position[src] = None;
            }
            MoveKind::Any => unreachable!("No move of kind \"Any\" should ever be used."),
            MoveKind::Quiet => (),
        }

        if to_move.kind == PieceKind::Pawn {
            const EP_DISTANCE: f64 = 2.0;

            self.position.halftime = 0;

            if m.from.distance(m.to) >= EP_DISTANCE {
                let ep_index = (m.from as usize + m.to as usize) / 2;
                self.position.en_passant = Some(Square::try_from(ep_index).unwrap());
            }
        } else if to_move.kind == PieceKind::King {
            self.position.castling[friendly] = [false, false];
        }

        // Update castling
        #[allow(clippy::needless_range_loop)]
        for side in 0..SIDE_COUNT {
            let rook = Piece::new(
                match side {
                    0 => Side::White,
                    1 => Side::Black,
                    _ => unreachable!(),
                },
                PieceKind::Rook,
            );

            for i in 0..2 {
                self.position.castling[side][i] &=
                    self.position[ROOK_SOURCES[side][i]] == Some(rook);
            }
        }

        // Update side to move
        self.position.side_to_move = !self.position.side_to_move;

        // Update legal moves
        self.legal_moves = movegen::generate_legal_moves(&self.position);

        Ok(())
    }

    /// Undo the last move.
    pub fn undo(&mut self) {
        if let Some((last_pos, last_moves)) = self.history.pop() {
            self.position = last_pos;
            self.legal_moves = last_moves;
        }
    }

    /// Perft move enumeration function.
    ///
    /// Counts all possible legal moves from the current position to a given
    /// depth.
    ///
    /// # Arguments
    ///
    /// * `depth` - How many max moves to make. Must be at least 1.
    /// * `print` - If the nodes for each node should be printed.
    #[allow(clippy::missing_panics_doc)]
    pub fn perft(&mut self, depth: usize, print: bool) -> usize {
        if depth == 0 {
            return 1;
        }

        let moves = self.moves().clone();

        if depth == 1 && !print {
            return moves.len();
        }

        let mut nodes = 0;

        for m in moves {
            // Safety: All moves should be legal
            self.make_move(m).unwrap();
            let count = self.perft(depth - 1, false);
            nodes += count;
            self.undo();

            if print {
                println!("{m}: {count}");
            }
        }

        nodes
    }

    /// Check if a move is legal.
    ///
    /// # Arguments
    ///
    /// * `m` - The move to validate.
    #[must_use]
    pub fn is_legal(&self, m: Move) -> bool {
        self.legal_moves.contains(&m)
    }

    /// Get a refrence to the board squares.
    ///
    /// This function returns a refrence to the internal one dimentional array
    /// of squares. The array is laid out such that it can be directly
    /// indexed with the integer values of the [`Square`] enum.
    #[must_use]
    pub fn squares(&self) -> &[Option<Piece>; BOARD_SIZE] {
        &self.position.squares
    }

    /// Get all the pieces on the board.
    ///
    /// This function returns a vector of all the pieces on the board in a toupe
    /// with its position on the board.
    #[must_use]
    pub fn pieces(&self) -> Vec<(Piece, Square)> {
        self.position.pieces()
    }

    fn save_current_to_history(&mut self) {
        self.history.push((self.position, self.legal_moves.clone()));
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

impl Position {
    pub fn pieces(&self) -> Vec<(Piece, Square)> {
        self.squares
            .iter()
            .enumerate()
            .filter_map(|(i, &option)| Some((option?, Square::try_from(i).ok()?)))
            .collect()
    }
}

impl ops::Index<Square> for Position {
    type Output = Option<Piece>;

    fn index(&self, index: Square) -> &Self::Output {
        &self.squares[index as usize]
    }
}

impl ops::IndexMut<Square> for Position {
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self.squares[index as usize]
    }
}

impl FromStr for Position {
    type Err = ParseFenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(ParseFenError::Empty);
        }

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
    pub fn to_rank_file(&self) -> (usize, usize) {
        let rank = *self as usize / BOARD_FILES;
        let file = *self as usize % BOARD_FILES;
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
    pub(crate) fn rank(self) -> Bitboard {
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
    pub(crate) fn file(self) -> Bitboard {
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
    fn side(self) -> PieceKind {
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
    pub(crate) fn offset(self) -> isize {
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

    #[test]
    fn neighbour_squares() {
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
