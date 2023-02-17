//! Chessboard structures.
//!
//! Provides the structures relating to the chessboard as a whole.

use crate::error::{MoveError, ParseFenError};
use crate::fen::FEN_STARTING_POSITION;
use crate::piece::{Piece, PieceKind, Side, SIDE_COUNT};
use crate::position::{Position, PositionBitboards, EMPTY_POSITION};
use crate::r#move::{Move, MoveKind};
use crate::square::{Square, BOARD_SIZE};
use crate::{evaluation, movegen};

use std::str::FromStr;
use std::{fmt, ops};

/// Structure representing a chessboard.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Chessboard {
    pub(crate) position: Position,
    pub(crate) bitboards: PositionBitboards,
    legal_moves: Vec<Move>,
    history: Vec<(Position, Vec<Move>)>,
}

/// Chessboard builder.
///
/// This structure allows you to build a position without worrying about the
/// legality of it untill it is built.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone)]
pub struct BoardBuilder {
    position: Position,
}

/// The possible states of the game.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GameState {
    /// The side to move is in checkmate.
    Checkmate,
    /// A draw has been reached.
    Draw(DrawReason),
    /// The game is not finished.
    Playing,
}

/// The reason for the draw.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DrawReason {
    /// The side to move has no legal moves, but is not in check.
    Stalemate,
    /// No pawn has moved and no captures have occured in the last 50 moves.
    Rule50,
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
        let bitboards = position.bitboards();
        let legal_moves = movegen::generate_legal_moves(&position, &bitboards);

        Ok(Self {
            position,
            bitboards,
            legal_moves,
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
        self.bitboards = self.position.bitboards();
        self.legal_moves = movegen::generate_legal_moves(&self.position, &self.bitboards);
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

        if self.position.side_to_move == Side::Black {
            self.position.halftime += 1;
            self.position.fulltime += 1;
        }

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
        self.bitboards = self.position.bitboards();
        self.legal_moves = movegen::generate_legal_moves(&self.position, &self.bitboards);

        Ok(())
    }

    /// Undo the last move.
    pub fn undo(&mut self) {
        if let Some((last_pos, last_moves)) = self.history.pop() {
            self.position = last_pos;
            self.bitboards = self.position.bitboards();
            self.legal_moves = last_moves;
        }
    }

    /// Get the current state of the board.
    #[must_use]
    pub fn state(&self) -> GameState {
        if self.position.halftime >= 50 {
            GameState::Draw(DrawReason::Rule50)
        } else if self.legal_moves.is_empty() {
            if self.in_check() {
                GameState::Checkmate
            } else {
                GameState::Draw(DrawReason::Stalemate)
            }
        } else {
            GameState::Playing
        }
    }

    /// Check if the side to move is currently in check.
    #[must_use]
    pub fn in_check(&self) -> bool {
        self.bitboards.count_checkers() > 0
    }

    /// Evaluate the current position.
    ///
    /// The position will be evaluated from the perspective of white and return
    /// the advantage in approximate centi-pawns.
    #[must_use]
    pub fn evaluate(&self) -> i32 {
        let relative_to_side = self.evaluate_relative();
        match self.position.side_to_move {
            Side::White => relative_to_side,
            Side::Black => -relative_to_side,
        }
    }

    /// Evaluate the current position relative to the side to move.
    ///
    /// The position will be evaluated from the perspective of the side to move
    /// and return the advantage in approximate centi-pawns.
    #[must_use]
    pub fn evaluate_relative(&self) -> i32 {
        let state = self.state();

        if state == GameState::Playing {
            evaluation::evaluate_position(&self.position, &self.bitboards)
        } else if state == GameState::Checkmate {
            -i32::MAX
        } else {
            // Its a draw
            0
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

impl ops::Index<Square> for Chessboard {
    type Output = Option<Piece>;

    fn index(&self, index: Square) -> &Self::Output {
        &self.position[index]
    }
}

impl FromStr for Chessboard {
    type Err = ParseFenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl fmt::Display for Chessboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.position.to_string().as_str())
    }
}

impl BoardBuilder {
    /// Create a new board builder.
    ///
    /// The board will by default be completly empty, with no pieces on the
    /// board, no time elapsed, and no castling rights.
    #[must_use]
    pub fn new() -> Self {
        Self {
            position: EMPTY_POSITION,
        }
    }

    /// Build the Chessboard.
    ///
    /// # Errors
    ///
    /// Can return a [`ParseFenError`] if the position in the builder is not
    /// legal.
    pub fn build(self) -> Result<Chessboard, ParseFenError> {
        let fen = self.position.to_string();
        Chessboard::from_str(fen.as_str())
    }

    /// Set a piece on the board.
    ///
    /// No pieces are placed on the board by default.
    ///
    /// # Arguments
    ///
    /// * `square` - Where to place the piece.
    /// * `piece` - The piece on the square.
    #[must_use]
    pub fn piece(mut self, square: Square, piece: Piece) -> Self {
        self.position[square] = Some(piece);
        self
    }

    /// Set the side to move.
    ///
    /// Initially set to white.
    ///
    /// # Arguments
    ///
    /// * `side` - The side to make a move.
    #[must_use]
    pub fn side_to_move(mut self, side: Side) -> Self {
        self.position.side_to_move = side;
        self
    }

    /// Set castling rights.
    ///
    /// Initially neither side has any castling rights, even if both the king
    /// and rooks are in position.
    ///
    /// # Arguments
    ///
    /// * `side` - The side to set the castling right for.
    /// * `kingside` - `true` for kingside castling, `false` for queenside
    ///   castling.
    /// * `allowed` - Whether or not to allow castling.
    #[must_use]
    pub fn castling(mut self, side: Side, kingside: bool, allowed: bool) -> Self {
        let i = if kingside {
            PieceKind::King as usize
        } else {
            PieceKind::Queen as usize
        };

        self.position.castling[side as usize][i] = allowed;
        self
    }

    /// Set the en passant square.
    ///
    /// If not set explicitly here, no en passant square will be set at all.
    ///
    /// # Arguments
    ///
    /// * `target_square` - The en passant square.
    #[must_use]
    pub fn en_passant(mut self, target_square: Square) -> Self {
        self.position.en_passant = Some(target_square);
        self
    }

    /// Set the halftime.
    ///
    /// This has an initial value of 0.
    ///
    /// # Arguments
    ///
    /// * `halftime` - Moves since last capture or pawn push.
    #[must_use]
    pub fn halftime(mut self, halftime: u32) -> Self {
        self.position.halftime = halftime;
        self
    }

    /// Set the fulltime.
    ///
    /// This has an initial value of 1.
    ///
    /// # Arguments
    ///
    /// * `fulltime` - Moves since the start of the game.
    #[must_use]
    pub fn fulltime(mut self, fulltime: u32) -> Self {
        self.position.fulltime = fulltime;
        self
    }
}

impl Default for BoardBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Draw(reason) => write!(f, "Draw by {reason}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

impl fmt::Display for DrawReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Stalemate => "stalemate",
            Self::Rule50 => "50 move rule",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fen::FEN_EMPTY_POSITION;
    #[allow(clippy::wildcard_imports)]
    use crate::piece::constants::*;
    use crate::square::Direction;

    fn test_fen(fen: &str, err: Result<(), ParseFenError>) {
        let chessboard = Chessboard::from_fen(fen);
        if let Err(e) = err {
            assert_eq!(chessboard, Err(e));
        } else {
            assert_eq!(chessboard.expect(fen).to_string(), fen);
        }
    }

    #[test]
    fn test_fen_parsing() {
        test_fen(FEN_STARTING_POSITION, Ok(()));
        test_fen(FEN_EMPTY_POSITION, Err(ParseFenError::MissingKing));
        test_fen("8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P1K/8/8 b - - 99 50", Ok(()));
        test_fen("8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 1 49", Ok(()));
        test_fen("5r1k/6pp/4Qpb1/p7/8/6PP/P4PK1/3q4 b - - 4 37", Ok(()));
        test_fen("8/8/2P5/4B3/1Q6/4K3/6P1/3k4 w - - 5 67", Ok(()));
        test_fen(
            "r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13",
            Ok(()),
        );
        test_fen("6k1/pppp1ppp/8/4p3/8/7P/PPPPPPP1/4K3 w - e6 0 13", Ok(()));
        test_fen(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN1 w Qkq - 0 1",
            Ok(()),
        );
        test_fen(
            "1nbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBN1 w k - 0 1",
            Ok(()),
        );
        test_fen(
            "1nbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBN1 w KQq - 0 1",
            Err(ParseFenError::IllegalCastling),
        );
    }

    #[test]
    fn test_board_building() {
        let board = BoardBuilder::new()
            .piece(Square::A1, WHITE_ROOK)
            .piece(Square::H1, WHITE_ROOK)
            .piece(Square::B1, WHITE_KNIGHT)
            .piece(Square::G1, WHITE_KNIGHT)
            .piece(Square::C1, WHITE_BISHOP)
            .piece(Square::F1, WHITE_BISHOP)
            .build();
        assert_eq!(board, Err(ParseFenError::MissingKing));

        let board = BoardBuilder::new()
            .piece(Square::E1, WHITE_KING)
            .piece(Square::E8, BLACK_KING)
            .side_to_move(Side::Black)
            .halftime(10)
            .fulltime(11)
            .build();
        assert_eq!(
            board.map(|board| board.to_string()),
            Ok(String::from("4k3/8/8/8/8/8/8/4K3 b - - 10 11"))
        );

        let board = BoardBuilder::new()
            .piece(Square::E1, WHITE_KING)
            .piece(Square::E8, BLACK_KING)
            .piece(Square::A1, WHITE_ROOK)
            .castling(Side::White, false, true)
            .build();
        assert_eq!(
            board.map(|board| board.to_string()),
            Ok(String::from("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"))
        );
    }

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
