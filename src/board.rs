//! Chessboard structures.
//!
//! Provides the structures relating to the chessboard as a whole.

use crate::error::{MoveError, ParseFenError};
use crate::evaluation;
use crate::evaluation::score::Score;
use crate::fen::FEN_STARTING_POSITION;
use crate::piece::{Piece, Side};
use crate::position::Position;
use crate::r#move::Move;
use crate::square::Square;

/// A game of Chess.
#[derive(Clone, PartialEq, Eq)]
pub struct Chessboard {
    pub(crate) position: Position,
    moves: Vec<Move>,
    history: Vec<Position>,
}

/// Represents the current state of the chess game.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GameState {
    /// The game has ended with a checkmate.
    Checkmate,
    /// The game has ended with a draw, with the reason for the draw specified.
    Draw(DrawReason),
    /// The game is currently in progress.
    Playing,
}

/// A reason for a draw in a chess game.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DrawReason {
    /// The game is a draw due to stalemate.
    Stalemate,
    /// The game is a draw due to the 50-move rule.
    Rule50,
}

impl Chessboard {
    /// Constructs a new `Chessboard` from a Forsyth-Edwards Notation (FEN)
    /// string.
    ///
    /// If you are uncertain whether the given FEN string is valid, use the
    /// `from_fen` function instead, which returns a `Result`.
    ///
    /// # Arguments
    ///
    /// * `fen` - A FEN string representing the position of the chessboard.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::prelude::*;
    ///
    /// let starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    ///
    /// let board = Chessboard::from_fen(starting_fen).unwrap();
    ///
    /// assert_eq!(board.side_to_move(), Side::White);
    /// ```
    ///
    /// # Panics
    ///
    /// This function panics if the given FEN string is invalid.
    #[must_use]
    pub fn new(fen: &str) -> Self {
        Self::from_fen(fen).expect("Failed to create chessboard; Invalid fen")
    }

    /// Creates a new Chessboard from a Forsyth-Edwards Notation (FEN) string.
    ///
    /// The FEN string describes the starting position of the Chessboard and
    /// includes information about which player is to move next, castling
    /// rights, en passant squares, and the halfmove clock and fullmove
    /// clock for move tracking.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::prelude::*;
    ///
    /// let starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    ///
    /// let board = Chessboard::from_fen(starting_fen).unwrap();
    ///
    /// assert_eq!(board.side_to_move(), Side::White);
    /// ```
    ///
    /// # Errors
    ///
    /// If the given FEN string is invalid, a `ParseFenError` will be returned.
    pub fn from_fen(fen: &str) -> Result<Self, ParseFenError> {
        let position = Position::from_fen(fen)?;
        let moves = position.generate_moves();

        Ok(Self {
            position,
            moves,
            history: Vec::new(),
        })
    }

    /// Returns a vector of all possible legal moves in the current position.
    #[must_use]
    pub fn moves(&self) -> Vec<Move> {
        self.moves.clone()
    }

    /// Checks if the given move is legal on the chessboard.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::prelude::*;
    ///
    /// let board = Chessboard::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// let e2e4 = Move::new(Square::E2, Square::E4).unwrap();
    /// assert!(board.is_legal(e2e4));
    ///
    /// let invalid_move = Move::new(Square::E3, Square::E4).unwrap();
    /// assert!(!board.is_legal(invalid_move));
    /// ```
    #[must_use]
    pub fn is_legal(&self, m: Move) -> bool {
        self.moves.contains(&m)
    }

    /// Returns a vector of tuples containing all the pieces on the board along
    /// with their squares.
    #[must_use]
    pub fn pieces(&self) -> Vec<(Square, Piece)> {
        self.position.pieces()
    }

    /// Get an array of the squares on the board and the piece on it.
    ///
    /// Returns an array representing the chessboard, with each element
    /// containing a tuple of the square and an optional piece occupying it. If
    /// a square is empty, its corresponding tuple contains a None value for the
    /// piece.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::prelude::*;
    ///
    /// let board = Chessboard::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// let squares = board.squares();
    ///
    /// assert_eq!(squares[0], (Square::A1, Piece::new(Side::White, PieceKind::Rook)));
    /// assert_eq!(squares[1], (Square::B1, Piece::new(Side::White, PieceKind::Knight)));
    /// assert_eq!(squares[2], (Square::C1, Piece::new(Side::White, PieceKind::Bishop)));
    /// // and so on...
    /// ```
    #[must_use]
    pub fn squares(&self) -> [(Square, Option<Piece>); 64] {
        self.position.squares()
    }

    /// Returns the current state of the game, which can be one of three
    /// possible values:
    ///
    /// * `GameState::Checkmate` - The game has ended due to one player being in
    ///   checkmate.
    /// * `GameState::Draw(reason)` - The game has ended in a draw, with a
    ///   specific `DrawReason` given.
    /// * `GameState::Playing` - The game is still ongoing.
    ///
    /// # Examples
    ///
    /// ```
    /// # use xadrez::Chessboard;
    /// let board = Chessboard::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// assert_eq!(board.game_state(), GameState::Playing);
    /// ```
    ///
    /// A draw can occur for different reasons, as represented by the
    /// `DrawReason` enum:
    ///
    /// * `DrawReason::Stalemate`: The player whose turn it is to move has no
    ///   legal moves, but is not in check.
    /// * `DrawReason::Rule50`: The game is drawn because 50 moves by each
    ///   player have been played without a pawn move or capture.
    #[must_use]
    pub fn game_state(&self) -> GameState {
        if self.position.data.halftime >= 50 {
            GameState::Draw(DrawReason::Rule50)
        } else if self.moves.is_empty() {
            if self.in_check() {
                GameState::Checkmate
            } else {
                GameState::Draw(DrawReason::Stalemate)
            }
        } else {
            GameState::Playing
        }
    }

    /// Returns true if the current side to move is in check, and false
    /// otherwise.
    #[must_use]
    pub fn in_check(&self) -> bool {
        self.position.in_check()
    }

    /// Returns the Side enum value representing the current side to move,
    /// either `Side::White` or `Side::Black`.
    #[must_use]
    pub fn side_to_move(&self) -> Side {
        self.position.side_to_move()
    }

    /// Evaluates the current position of the chessboard and returns a `Score`
    /// struct.
    ///
    /// The evaluation function uses either HCE (Handcrafted Evaluation
    /// Function) or NNUE (Efficiently Updatable Neural Network Evaluation
    /// Function) depending on whether the nnue feature is enabled at compile
    /// time. NNUE is a neural network-based evaluation function that has been
    /// shown to be much more efficient than HCE, and is used if the
    /// feature is enabled. In testing, this NNUE implementation has a better
    /// early and midgame, but struggles more with the endgame.
    #[must_use]
    pub fn evaluate(&self) -> Score {
        evaluation::evaluate_position(self.game_state(), &self.position)
    }

    /// Sets the position on the chessboard to the one specified by the
    /// Forsyth-Edwards Notation (FEN) string `fen`.
    ///
    /// If the FEN string is invalid, the position remains unchanged and an
    /// error is returned.
    ///
    /// # Arguments
    ///
    /// * `fen`: A string slice containing the FEN notation of the desired
    ///   position.
    ///
    /// # Errors
    ///
    /// Returns a `ParseFenError` if the given FEN string is invalid.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::prelude::*;
    ///
    /// let mut board = Chessboard::default();
    /// assert!(board.set_position("r1bqkbnr/pppppppp/2n5/1B6/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 2").is_ok());
    /// ```
    ///
    /// # Remarks
    ///
    /// The previously stored position can be restored by calling the `undo`
    /// function.
    pub fn set_position(&mut self, fen: &str) -> Result<(), ParseFenError> {
        let position = Position::from_fen(fen)?;

        self.history.push(self.position);

        self.position = position;
        self.moves = self.position.generate_moves();

        Ok(())
    }

    /// Makes a move on the chessboard.
    ///
    /// If the move isn't legal, no changes will occur on the board and the
    /// function will return an error. If the move is legal, the function will
    /// return the move that was made.
    ///
    /// # Arguments
    ///
    /// * `m` - The [`Move`] to be made.
    ///
    /// # Errors
    ///
    /// Returns a [`MoveError`] if the move isn't legal.
    pub fn make_move(&mut self, m: Move) -> Result<Move, MoveError> {
        let m = if let Some(legal) = self.moves.iter().find(|&legal| legal == &m) {
            Ok(*legal)
        } else {
            Err(MoveError::IllegalMove)
        }?;

        self.history.push(self.position);

        self.position = self.position.make_move(m);
        self.moves = self.position.generate_moves();

        Ok(m)
    }

    pub(crate) fn make_null_move(&mut self) {
        self.history.push(self.position);

        self.position = self.position.make_null_move();
        self.moves = self.position.generate_moves();
    }

    /// Undo the most recent move made on the board.
    ///
    /// If there is no move to undo, the function returns without modifying
    /// the current position.
    ///
    /// If a position was changed using the `set_position` function, the last
    /// position will also be restored.
    pub fn undo(&mut self) {
        if let Some(last_position) = self.history.pop() {
            self.position = last_position;
            self.moves = self.position.generate_moves();
        }
    }

    /// Returns the number of leaf nodes in the game tree for the current
    /// position at the given depth. This function is primarily used for testing
    /// and debugging purposes.
    ///
    /// The `print` flag indicates whether or not to print the result of each
    /// move calculation to the console. The result is the total number of leaf
    /// nodes found in the game tree at the given depth.
    ///
    /// # Arguments
    ///
    /// * `depth` - The depth at which to calculate the leaf nodes in the game
    /// tree.
    /// * `print` - Whether or not to print the result of each move calculation
    /// to the console.
    ///
    /// # Examples
    ///
    /// ```
    /// use xadrez::Chessboard;
    ///
    /// let board = Chessboard::default();
    /// let nodes = board.perft(4, false);
    /// assert_eq!(nodes, 197281);
    /// ```
    #[must_use]
    pub fn perft(&self, depth: usize, print: bool) -> usize {
        Self::perft_internal(&self.position, depth, print)
    }

    fn perft_internal(position: &Position, depth: usize, print: bool) -> usize {
        if depth == 0 {
            return 1;
        }

        let moves = position.generate_moves();

        if (depth == 1 || moves.is_empty()) && !print {
            return moves.len();
        }

        let mut nodes = 0;
        for m in moves {
            let count = Self::perft_internal(&position.make_move(m), depth - 1, false);
            nodes += count;

            if print {
                println!("{m}: {count}");
            }
        }
        nodes
    }
}

impl Default for Chessboard {
    fn default() -> Self {
        // Safety: Unwrap here is fine since the FEN starting position is always valid.
        Self::from_fen(FEN_STARTING_POSITION).unwrap()
    }
}

impl std::ops::Index<Square> for Chessboard {
    type Output = Option<Piece>;

    fn index(&self, index: Square) -> &Self::Output {
        &self.position[index]
    }
}

impl std::fmt::Debug for Chessboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Chessboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.position)
    }
}

impl std::fmt::Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Draw(reason) => write!(f, "Draw by {reason}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Display for DrawReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

    fn test_fen(fen: &str, err: Result<(), ParseFenError>) {
        let chessboard = Chessboard::from_fen(fen);
        if err.is_ok() {
            assert_eq!(chessboard.expect(fen).to_string(), fen);
        } else {
            assert_eq!(chessboard.err(), err.err());
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
}
