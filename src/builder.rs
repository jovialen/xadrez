//! Chessboard Builder.
//!
//! This module provides a builder for the `Chessboard` struct, which is used to
//! create a new chessboard with the desired initial positions of the pieces.
//!
//! The `ChessboardBuilder` struct provides a way to configure the initial
//! positions of the pieces on the chessboard. Once all the desired
//! configurations are set, the `build` method is called to return the resulting
//! `Chessboard`.
//!
//! Example:
//!
//! ```
//! use xadrez::prelude::*;
//!
//! let chessboard = BoardBuilder::new()
//!     // Place a white pawn at e2
//!     piece(Square::E2, Piece::new(Side::White, PieceKind::Pawn))
//!     // Place a black pawn at e7
//!     .piece(Square::E7, Piece::new(Side::Black, PieceKind::Pawn))
//!     .build();
//! ```

use crate::board::Chessboard;
use crate::error::ParseFenError;
use crate::fen::FEN_STARTING_POSITION;
use crate::piece::{Piece, Side};
use crate::position::PositionData;
use crate::square::Square;
use std::str::FromStr;

/// Chessboard Builder.
///
/// A builder for a `Chessboard` struct, which is used to create a new
/// chessboard with the desired initial positions of the pieces.
#[allow(clippy::module_name_repetitions)]
pub struct BoardBuilder {
    data: PositionData,
}

impl BoardBuilder {
    /// Create a new board builder.
    #[must_use]
    pub fn new() -> Self {
        Self {
            data: PositionData::empty(),
        }
    }

    /// Builds and returns a new `Chessboard` with the desired initial positions
    /// of the pieces, as set through the `BoardBuilder` methods.
    ///
    /// # Example
    /// ```
    /// use chess::{BoardBuilder, Chessboard};
    ///
    /// let board: Chessboard = BoardBuilder::new()
    ///     .start_position()
    ///     .build()
    ///     .unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// If any errors occur during the parsing of the FEN position string, a
    /// `ParseFenError` is returned.
    pub fn build(self) -> Result<Chessboard, ParseFenError> {
        Chessboard::from_fen(&self.data.to_string())
    }

    /// Set the board to the default starting position.
    ///
    /// This method sets the board to the standard starting position for chess,
    /// with all pieces in their default locations.
    #[must_use]
    pub fn start_position(self) -> Self {
        self.position(FEN_STARTING_POSITION)
            .expect("FEN is valid and should never fail parsing")
    }

    /// Adds the pieces and attributes from the given FEN position to the
    /// current board being built. Any pieces that already exist on the board
    /// at the same square will not be overwritten.
    ///
    /// # Arguments
    ///
    /// * `fen` - A FEN string representing the position to add to the board.
    ///
    /// # Errors
    ///
    /// This method will return an error if the FEN string is invalid.
    pub fn position(mut self, fen: &str) -> Result<Self, ParseFenError> {
        let other = PositionData::from_str(fen)?;

        for (square, piece) in other.pieces() {
            if self.data[square].is_none() {
                self.data[square] = Some(piece);
            }
        }

        if self.data.en_passant.is_none() {
            self.data.en_passant = other.en_passant;
        }

        self.data.castling[0][0] |= other.castling[0][0];
        self.data.castling[0][1] |= other.castling[0][1];
        self.data.castling[1][0] |= other.castling[1][0];
        self.data.castling[1][1] |= other.castling[1][1];

        self.data.halftime = self.data.halftime.max(other.halftime);
        self.data.fulltime = self.data.fulltime.max(other.fulltime);

        Ok(self)
    }

    /// Sets a specific [`Piece`] on a [`Square`] of the board.
    ///
    /// This method takes a [`Square`] and an [`Option<Piece>`] and sets the
    /// [`Piece`] on the corresponding [`Square`] of the `BoardBuilder`. If
    /// `piece` is [`None`], the Square is cleared of any piece.
    ///
    /// # Arguments
    ///
    /// * `square` - The [`Square`] to set the Piece on.
    /// * `piece` - The [`Piece`] to set on the [`Square`], or [`None`] to clear
    ///   it.
    #[must_use]
    pub fn piece(mut self, square: Square, piece: Option<Piece>) -> Self {
        self.data[square] = piece;
        self
    }

    /// Sets the en-passant square on the chessboard.
    ///
    /// # Arguments
    ///
    /// * `en_passant_square` - The square on which the en-passant capture can
    ///   be made.
    #[must_use]
    pub fn en_passant(mut self, en_passant_square: Square) -> Self {
        self.data.en_passant = Some(en_passant_square);
        self
    }

    /// Sets castling rights for the specified side and direction.
    ///
    /// # Arguments
    ///
    /// * `side` - A `Side` enum representing the side (white or black) whose
    ///   castling rights to set.
    /// * `kingside` - A boolean indicating whether to set castling rights for
    ///   the king side or queen side.
    /// * `allowed` - A boolean indicating whether castling is allowed for the
    ///   specified side and direction.
    #[must_use]
    pub fn castling(mut self, side: Side, kingside: bool, allowed: bool) -> Self {
        let i = if kingside { 0 } else { 1 };
        self.data.castling[side as usize][i] = allowed;
        self
    }

    /// Sets the halftime.
    ///
    /// Halftime is how many moves have been made since the last capture or pawn
    /// push. If the halftime exceeds 50, the game automatically ends in a draw.
    ///
    /// # Arguments
    ///
    /// * `halftime` - How many moves have been made since the last capture or
    ///   pawn push.
    #[must_use]
    pub fn halftime(mut self, halftime: u32) -> Self {
        self.data.halftime = halftime;
        self
    }

    /// Sets the fulltime.
    ///
    /// The fulltime is a measure of how long the game has been, measured in
    /// moves made by both sides since the start of the game.
    ///
    /// # Arguments
    ///
    /// * `fulltime` - The duration of the game measured in moves by both sides
    ///   since the start of the game.
    #[must_use]
    pub fn fulltime(mut self, fulltime: u32) -> Self {
        self.data.fulltime = fulltime;
        self
    }
}

impl Default for BoardBuilder {
    fn default() -> Self {
        Self::new().start_position()
    }
}
