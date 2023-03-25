use crate::bitboards::constants::BITBOARD_ALL;
use crate::bitboards::Bitboard;
use crate::error::ParseFenError;

use crate::fen::FenString;
use crate::movegen;
use crate::piece::{Piece, PieceKind, Side, PIECE_KIND_COUNT, SIDE_COUNT};
use crate::r#move::{Move, MoveKind};
use crate::square::{Square, BOARD_FILES, BOARD_SIZE};
use itertools::Itertools;
use std::str::FromStr;
use std::{fmt, ops};

#[allow(clippy::all, clippy::pedantic, clippy::nursery)]
mod pregenerated {
    include!(concat!(env!("OUT_DIR"), "/generated_zobrist_hashes.rs"));
}
use pregenerated::{ZOBRIST_BLACK_MOVE, ZOBRIST_CASTLING, ZOBRIST_EN_PASSANT, ZOBRIST_PIECE_NUMS};

#[derive(Clone, Copy, Eq)]
pub(crate) struct Position {
    pub data: PositionData,
    pub bb: PositionBitboards,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct PositionData {
    pub squares: [Option<Piece>; BOARD_SIZE],
    pub side_to_move: Side,
    pub castling: [[bool; 2]; SIDE_COUNT],
    pub en_passant: Option<Square>,
    pub halftime: u32,
    pub fulltime: u32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct PositionBitboards {
    pub pieces: [[Bitboard; PIECE_KIND_COUNT]; SIDE_COUNT],
    pub sides: [Bitboard; SIDE_COUNT],
    pub occupied: Bitboard,

    pub checkmask: Bitboard,
    pub checkers: Bitboard,
    pub pinmask_hv: [Bitboard; SIDE_COUNT],
    pub pinmask_d12: [Bitboard; SIDE_COUNT],
    pub attacked: [Bitboard; SIDE_COUNT],
    pub attacked_by: [[Bitboard; PIECE_KIND_COUNT]; SIDE_COUNT],
    pub attacked_by_2: [Bitboard; SIDE_COUNT],
    pub king_danger_squares: [Bitboard; SIDE_COUNT],
}

impl Position {
    pub fn from_fen(fen: &str) -> Result<Self, ParseFenError> {
        let data = PositionData::from_str(fen)?;
        let bb = PositionBitboards::new(&data);

        Ok(Self { data, bb })
    }

    #[inline]
    pub fn generate_moves(&self) -> Vec<Move> {
        movegen::generate_legal_moves(self)
    }

    #[inline]
    pub fn pieces(&self) -> Vec<(Square, Piece)> {
        self.data.pieces()
    }

    #[inline]
    pub fn squares(&self) -> [(Square, Option<Piece>); 64] {
        self.data.squares()
    }

    #[inline]
    pub fn in_check(&self) -> bool {
        self.bb.checkers != 0
    }

    #[inline]
    pub fn side_to_move(&self) -> Side {
        self.data.side_to_move
    }

    pub fn make_move(&self, m: Move) -> Self {
        let mut next = *self;

        next.data.make_move(m);
        next.bb = PositionBitboards::new(&next.data);

        next
    }

    pub fn make_null_move(&self) -> Self {
        let mut next = *self;

        next.data.side_to_move = !next.data.side_to_move;
        next.bb = PositionBitboards::new(&next.data);

        next
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.data.squares == other.data.squares
            && self.data.side_to_move == other.data.side_to_move
            && self.data.castling == other.data.castling
            && self.data.en_passant == other.data.en_passant
    }
}

impl std::hash::Hash for Position {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut hash = 0;

        for (square, piece) in self.pieces() {
            let piece_id = piece.kind as usize + piece.side as usize * PIECE_KIND_COUNT;
            let zobrist_id = square as usize * (piece_id + 1);
            hash ^= ZOBRIST_PIECE_NUMS[zobrist_id];
        }

        if self.data.side_to_move == Side::Black {
            hash ^= ZOBRIST_BLACK_MOVE;
        }

        for (i, castling) in self.data.castling.iter().flatten().enumerate() {
            if *castling {
                hash ^= ZOBRIST_CASTLING[i];
            }
        }

        if let Some(square) = self.data.en_passant {
            let (_, file) = square.to_rank_file();
            hash ^= ZOBRIST_EN_PASSANT[file];
        }

        hash.hash(state);
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl ops::Index<Square> for Position {
    type Output = Option<Piece>;

    fn index(&self, index: Square) -> &Self::Output {
        &self.data[index]
    }
}

impl PositionData {
    #[inline]
    fn pieces(&self) -> Vec<(Square, Piece)> {
        self.squares()
            .into_iter()
            .filter_map(|(square, piece)| Some((square, piece?)))
            .collect()
    }

    #[inline]
    fn squares(&self) -> [(Square, Option<Piece>); 64] {
        let mut result = [(Square::A1, None); 64];

        for (i, piece) in self.squares.into_iter().enumerate() {
            let square = Square::try_from(i).unwrap();
            result[i] = (square, piece);
        }

        result
    }

    fn make_move(&mut self, m: Move) {
        const ROOK_SOURCES: [[Square; 2]; SIDE_COUNT] =
            [[Square::H1, Square::A1], [Square::H8, Square::A8]];
        const ROOK_DESTS: [[Square; 2]; SIDE_COUNT] =
            [[Square::F1, Square::D1], [Square::F8, Square::D8]];

        let side = self.side_to_move;
        let friendly = side as usize;
        let hostile = !side as usize;
        let to_move = self[m.from].expect("Legal moves should always start from square with piece");

        // Clear the en passant
        self.en_passant = None;

        // Do the move
        self[m.to] = self[m.from];
        self[m.from] = None;

        if self.side_to_move == Side::Black {
            self.halftime += 1;
            self.fulltime += 1;
        }

        // Check for any special conditions with the move
        match m.kind {
            MoveKind::EnPassant => {
                assert_eq!(to_move.kind, PieceKind::Pawn);

                let capture_square =
                    m.to.neighbour(side.backward())
                        .expect("Invalid en-passant target square");
                self[capture_square] = None;
            }
            MoveKind::Capture => self.halftime = 0,
            MoveKind::Promotion(into) => {
                assert!(to_move.kind == PieceKind::Pawn);

                if let Some(ref mut piece) = self[m.to] {
                    piece.kind = into;
                }
            }
            MoveKind::Castling => {
                let side = m.to.side() as usize;

                assert!(self.castling[friendly][side]);

                let src = ROOK_SOURCES[friendly][side];
                let dest = ROOK_DESTS[friendly][side];

                assert_eq!(
                    self[src],
                    Some(Piece::new(self.side_to_move, PieceKind::Rook))
                );
                assert_eq!(self[dest], None);

                self[dest] = self[src];
                self[src] = None;
            }
            MoveKind::Quiet => (),
            MoveKind::Any => unreachable!("No move of kind \"Any\" should ever be used."),
        }

        if to_move.kind == PieceKind::Pawn {
            const EP_DISTANCE: f64 = 2.0;

            self.halftime = 0;

            if m.from.distance(m.to) >= EP_DISTANCE {
                let ep_index = (m.from as usize + m.to as usize) / 2;
                self.en_passant = Some(Square::try_from(ep_index).unwrap());
            }
        } else if to_move.kind == PieceKind::King {
            self.castling[friendly] = [false, false];
        }

        if ROOK_SOURCES[friendly].contains(&m.from) || ROOK_SOURCES[hostile].contains(&m.to) {
            self.castling[friendly][0] &=
                self[ROOK_SOURCES[friendly][0]] == Some(Piece::new(side, PieceKind::Rook));
            self.castling[friendly][1] &=
                self[ROOK_SOURCES[friendly][1]] == Some(Piece::new(side, PieceKind::Rook));

            self.castling[hostile][0] &=
                self[ROOK_SOURCES[hostile][0]] == Some(Piece::new(!side, PieceKind::Rook));
            self.castling[hostile][1] &=
                self[ROOK_SOURCES[hostile][1]] == Some(Piece::new(!side, PieceKind::Rook));
        }

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
                self.castling[side][i] &= self[ROOK_SOURCES[side][i]] == Some(rook);
            }
        }

        self.side_to_move = !self.side_to_move;
    }
}

impl ops::Index<Square> for PositionData {
    type Output = Option<Piece>;

    fn index(&self, index: Square) -> &Self::Output {
        &self.squares[index as usize]
    }
}

impl ops::IndexMut<Square> for PositionData {
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self.squares[index as usize]
    }
}

impl FromStr for PositionData {
    type Err = ParseFenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(ParseFenError::Empty);
        }

        let fen = FenString::try_from(s)?;

        let mut squares = [None; BOARD_SIZE];

        let mut kings = [false, false];
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

                if let Some(Piece {
                    kind: PieceKind::King,
                    side,
                }) = squares[i]
                {
                    kings[side as usize] = true;
                }

                i += 1;
            }
        }

        if i != BOARD_SIZE {
            return Err(ParseFenError::InvalidBoardSize);
        }

        if !kings[0] || !kings[1] {
            return Err(ParseFenError::MissingKing);
        }

        let side_to_move = Side::from(fen.side_to_move);

        let mut castling = [[false; 2]; SIDE_COUNT];
        for p in fen
            .castling_ability
            .chars()
            .filter_map(|c| Piece::try_from(c).ok())
            .filter(|piece| piece.kind == PieceKind::King || piece.kind == PieceKind::Queen)
        {
            let rank = if p.side == Side::White { 0 } else { 7 } * BOARD_FILES;
            let file = if p.kind == PieceKind::King { 7 } else { 0 };

            if squares[rank + Square::E1 as usize]
                != Some(Piece {
                    kind: PieceKind::King,
                    side: p.side,
                })
                || squares[rank + file]
                    != Some(Piece {
                        kind: PieceKind::Rook,
                        side: p.side,
                    })
            {
                return Err(ParseFenError::IllegalCastling);
            }

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

impl fmt::Display for PositionData {
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

impl PositionBitboards {
    pub fn new(data: &PositionData) -> Self {
        let mut result = Self::empty();
        let side = data.side_to_move;

        for (square, content) in data.squares.iter().enumerate() {
            if let Some(piece) = content {
                result.pieces[piece.side as usize][piece.kind as usize].on(square);
                result.sides[piece.side as usize].on(square);
                result.occupied.on(square);
            }
        }

        result.generate_attacks(side);
        result.generate_attacks(!side);

        let king_square = result.king_square(side);
        result.generate_checkers(side, king_square);

        result.generate_pins(side, king_square);
        result.generate_pins(!side, king_square);

        result
    }

    fn empty() -> Self {
        Self {
            pieces: [[Bitboard(0); PIECE_KIND_COUNT]; SIDE_COUNT],
            sides: [Bitboard(0); SIDE_COUNT],
            occupied: Bitboard(0),

            checkmask: BITBOARD_ALL,
            checkers: Bitboard(0),
            pinmask_hv: [Bitboard(0); SIDE_COUNT],
            pinmask_d12: [Bitboard(0); SIDE_COUNT],
            attacked: [Bitboard(0); SIDE_COUNT],
            attacked_by: [[Bitboard(0); PIECE_KIND_COUNT]; SIDE_COUNT],
            attacked_by_2: [Bitboard(0); SIDE_COUNT],
            king_danger_squares: [Bitboard(0); SIDE_COUNT],
        }
    }

    fn king_square(&self, side: Side) -> Square {
        use crate::piece::PieceKind::King;

        self.pieces[side as usize][King as usize]
            .lsb_square()
            .expect("There should always be a king on the board")
    }

    fn generate_attacks(&mut self, side: Side) {
        use crate::piece::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};

        let friendly = side as usize;
        let hostile = !side as usize;

        self.attacked_by_2[hostile] = Bitboard(0);
        self.attacked_by[hostile] = [Bitboard(0); PIECE_KIND_COUNT];
        self.attacked[hostile] = Bitboard(0);
        self.king_danger_squares[friendly] = Bitboard(0);

        for kind in [Pawn, Rook, Knight, Bishop, Queen, King] {
            let mut pieces = self.pieces[hostile][kind as usize];

            while let Some(from) = pieces.pop_lsb_square() {
                let attack = movegen::get_attacks_bitboard(kind, !side, from, self.occupied);

                self.attacked_by_2[hostile] |= self.attacked[hostile] & attack;
                self.attacked_by[hostile][kind as usize] |= attack;
                self.attacked[hostile] |= attack;

                self.king_danger_squares[friendly] |= movegen::get_attacks_bitboard(
                    kind,
                    !side,
                    from,
                    self.occupied & !self.pieces[friendly][King as usize],
                );
            }
        }
    }

    fn generate_checkers(&mut self, side: Side, king_square: Square) {
        use crate::piece::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};

        let hostile = !side as usize;

        self.checkmask = BITBOARD_ALL;
        self.checkers = Bitboard(0);

        let in_check = self.attacked[hostile].get(king_square);
        if !in_check {
            return;
        }

        self.checkmask = Bitboard(0);

        for kind in [Pawn, Rook, Knight, Bishop, Queen, King] {
            let pieces = self.pieces[hostile][kind as usize];
            self.checkmask |=
                movegen::find_attacks_on_square(kind, side, king_square, pieces, self.occupied);
        }

        self.checkers = self.sides[hostile] & self.checkmask;
    }

    fn generate_pins(&mut self, side: Side, king_square: Square) {
        use crate::piece::PieceKind::{Bishop, Queen, Rook};

        let friendly = side as usize;
        let hostile = !side as usize;

        let hv_pieces = (self.pieces[hostile][Rook as usize]
            | self.pieces[hostile][Queen as usize])
            & !self.checkers;
        self.pinmask_hv[friendly] = Self::find_pins(
            PieceKind::Rook,
            side,
            king_square,
            hv_pieces,
            self.sides[friendly],
            self.occupied,
        );

        let d12_pieces = (self.pieces[hostile][Bishop as usize]
            | self.pieces[hostile][Queen as usize])
            & !self.checkers;
        self.pinmask_d12[friendly] = Self::find_pins(
            PieceKind::Bishop,
            side,
            king_square,
            d12_pieces,
            self.sides[friendly],
            self.occupied,
        );
    }

    fn find_pins(
        kind: PieceKind,
        side: Side,
        king_square: Square,
        attackers: Bitboard,
        defenders: Bitboard,
        occupied: Bitboard,
    ) -> Bitboard {
        let from_king = movegen::get_attacks_bitboard(kind, side, king_square, occupied);

        let mut occupied = occupied;
        let mut pred = attackers;
        while let Some(attacker) = pred.pop_lsb_square() {
            let from_attacker = movegen::get_attacks_bitboard(kind, !side, attacker, occupied);
            let overlap = from_king & from_attacker;
            let pinned = overlap & defenders;
            occupied &= !pinned;
        }

        movegen::find_attacks_on_square(kind, side, king_square, attackers, occupied)
    }
}
