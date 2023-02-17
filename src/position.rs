use crate::bitboards::constants::BITBOARD_ALL;
use crate::bitboards::Bitboard;
use crate::error::ParseFenError;
use crate::fen::FenString;
use crate::movegen;
use crate::piece::{Piece, PieceKind, Side, PIECE_KIND_COUNT, SIDE_COUNT};
use crate::square::{Square, BOARD_FILES, BOARD_SIZE};
use itertools::Itertools;
use lazy_static::lazy_static;
use std::hash::Hash;
use std::str::FromStr;
use std::{fmt, ops};

lazy_static! {
    static ref ZOBRIST_PIECE_NUMS: [u64; BOARD_SIZE * PIECE_KIND_COUNT * 2] =
        init_rand_nums::<u64, { BOARD_SIZE * PIECE_KIND_COUNT * 2 }>();
    static ref ZOBRIST_BLACK_MOVE: u64 = rand::random::<u64>();
    static ref ZOBRIST_CASTLING: [u64; 4] = init_rand_nums::<u64, 4>();
    static ref ZOBRIST_EN_PASSANT: [u64; BOARD_FILES] = init_rand_nums::<u64, BOARD_FILES>();
}

pub(crate) const EMPTY_POSITION: Position = Position {
    squares: [None; BOARD_SIZE],
    side_to_move: Side::White,
    castling: [[false; 2]; SIDE_COUNT],
    en_passant: None,
    halftime: 0,
    fulltime: 1,
};

#[derive(Clone, Copy, Debug, Eq)]
pub(crate) struct Position {
    pub squares: [Option<Piece>; BOARD_SIZE],
    pub side_to_move: Side,
    pub castling: [[bool; 2]; SIDE_COUNT],
    pub en_passant: Option<Square>,
    pub halftime: u32,
    pub fulltime: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

fn init_rand_nums<T, const SIZE: usize>() -> [T; SIZE]
where
    T: Default + Clone + Copy + From<u64>,
{
    let mut result = [T::default(); SIZE];

    for v in result.iter_mut() {
        *v = T::from(rand::random::<u64>());
    }

    result
}

impl Position {
    pub(crate) fn pieces(&self) -> Vec<(Piece, Square)> {
        self.squares
            .iter()
            .enumerate()
            .filter_map(|(i, &option)| Some((option?, Square::try_from(i).ok()?)))
            .collect()
    }

    pub(crate) fn bitboards(&self) -> PositionBitboards {
        PositionBitboards::new(self)
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

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.squares == other.squares
            && self.side_to_move == other.side_to_move
            && self.castling == other.castling
            && self.en_passant == other.en_passant
            && self.halftime == other.halftime
            && self.fulltime == other.fulltime
    }
}

impl Hash for Position {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut hash = 0;

        for (square, content) in self.squares.iter().enumerate() {
            if let Some(piece) = content {
                let piece_id = piece.kind as usize + piece.side as usize * PIECE_KIND_COUNT;
                let zobrist_id = square * (piece_id + 1);
                hash ^= ZOBRIST_PIECE_NUMS[zobrist_id];
            }
        }

        if self.side_to_move == Side::Black {
            hash ^= *ZOBRIST_BLACK_MOVE;
        }

        for (i, castling) in self.castling.iter().flatten().enumerate() {
            if *castling {
                hash ^= ZOBRIST_CASTLING[i];
            }
        }

        if let Some(square) = self.en_passant {
            let (_, file) = square.to_rank_file();
            hash ^= ZOBRIST_EN_PASSANT[file];
        }

        hash.hash(state);
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

impl PositionBitboards {
    #[allow(clippy::enum_glob_use)]
    pub fn new(position: &Position) -> Self {
        use crate::piece::PieceKind::*;

        let mut pb = Self::default();

        let side = position.side_to_move;
        let friendly = position.side_to_move as usize;
        let hostile = !position.side_to_move as usize;

        // Fill piece bitboards
        for (square, content) in position.squares.iter().enumerate() {
            if let Some(piece) = content {
                pb.pieces[piece.side as usize][piece.kind as usize].on(square);
                pb.sides[piece.side as usize].on(square);
                pb.occupied.on(square);
            }
        }

        let king_square = pb.pieces[friendly][King as usize]
            .lsb_square()
            .expect("Cannot generate moves without a king on the board.");

        // Find attacked squares
        for side in [Side::White, Side::Black] {
            let friendly = side as usize;
            let hostile = !side as usize;

            for (i, mut pieces) in pb.pieces[hostile].into_iter().enumerate() {
                let kind = PieceKind::try_from(i).unwrap();

                while let Some(from) = pieces.pop_lsb_square() {
                    let attack = movegen::get_attacks_bitboard(kind, !side, from, pb.occupied);

                    pb.attacked_by_2[hostile] |= pb.attacked[hostile] & attack;
                    pb.attacked_by[hostile][i] |= attack;
                    pb.attacked[hostile] |= attack;

                    pb.king_danger_squares[friendly] |= movegen::get_attacks_bitboard(
                        kind,
                        !side,
                        from,
                        pb.occupied & !pb.pieces[friendly][King as usize],
                    );
                }
            }
        }

        // Find checkers and generate checkmask
        let in_check = pb.attacked[hostile].get(king_square);
        if in_check {
            pb.checkmask =
                Self::generate_checkmask(king_square, side, pb.pieces[hostile], pb.occupied);
            pb.checkers = pb.sides[hostile] & pb.checkmask;
        }

        // Find pins and generate pinmask
        for side in [Side::White, Side::Black] {
            let friendly = side as usize;
            let hostile = !side as usize;

            let hv_pieces = (pb.pieces[hostile][Rook as usize]
                | pb.pieces[hostile][Queen as usize])
                & !pb.checkers;
            pb.pinmask_hv[friendly] = Self::find_pins(
                Rook,
                side,
                king_square,
                hv_pieces,
                pb.sides[friendly],
                pb.occupied,
            );

            let d12_pieces = (pb.pieces[hostile][Bishop as usize]
                | pb.pieces[hostile][Queen as usize])
                & !pb.checkers;
            pb.pinmask_d12[friendly] = Self::find_pins(
                Bishop,
                side,
                king_square,
                d12_pieces,
                pb.sides[friendly],
                pb.occupied,
            );
        }

        pb
    }

    pub fn count_checkers(&self) -> usize {
        self.checkers.pop_count()
    }

    fn generate_checkmask(
        king_square: Square,
        side: Side,
        hostile: [Bitboard; PIECE_KIND_COUNT],
        occupied: Bitboard,
    ) -> Bitboard {
        let mut checkmask = Bitboard(0);

        for (i, pieces) in hostile.into_iter().enumerate() {
            let kind = PieceKind::try_from(i).unwrap();
            checkmask |= movegen::find_attacks_on_square(kind, side, king_square, pieces, occupied);
        }

        checkmask
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

impl Default for PositionBitboards {
    fn default() -> Self {
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
}
