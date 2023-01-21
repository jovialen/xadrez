#[allow(clippy::wildcard_imports)]
use crate::{
    bitboards::{constants::*, Bitboard},
    board::{Direction, Position, Square, BOARD_FILES, BOARD_SIZE},
    piece::{PieceKind, Side, PIECE_KIND_COUNT, SIDE_COUNT},
    r#move::{Move, MoveKind},
};
use lazy_static::lazy_static;

// Array for all types of piece moves, except for the pawn.
type PieceMoveBitboards = [[Bitboard; BOARD_SIZE]; PIECE_KIND_COUNT - 1];

// Pawns move differently depending on what side they're on, and whether they're
// attacking or just moving.
type PawnMoveBitboards = [[(Bitboard, Bitboard); BOARD_SIZE]; SIDE_COUNT];

lazy_static! {
    static ref PSEUDO_ATTACKS: PieceMoveBitboards = init_pseudo_attacks();
    static ref PAWN_MOVES: PawnMoveBitboards = init_pawn_moves();
    static ref BISHOP_MOVES: MagicTable<512> =
        MagicTable::new(&BISHOP_DIRECTIONS, &BISHOP_MAGICS, &BISHOP_BITS);
    static ref ROOK_MOVES: MagicTable<4096> =
        MagicTable::new(&ROOK_DIRECTIONS, &ROOK_MAGICS, &ROOK_BITS);
}

const ROOK_DIRECTIONS: [Direction; 4] = [
    Direction::North,
    Direction::East,
    Direction::South,
    Direction::West,
];

const BISHOP_DIRECTIONS: [Direction; 4] = [
    Direction::NorthEast,
    Direction::NorthWest,
    Direction::SouthEast,
    Direction::SouthWest,
];

// Relevant bits for the rook magics.
const ROOK_BITS: [usize; BOARD_SIZE] = [
    12, 11, 11, 11, 11, 11, 11, 12, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 12, 11, 11, 11, 11, 11, 11, 12,
];

// Relevant bits for the bishop magics.
const BISHOP_BITS: [usize; BOARD_SIZE] = [
    6, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 6,
];

// Pre-generated valid magic numbers for the rook.
// Source: https://github.com/maksimKorzh/chess_programming/blob/master/src/magics/magics.c
const ROOK_MAGICS: [u64; BOARD_SIZE] = [
    0x0a80_02c0_0010_8020,
    0x06c0_0049_b000_2001,
    0x0100_2000_1009_0040,
    0x2480_0410_0080_0801,
    0x0280_0280_0400_0800,
    0x0900_4100_0804_0022,
    0x0280_0200_0100_1080,
    0x2880_0020_4100_0080,
    0xa000_8000_8040_0034,
    0x0004_8080_2000_4000,
    0x2290_8020_0480_1000,
    0x0411_000d_0010_0020,
    0x0402_8008_0004_0080,
    0x000b_0004_0100_4208,
    0x2409_0001_0004_0200,
    0x0001_0021_0000_4082,
    0x0022_8780_01e2_4000,
    0x1090_8100_2100_4010,
    0x0801_0300_4020_0012,
    0x0500_8080_0800_1000,
    0x0a08_0180_1400_0880,
    0x8000_8080_0400_0200,
    0x0201_0080_8001_0200,
    0x0801_0200_0044_1091,
    0x8000_8020_4005,
    0x1040_2000_4010_0048,
    0x1202_0040_2082,
    0x0d14_8804_8010_0080,
    0x0012_0402_8008_0080,
    0x0100_0400_8002_0080,
    0x9020_0100_8080_0200,
    0x0813_2412_0014_8449,
    0x0491_6040_0180_0080,
    0x0100_4010_0040_2001,
    0x4820_0100_2100_1040,
    0x0400_4022_0200_0812,
    0x0209_0090_0500_0802,
    0x0810_8006_0180_0400,
    0x4301_0832_1400_0150,
    0x2040_2645_8e00_1401,
    0x0040_2040_0080_8000,
    0x8001_0080_4001_0020,
    0x8410_8208_2042_0010,
    0x1003_0010_0009_0020,
    0x0804_0400_0800_8080,
    0x0012_0008_1002_0004,
    0x1000_1002_0004_0208,
    0x4300_00a0_4402_0001,
    0x0280_0090_2341_0300,
    0x00e0_1000_4000_2240,
    0x2001_0040_1700,
    0x2244_1004_0800_8080,
    0x0008_0004_0080_1980,
    0x0002_0008_1004_0200,
    0x8010_1002_2881_0400,
    0x2000_0090_4421_0200,
    0x4080_0080_4010_2101,
    0x0040_0020_8041_1d01,
    0x2005_5240_6000_0901,
    0x0502_0010_0840_0422,
    0x489a_0008_1020_0402,
    0x0001_0044_0008_0a13,
    0x4000_0110_0802_0084,
    0x0026_0021_1405_8042,
];

// Pre-generated valid magic numbers for the bishop.
// Source: https://github.com/maksimKorzh/chess_programming/blob/master/src/magics/magics.c
const BISHOP_MAGICS: [u64; BOARD_SIZE] = [
    0x89a1_1218_9604_0240,
    0x2004_8448_0200_2010,
    0x2068_0800_5192_1000,
    0x6288_0a02_2020_0808,
    0x0004_0420_0400_0000,
    0x0100_8220_2020_0011,
    0xc004_4422_2012_000a,
    0x0028_8088_0121_6001,
    0x0400_4920_8840_8100,
    0x0201_c401_040c_0084,
    0x0084_0800_910a_0010,
    0x0820_8024_0060,
    0x2000_8405_0400_6000,
    0x3001_0c41_0840_5004,
    0x1008_0054_1008_0802,
    0x8144_0422_0910_0900,
    0x0208_0810_2001_4400,
    0x0048_0020_1208_ca00,
    0x0f18_1404_0801_2008,
    0x1004_0028_0210_2001,
    0x0841_0008_2008_0811,
    0x0040_2002_00a4_2008,
    0x8000_5404_2000,
    0x8801_0400_410c_9000,
    0x0520_0404_7010_4290,
    0x1004_0400_5150_0081,
    0x2002_0818_3308_0021,
    0x0004_00c0_0c01_0142,
    0x9414_0820_0c00_2000,
    0x0658_8100_0080_6011,
    0x0188_0710_4044_0a00,
    0x4800_4040_0201_1c00,
    0x0104_4420_4040_4200,
    0x0511_0802_0209_1021,
    0x0004_0224_0112_0400,
    0x80c0_0404_0008_0120,
    0x8040_0100_4082_0802,
    0x0480_8107_0002_0090,
    0x0102_008e_0004_0242,
    0x0809_0052_0205_0100,
    0x8002_0242_2010_4080,
    0x0431_0088_0414_2000,
    0x0019_0018_0208_1400,
    0x0200_0142_0804_0080,
    0x3308_0820_0820_0100,
    0x0410_1050_0040_c020,
    0x4012_020c_0421_0308,
    0x2082_20a2_0200_4080,
    0x0111_0401_2008_2000,
    0x6803_0401_4128_0a00,
    0x2101_0042_0241_0000,
    0x8200_0000_4110_8022,
    0x0210_8208_8000,
    0x0002_4102_0401_0040,
    0x0040_1004_0080_9000,
    0x0822_0882_2082_0214,
    0x0040_8080_9001_2004,
    0x0091_0224_0402_18c9,
    0x0402_8144_2201_5008,
    0x0090_0140_0484_2410,
    0x0001_0000_4230_4105,
    0x0010_0088_3041_2a00,
    0x2520_0810_9000_8908,
    0x4010_2000_a0a6_0140,
];

#[derive(Default)]
struct PositionBitboards {
    pieces: [[Bitboard; PIECE_KIND_COUNT]; SIDE_COUNT],
    sides: [Bitboard; SIDE_COUNT],
    occupied: Bitboard,

    checkmask: Bitboard,
    checkers: Bitboard,
    attacks: Bitboard,
    king_danger_squares: Bitboard,
}

// This project uses the fancy magics approach to find sliding moves. It's the
// most common method, notably used by Stockfish.
// https://www.chessprogramming.org/Magic_Bitboards#Fancy
struct MagicTable<const SIZE: usize> {
    magics: [Magic; BOARD_SIZE],
    table: Vec<[Bitboard; SIZE]>,
}

#[derive(Clone, Copy, Default)]
struct Magic {
    magic: u64,
    mask: Bitboard,
    shift: usize,
}

pub(crate) fn generate_legal_moves(position: &Position) -> Vec<Move> {
    let pb = PositionBitboards::new(position);
    let mut result = Vec::new();

    if pb.checkers.pop_count() < 2 {
        generate_moves(PieceKind::Queen, &pb, position.side_to_move, &mut result);
        generate_moves(PieceKind::Bishop, &pb, position.side_to_move, &mut result);
        generate_moves(PieceKind::Knight, &pb, position.side_to_move, &mut result);
        generate_moves(PieceKind::Rook, &pb, position.side_to_move, &mut result);
        generate_pawn_moves(&pb, position.side_to_move, position.en_passant, &mut result);
    }

    let mut kings = pb.pieces[position.side_to_move as usize][PieceKind::King as usize];
    while let Some(square) = kings.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(square).unwrap();
        let moves = get_attacks_bitboard(PieceKind::King, position.side_to_move, from, pb.occupied)
            & !pb.king_danger_squares;

        let quiets = moves & !pb.occupied;
        let captures = moves & pb.sides[!position.side_to_move as usize];

        push_moves(MoveKind::Quiet, quiets, from, &mut result);
        push_moves(MoveKind::Capture, captures, from, &mut result);
    }

    result
}

fn generate_moves(
    kind: PieceKind,
    bitboards: &PositionBitboards,
    side: Side,
    dest: &mut Vec<Move>,
) {
    let kind_index = kind as usize;
    let friendly = side as usize;
    let hostile = !side as usize;
    let mut pieces = bitboards.pieces[friendly][kind_index];

    while let Some(square) = pieces.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(square).unwrap();
        let pseudo_attacks = get_attacks_bitboard(kind, side, from, bitboards.occupied);
        let moves = pseudo_attacks & bitboards.checkmask;

        let quiets = moves & !bitboards.occupied;
        let captures = moves & bitboards.sides[hostile];

        push_moves(MoveKind::Quiet, quiets, from, dest);
        push_moves(MoveKind::Capture, captures, from, dest);
    }
}

fn generate_pawn_moves(
    bitboards: &PositionBitboards,
    side: Side,
    ep: Option<Square>,
    dest: &mut Vec<Move>,
) {
    const PAWN: usize = PieceKind::Pawn as usize;
    const PROMOTION_RANK: [Bitboard; SIDE_COUNT] = [BITBOARD_RANK_7, BITBOARD_RANK_2];
    const DOUBLE_RANK: [Bitboard; SIDE_COUNT] = [BITBOARD_RANK_2, BITBOARD_RANK_7];

    let friendly = side as usize;
    let hostile = !side as usize;

    let ep_square = Bitboard(if let Some(offset) = ep {
        0b1 << offset as usize
    } else {
        0
    });

    let mut doubles = bitboards.pieces[friendly][PAWN] & DOUBLE_RANK[friendly];
    let mut promotions = bitboards.pieces[friendly][PAWN] & PROMOTION_RANK[friendly];
    let mut normal = bitboards.pieces[friendly][PAWN] & !promotions;

    while let Some(pawn) = normal.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(pawn).unwrap();
        let (moves, attacks) = PAWN_MOVES[friendly][pawn];

        let quiets = moves & !bitboards.occupied & bitboards.checkmask;
        let captures = attacks & bitboards.sides[hostile] & bitboards.checkmask;
        let ep_capture = attacks & ep_square & bitboards.checkmask;

        push_moves(MoveKind::Quiet, quiets, from, dest);
        push_moves(MoveKind::Capture, captures, from, dest);
        push_moves(MoveKind::EnPassant, ep_capture, from, dest);
    }

    while let Some(doubles) = doubles.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(doubles).unwrap();

        // Safety: There is always one bit set in the moves bitboard.
        let jump = PAWN_MOVES[friendly][doubles].0.lsb().unwrap();
        let to = PAWN_MOVES[friendly][jump].0.lsb().unwrap();

        if !bitboards.occupied.get(jump)
            && !bitboards.occupied.get(to)
            && Bitboard(0b1 << to) & bitboards.checkmask != 0
        {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();
            dest.push(Move::new(from, to, MoveKind::PawnPush));
        }
    }

    while let Some(pawn) = promotions.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(pawn).unwrap();
        let (moves, attacks) = PAWN_MOVES[friendly][pawn];

        let quiets = moves & !bitboards.occupied;
        let captures = attacks & bitboards.sides[hostile];
        let mut all = (quiets | captures) & bitboards.checkmask;

        while let Some(to) = all.pop_lsb() {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();

            dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Queen)));
            dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Bishop)));
            dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Rook)));
            dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Knight)));
        }
    }
}

fn get_attacks_bitboard(
    kind: PieceKind,
    side: Side,
    square: Square,
    occupied: Bitboard,
) -> Bitboard {
    match kind {
        PieceKind::Bishop => BISHOP_MOVES.get(square, occupied),
        PieceKind::Rook => ROOK_MOVES.get(square, occupied),
        PieceKind::Queen => BISHOP_MOVES.get(square, occupied) | ROOK_MOVES.get(square, occupied),
        PieceKind::Pawn => PAWN_MOVES[side as usize][square as usize].1,
        _ => PSEUDO_ATTACKS[kind as usize][square as usize],
    }
}

fn push_moves(kind: MoveKind, mut bb: Bitboard, from: Square, dest: &mut Vec<Move>) {
    while let Some(square) = bb.pop_lsb() {
        // Safety: Always in range 0..64
        let to = Square::try_from(square).unwrap();
        dest.push(Move::new(from, to, kind));
    }
}

#[allow(clippy::cast_possible_wrap)]
fn init_pseudo_attacks() -> PieceMoveBitboards {
    let mut moves = [[Bitboard(0); BOARD_SIZE]; 5];

    for i in 0..64 {
        // Safety: 0..64 should always be in range.
        let from = Square::try_from(i).unwrap();

        let king_offsets = [1, -1, 8, -8, 9, -9, 7, -7];
        for offset in king_offsets {
            let dest_i = i as isize + offset;
            if let Ok(dest) = Square::try_from(dest_i) {
                if from.distance(dest) <= 2.0 {
                    moves[PieceKind::King as usize][i].on(dest);
                }
            }
        }

        let knight_offsets = [-17, -15, -10, -6, 6, 10, 15, 17];
        for offset in knight_offsets {
            let dest_i = i as isize + offset;
            if let Ok(dest) = Square::try_from(dest_i) {
                if from.distance(dest) <= 3.0 {
                    moves[PieceKind::Knight as usize][i].on(dest);
                }
            }
        }

        moves[PieceKind::Rook as usize][i] = seek_sliding(&ROOK_DIRECTIONS, from, Bitboard(0));
        moves[PieceKind::Bishop as usize][i] = seek_sliding(&BISHOP_DIRECTIONS, from, Bitboard(0));

        moves[PieceKind::Queen as usize][i] |= moves[PieceKind::Rook as usize][i];
        moves[PieceKind::Queen as usize][i] |= moves[PieceKind::Bishop as usize][i];
    }

    moves
}

// TODO: Refactor this monstrosity
#[allow(clippy::cast_possible_wrap)]
fn init_pawn_moves() -> PawnMoveBitboards {
    let mut moves = [[(Bitboard(0), Bitboard(0)); BOARD_SIZE]; SIDE_COUNT];

    // Iterate through the entire board except the last rank.
    // Pawns get promoted on the last rank, so no pawn moves ever occur there.
    for white_i in 0..(BOARD_SIZE - BOARD_FILES) {
        // Safety: 0..64 should always be in range.
        let white_from = Square::try_from(white_i).unwrap();

        // The black pawns are calculated with the inverse of the white pawns
        let black_i = BOARD_SIZE - white_i - 1;
        let black_from = Square::try_from(black_i).unwrap();

        moves[Side::White as usize][white_i]
            .0
            .on(white_i + BOARD_FILES);
        moves[Side::Black as usize][black_i]
            .0
            .on(black_i - BOARD_FILES);

        // Captures
        for offset in [9, 7] {
            let white_dest_i = white_i as isize + offset;
            let black_dest_i = black_i as isize - offset;

            if let Ok(dest) = Square::try_from(white_dest_i) {
                if white_from.distance(dest) <= 2.0 {
                    moves[Side::White as usize][white_i].1.on(dest);
                }
            }

            if let Ok(dest) = Square::try_from(black_dest_i) {
                if black_from.distance(dest) <= 2.0 {
                    moves[Side::Black as usize][black_i].1.on(dest);
                }
            }
        }
    }

    moves
}

fn seek_sliding(directions: &[Direction], origin: Square, blockers: Bitboard) -> Bitboard {
    let mut result = Bitboard(0);
    for &direction in directions {
        let mut current = origin;
        while let Some(next) = current.neighbour(direction) {
            result.on(next);

            if blockers.get(next) {
                break;
            }

            current = next;
        }
    }
    result
}

// Source: https://github.com/maksimKorzh/chess_programming/blob/master/src/magics/magics.c
fn set_occupancy(index: u64, bitcount: usize, mut mask: Bitboard) -> Bitboard {
    let mut result = Bitboard(0);
    for i in 0..bitcount {
        // Safety: bitcount should never be higher than the set bits in the mask
        let lsb = mask.pop_lsb().unwrap();

        if index & (1 << i) != 0 {
            result.on(lsb);
        }
    }
    result
}

impl PositionBitboards {
    fn new(position: &Position) -> Self {
        let mut pb = Self::default();

        let friendly = position.side_to_move as usize;
        let hostile = !position.side_to_move as usize;

        let king_square = position
            .pieces()
            .iter()
            .find(|(piece, _)| piece.kind == PieceKind::King && piece.side == position.side_to_move)
            .expect("There must be a king on the board to generate legal moves")
            .1;

        for (piece, square) in position.pieces() {
            pb.pieces[piece.side as usize][piece.kind as usize].on(square);
            pb.sides[piece.side as usize].on(square);
            pb.occupied.on(square);
        }

        for (i, mut pieces) in pb.pieces[hostile].into_iter().enumerate() {
            let kind = PieceKind::try_from(i).unwrap();

            while let Some(piece) = pieces.pop_lsb() {
                let from = Square::try_from(piece).unwrap();

                pb.attacks |= get_attacks_bitboard(kind, !position.side_to_move, from, pb.occupied)
                    & !pb.sides[hostile];

                pb.king_danger_squares |= get_attacks_bitboard(
                    kind,
                    !position.side_to_move,
                    from,
                    pb.occupied & !pb.pieces[friendly][PieceKind::King as usize],
                );
            }
        }

        let in_check = pb.pieces[friendly][PieceKind::King as usize] & pb.attacks != 0;
        if in_check {
            for (i, mut pieces) in pb.pieces[hostile].into_iter().enumerate() {
                let kind = PieceKind::try_from(i).unwrap();
                let from_king_square =
                    get_attacks_bitboard(kind, position.side_to_move, king_square, pb.occupied);

                pieces &= from_king_square;
                pb.checkers |= pieces;
                pb.checkmask |= pieces;

                while let Some(piece) = pieces.pop_lsb() {
                    let from = Square::try_from(piece).unwrap();
                    pb.checkmask |=
                        get_attacks_bitboard(kind, !position.side_to_move, from, pb.occupied)
                            & from_king_square;
                }
            }
        } else {
            pb.checkmask = BITBOARD_ALL;
        };

        pb
    }
}

impl<const T: usize> MagicTable<T> {
    fn new(
        directions: &[Direction],
        magics: &[u64; BOARD_SIZE],
        relevant_bits: &[usize; BOARD_SIZE],
    ) -> Self {
        let mut magic_table = MagicTable::default();

        for i in 0..BOARD_SIZE {
            // Safety: Always within 0..64
            let square = Square::try_from(i).unwrap();

            let edges = ((BITBOARD_RANK_1 | BITBOARD_RANK_8) & !square.rank())
                | ((BITBOARD_FILE_A | BITBOARD_FILE_H) & !square.file());

            let magic = &mut magic_table.magics[i];
            let table = &mut magic_table.table[i];

            magic.magic = magics[i];
            magic.mask = seek_sliding(directions, square, Bitboard(0)) & !edges;
            magic.shift = relevant_bits[i];

            let mask_size = magic.mask.0.count_ones() as usize;
            let variations = 1 << mask_size;

            for j in 0..variations {
                let occupancy = set_occupancy(j, mask_size, magic.mask);
                let magic_index = magic.index_of(occupancy);
                table[magic_index] = seek_sliding(directions, square, occupancy);
            }
        }

        magic_table
    }

    fn get(&self, origin: Square, occupied: Bitboard) -> Bitboard {
        self.table[origin as usize][self.index_of(origin, occupied)]
    }

    fn index_of(&self, origin: Square, occupied: Bitboard) -> usize {
        self.magics[origin as usize].index_of(occupied)
    }
}

impl Magic {
    #[allow(clippy::cast_possible_truncation)]
    fn index_of(&self, mut occupied: Bitboard) -> usize {
        occupied &= self.mask;
        occupied *= Bitboard(self.magic);
        occupied >>= BOARD_SIZE - self.shift;

        occupied.0 as usize
    }
}

impl<const T: usize> Default for MagicTable<T> {
    fn default() -> Self {
        Self {
            magics: [Magic::default(); BOARD_SIZE],
            table: vec![[Bitboard(0); T]; BOARD_SIZE],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod movegen {
        use crate::{board::Chessboard, fen::FEN_STARTING_POSITION};

        #[test]
        fn perft_test_startpos() {
            let mut chessboard = Chessboard::from_fen(FEN_STARTING_POSITION).unwrap();
            assert_eq!(chessboard.perft(1), 20);
            assert_eq!(chessboard.perft(2), 400);
        }

        #[test]
        fn perft_test_01() {
            let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 119_060_324);
        }

        #[test]
        fn perft_test_02() {
            let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 193_690_690);
        }

        #[test]
        fn perft_test_03() {
            let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(7), 178_633_661);
        }

        #[test]
        fn perft_test_04() {
            let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 706_045_033);
        }

        #[test]
        fn perft_test_05() {
            let fen = "1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 1_063_513);
        }

        #[test]
        fn perft_test_illegal_ep_1() {
            let fen = "3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1_134_888);
        }

        #[test]
        fn perft_test_illegal_ep_2() {
            let fen = "8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1_015_133);
        }

        #[test]
        fn perft_test_ep_capture_checks() {
            let fen = "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1_015_133);
        }

        #[test]
        fn perft_test_short_castling_check() {
            let fen = "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 661_072);
        }

        #[test]
        fn perft_test_long_castling_check() {
            let fen = "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 803_711);
        }

        #[test]
        fn perft_test_castle_rights() {
            let fen = "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4), 1_274_206);
        }

        #[test]
        fn perft_test_prevented_castle_rights() {
            let fen = "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4), 1_720_476);
        }

        #[test]
        fn perft_test_promote_out_of_check() {
            let fen = "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 3_821_001);
        }

        #[test]
        fn perft_test_discovered_check() {
            let fen = "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 1_004_658);
        }

        #[test]
        fn perft_test_promote_to_give_check() {
            let fen = "4k3/1P6/8/8/8/8/K7/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 217_342);
        }

        #[test]
        fn perft_test_under_promote_to_give_check() {
            let fen = "8/P1k5/K7/8/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 92683);
        }

        #[test]
        fn perft_test_self_stalemate() {
            let fen = "K1k5/8/P7/8/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 2217);
        }

        #[test]
        fn perft_test_stalemate_and_checkmate_1() {
            let fen = "8/k1P5/8/1K6/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(7), 567_584);
        }

        #[test]
        fn perft_test_stalemate_and_checkmate_2() {
            let fen = "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4), 23527);
        }
    }

    mod bitboard_generation {
        use crate::{
            fen::FEN_STARTING_POSITION,
            piece::{PieceKind, Side},
        };
        use std::str::FromStr;

        use super::*;

        #[test]
        fn startpos() {
            let pb = PositionBitboards::new(&Position::from_str(FEN_STARTING_POSITION).unwrap());

            assert_eq!(pb.occupied, Bitboard(0xFFFF_0000_0000_FFFF));
            assert_eq!(
                pb.sides[Side::White as usize],
                Bitboard(0x0000_0000_0000_FFFF)
            );
            assert_eq!(
                pb.sides[Side::Black as usize],
                Bitboard(0xFFFF_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Pawn as usize],
                Bitboard(0x0000_0000_0000_FF00)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Pawn as usize],
                Bitboard(0x00FF_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Rook as usize],
                Bitboard(0x0000_0000_0000_0081)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Rook as usize],
                Bitboard(0x8100_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Knight as usize],
                Bitboard(0x0000_0000_0000_0042)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Knight as usize],
                Bitboard(0x4200_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Bishop as usize],
                Bitboard(0x0000_0000_0000_0024)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Bishop as usize],
                Bitboard(0x2400_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Queen as usize],
                Bitboard(0x0000_0000_0000_0008)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Queen as usize],
                Bitboard(0x0800_0000_0000_0000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::King as usize],
                Bitboard(0x0000_0000_0000_0010)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::King as usize],
                Bitboard(0x1000_0000_0000_0000)
            );
        }
    }
}
