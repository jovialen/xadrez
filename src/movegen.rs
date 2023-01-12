use crate::{
    bitboards::{bitboard_constants::*, Bitboard},
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
    static ref PIECE_MOVES: PieceMoveBitboards = init_piece_moves();
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
    0xa8002c000108020,
    0x6c00049b0002001,
    0x100200010090040,
    0x2480041000800801,
    0x280028004000800,
    0x900410008040022,
    0x280020001001080,
    0x2880002041000080,
    0xa000800080400034,
    0x4808020004000,
    0x2290802004801000,
    0x411000d00100020,
    0x402800800040080,
    0xb000401004208,
    0x2409000100040200,
    0x1002100004082,
    0x22878001e24000,
    0x1090810021004010,
    0x801030040200012,
    0x500808008001000,
    0xa08018014000880,
    0x8000808004000200,
    0x201008080010200,
    0x801020000441091,
    0x800080204005,
    0x1040200040100048,
    0x120200402082,
    0xd14880480100080,
    0x12040280080080,
    0x100040080020080,
    0x9020010080800200,
    0x813241200148449,
    0x491604001800080,
    0x100401000402001,
    0x4820010021001040,
    0x400402202000812,
    0x209009005000802,
    0x810800601800400,
    0x4301083214000150,
    0x204026458e001401,
    0x40204000808000,
    0x8001008040010020,
    0x8410820820420010,
    0x1003001000090020,
    0x804040008008080,
    0x12000810020004,
    0x1000100200040208,
    0x430000a044020001,
    0x280009023410300,
    0xe0100040002240,
    0x200100401700,
    0x2244100408008080,
    0x8000400801980,
    0x2000810040200,
    0x8010100228810400,
    0x2000009044210200,
    0x4080008040102101,
    0x40002080411d01,
    0x2005524060000901,
    0x502001008400422,
    0x489a000810200402,
    0x1004400080a13,
    0x4000011008020084,
    0x26002114058042,
];

// Pre-generated valid magic numbers for the bishop.
// Source: https://github.com/maksimKorzh/chess_programming/blob/master/src/magics/magics.c
const BISHOP_MAGICS: [u64; BOARD_SIZE] = [
    0x89a1121896040240,
    0x2004844802002010,
    0x2068080051921000,
    0x62880a0220200808,
    0x4042004000000,
    0x100822020200011,
    0xc00444222012000a,
    0x28808801216001,
    0x400492088408100,
    0x201c401040c0084,
    0x840800910a0010,
    0x82080240060,
    0x2000840504006000,
    0x30010c4108405004,
    0x1008005410080802,
    0x8144042209100900,
    0x208081020014400,
    0x4800201208ca00,
    0xf18140408012008,
    0x1004002802102001,
    0x841000820080811,
    0x40200200a42008,
    0x800054042000,
    0x88010400410c9000,
    0x520040470104290,
    0x1004040051500081,
    0x2002081833080021,
    0x400c00c010142,
    0x941408200c002000,
    0x658810000806011,
    0x188071040440a00,
    0x4800404002011c00,
    0x104442040404200,
    0x511080202091021,
    0x4022401120400,
    0x80c0040400080120,
    0x8040010040820802,
    0x480810700020090,
    0x102008e00040242,
    0x809005202050100,
    0x8002024220104080,
    0x431008804142000,
    0x19001802081400,
    0x200014208040080,
    0x3308082008200100,
    0x41010500040c020,
    0x4012020c04210308,
    0x208220a202004080,
    0x111040120082000,
    0x6803040141280a00,
    0x2101004202410000,
    0x8200000041108022,
    0x21082088000,
    0x2410204010040,
    0x40100400809000,
    0x822088220820214,
    0x40808090012004,
    0x910224040218c9,
    0x402814422015008,
    0x90014004842410,
    0x1000042304105,
    0x10008830412a00,
    0x2520081090008908,
    0x40102000a0a60140,
];

#[derive(Default)]
struct PositionBitboards {
    pieces: [[Bitboard; PIECE_KIND_COUNT]; SIDE_COUNT],
    sides: [Bitboard; SIDE_COUNT],
    occupied: Bitboard,
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
    let mut moves = Vec::new();

    generate_moves(PieceKind::King, &pb, position.side_to_move, &mut moves);
    generate_moves(PieceKind::Queen, &pb, position.side_to_move, &mut moves);
    generate_moves(PieceKind::Bishop, &pb, position.side_to_move, &mut moves);
    generate_moves(PieceKind::Knight, &pb, position.side_to_move, &mut moves);
    generate_moves(PieceKind::Rook, &pb, position.side_to_move, &mut moves);
    generate_pawn_moves(&pb, position.side_to_move, position.en_passant, &mut moves);

    moves
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
        let moves = get_move_bitboard(kind, from, bitboards.occupied);

        let mut quiets = moves & !bitboards.occupied;
        let mut captures = moves & bitboards.sides[hostile];

        while let Some(to) = quiets.pop_lsb() {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();
            dest.push(Move::new(from, to, MoveKind::Quiet));
        }

        while let Some(to) = captures.pop_lsb() {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();
            dest.push(Move::new(from, to, MoveKind::Capture));
        }
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

        let mut quiets = moves & !bitboards.occupied;
        let mut captures = attacks & (bitboards.sides[hostile] | ep_square);

        while let Some(i) = quiets.pop_lsb() {
            // Safety: Always in range 0..64
            let to = Square::try_from(i).unwrap();
            dest.push(Move::new(from, to, MoveKind::Quiet));
        }

        while let Some(to) = captures.pop_lsb() {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();
            dest.push(Move::new(from, to, MoveKind::Capture));
        }
    }

    while let Some(doubles) = doubles.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(doubles).unwrap();

        // Safety: There is always one bit set in the moves bitboard.
        let jump = PAWN_MOVES[friendly][doubles].0.lsb().unwrap();
        let to = PAWN_MOVES[friendly][jump].0.lsb().unwrap();

        if !bitboards.occupied.get(jump) && !bitboards.occupied.get(to) {
            // Safety: Always in range 0..64
            let to = Square::try_from(to).unwrap();
            dest.push(Move::new(from, to, MoveKind::EnPassant));
        }
    }

    while let Some(pawn) = promotions.pop_lsb() {
        // Safety: Always in range 0..64
        let from = Square::try_from(pawn).unwrap();
        let (moves, attacks) = PAWN_MOVES[friendly][pawn];

        let quiets = moves & !bitboards.occupied;
        let captures = attacks & bitboards.sides[hostile];
        let mut all = quiets | captures;

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

fn get_move_bitboard(kind: PieceKind, square: Square, occupied: Bitboard) -> Bitboard {
    match kind {
        PieceKind::Bishop => BISHOP_MOVES.get(square, occupied),
        PieceKind::Rook => ROOK_MOVES.get(square, occupied),
        PieceKind::Queen => BISHOP_MOVES.get(square, occupied) | ROOK_MOVES.get(square, occupied),
        PieceKind::Pawn => unreachable!("Pawn moves should be calculated with dedicated function."),
        _ => PIECE_MOVES[kind as usize][square as usize],
    }
}

fn init_piece_moves() -> PieceMoveBitboards {
    let mut moves = [[Bitboard(0); BOARD_SIZE]; 5];

    for i in 0..64 {
        // Safety: 0..64 should always be in range.
        let from = Square::try_from(i).unwrap();

        let king_offsets = [1, -1, 8, -8, 9, -9, 7, -7];
        for offset in king_offsets {
            let dest_i = i as isize + offset;
            if let Some(dest) = Square::try_from(dest_i).ok() {
                if from.distance(dest) <= 2.0 {
                    moves[PieceKind::King as usize][i].on(dest);
                }
            }
        }

        let knight_offsets = [-17, -15, -10, -6, 6, 10, 15, 17];
        for offset in knight_offsets {
            let dest_i = i as isize + offset;
            if let Some(dest) = Square::try_from(dest_i).ok() {
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

fn init_pawn_moves() -> PawnMoveBitboards {
    let mut moves = [[(Bitboard(0), Bitboard(0)); BOARD_SIZE]; SIDE_COUNT];

    // Iterate through the entire board except the last rank.
    // Pawns get promoted on the last rank, so no pawn moves ever occur there.
    for i in 0..(BOARD_SIZE - BOARD_FILES) {
        // Safety: 0..64 should always be in range.
        let from = Square::try_from(i).unwrap();

        // The black pawns are calculated with the inverse of the white pawns
        let bi = BOARD_SIZE - i - 1;

        moves[Side::White as usize][i].0.on(i + BOARD_FILES);
        moves[Side::Black as usize][bi].0.on(bi - BOARD_FILES);

        // Captures
        for offset in [9, 7] {
            let dest_i = i as isize + offset;
            let dest_bi = bi as isize - offset;

            if let Some(dest) = Square::try_from(dest_i).ok() {
                if from.distance(dest) <= 2.0 {
                    moves[Side::White as usize][i].1.on(dest);
                }
            }

            if let Some(dest) = Square::try_from(dest_bi).ok() {
                if from.distance(dest) <= 2.0 {
                    moves[Side::Black as usize][i].1.on(dest);
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

            current = next
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

        for (piece, square) in position.pieces() {
            pb.pieces[piece.side as usize][piece.kind as usize].on(square);
            pb.sides[piece.side as usize].on(square);
            pb.occupied.on(square);
        }

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
                let occupancy = set_occupancy(j as u64, mask_size, magic.mask);
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
            assert_eq!(chessboard.perft(6), 119060324);
        }

        #[test]
        fn perft_test_02() {
            let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 193690690);
        }

        #[test]
        fn perft_test_03() {
            let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(7), 178633661);
        }

        #[test]
        fn perft_test_04() {
            let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 706045033);
        }

        #[test]
        fn perft_test_05() {
            let fen = "1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 1063513);
        }

        #[test]
        fn perft_test_illegal_ep_1() {
            let fen = "3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1134888);
        }

        #[test]
        fn perft_test_illegal_ep_2() {
            let fen = "8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1015133);
        }

        #[test]
        fn perft_test_ep_capture_checks() {
            let fen = "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 1015133);
        }

        #[test]
        fn perft_test_short_castling_check() {
            let fen = "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 661072);
        }

        #[test]
        fn perft_test_long_castling_check() {
            let fen = "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 803711);
        }

        #[test]
        fn perft_test_castle_rights() {
            let fen = "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4), 1274206);
        }

        #[test]
        fn perft_test_prevented_castle_rights() {
            let fen = "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4), 1720476);
        }

        #[test]
        fn perft_test_promote_out_of_check() {
            let fen = "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 3821001);
        }

        #[test]
        fn perft_test_discovered_check() {
            let fen = "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5), 1004658);
        }

        #[test]
        fn perft_test_promote_to_give_check() {
            let fen = "4k3/1P6/8/8/8/8/K7/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6), 217342);
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
            assert_eq!(chessboard.perft(7), 567584);
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

            assert_eq!(pb.occupied, Bitboard(0xFFFF00000000FFFF));
            assert_eq!(pb.sides[Side::White as usize], Bitboard(0x000000000000FFFF));
            assert_eq!(pb.sides[Side::Black as usize], Bitboard(0xFFFF000000000000));

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Pawn as usize],
                Bitboard(0x000000000000FF00)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Pawn as usize],
                Bitboard(0x00FF000000000000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Rook as usize],
                Bitboard(0x0000000000000081)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Rook as usize],
                Bitboard(0x8100000000000000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Knight as usize],
                Bitboard(0x0000000000000042)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Knight as usize],
                Bitboard(0x4200000000000000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Bishop as usize],
                Bitboard(0x0000000000000024)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Bishop as usize],
                Bitboard(0x2400000000000000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::Queen as usize],
                Bitboard(0x0000000000000008)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::Queen as usize],
                Bitboard(0x0800000000000000)
            );

            assert_eq!(
                pb.pieces[Side::White as usize][PieceKind::King as usize],
                Bitboard(0x0000000000000010)
            );
            assert_eq!(
                pb.pieces[Side::Black as usize][PieceKind::King as usize],
                Bitboard(0x1000000000000000)
            );
        }
    }
}
