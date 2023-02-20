#[allow(clippy::wildcard_imports)]
use crate::bitboards::constants::*;
use crate::bitboards::Bitboard;
use crate::piece::{PieceKind, Side, PIECE_KIND_COUNT, SIDE_COUNT};
use crate::square::{Direction, Square, BOARD_FILES, BOARD_SIZE};
use std::fmt;

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

// Array for all types of piece moves, except for the pawn.
pub(crate) type PieceMoveBitboards = [[Bitboard; BOARD_SIZE]; PIECE_KIND_COUNT - 1];

// Pawns move differently depending on what side they're on, and whether they're
// attacking or just moving.
pub(crate) type PawnMoveBitboards = [[(Bitboard, Bitboard); BOARD_SIZE]; SIDE_COUNT];

// This project uses the fancy magics approach to find sliding moves. It's the
// most common method, notably used by Stockfish.
// https://www.chessprogramming.org/Magic_Bitboards#Fancy
pub(crate) struct MagicTable<const SIZE: usize> {
    pub magics: [Magic; BOARD_SIZE],
    pub table: [[Bitboard; SIZE]; BOARD_SIZE],
}

#[derive(Clone, Copy, Default)]
pub(super) struct Magic {
    pub magic: u64,
    pub mask: Bitboard,
    pub shift: usize,
}

#[allow(clippy::cast_possible_wrap)]
pub(crate) fn init_pseudo_attacks() -> PieceMoveBitboards {
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
pub(crate) fn init_pawn_moves() -> PawnMoveBitboards {
    const DOUBLE_RANK: [Bitboard; SIDE_COUNT] = [BITBOARD_RANK_2, BITBOARD_RANK_7];

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

        if Bitboard(0b1 << white_i) & DOUBLE_RANK[Side::White as usize] != 0 {
            moves[Side::White as usize][white_i]
                .0
                .on(white_i + BOARD_FILES * 2);
        }
        if Bitboard(0b1 << black_i) & DOUBLE_RANK[Side::Black as usize] != 0 {
            moves[Side::Black as usize][black_i]
                .0
                .on(black_i - BOARD_FILES * 2);
        }

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

pub(crate) fn init_rook_moves() -> Box<MagicTable<4096>> {
    MagicTable::new(&ROOK_DIRECTIONS, &ROOK_MAGICS, &ROOK_BITS)
}

pub(crate) fn init_bishop_moves() -> Box<MagicTable<512>> {
    MagicTable::new(&BISHOP_DIRECTIONS, &BISHOP_MAGICS, &BISHOP_BITS)
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

impl<const SIZE: usize> MagicTable<SIZE> {
    fn new(
        directions: &[Direction],
        magics: &[u64; BOARD_SIZE],
        relevant_bits: &[usize; BOARD_SIZE],
    ) -> Box<Self> {
        let mut magic_table = unsafe {
            use std::alloc::{alloc, Layout};

            let layout = Layout::new::<Self>();
            let ptr = alloc(layout).cast::<MagicTable<SIZE>>();

            Box::from_raw(ptr)
        };

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

    pub const fn get(&self, origin: Square, occupied: Bitboard) -> Bitboard {
        self.table[origin as usize][self.index_of(origin, occupied)]
    }

    pub const fn index_of(&self, origin: Square, occupied: Bitboard) -> usize {
        self.magics[origin as usize].index_of(occupied)
    }
}

impl<const SIZE: usize> Default for MagicTable<SIZE> {
    fn default() -> Self {
        Self {
            magics: [Magic::default(); BOARD_SIZE],
            table: [[Bitboard(0); SIZE]; BOARD_SIZE],
        }
    }
}

impl<const SIZE: usize> fmt::Debug for MagicTable<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "MagicTable {{ magics: {:?}, table: {:?} }}",
            self.magics, self.table
        )
    }
}

impl Magic {
    #[allow(clippy::cast_possible_truncation)]
    const fn index_of(&self, mut occupied: Bitboard) -> usize {
        let mut result = occupied.0;
        result &= self.mask.0;
        result = result.wrapping_mul(self.magic);
        result >>= BOARD_SIZE - self.shift;
        result as usize
    }
}

impl fmt::Debug for Magic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "Magic {{ magic: {}, mask: {:?}, shift: {} }}",
            self.magic, self.mask, self.shift
        )
    }
}
