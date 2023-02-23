//! HCE evaluation mainly written based on Stockfishs evaluation guide.
//!
//! <https://hxim.github.io/Stockfish-Evaluation-Guide/>

#![cfg_attr(feature = "nnue", allow(dead_code))]

#[allow(clippy::wildcard_imports)]
use crate::bitboards::{constants::*, Bitboard};
use crate::movegen;
use crate::piece::{Piece, PieceKind, Side, PIECE_KIND_COUNT, SIDE_COUNT};
use crate::position::{Position, PositionBitboards};
use crate::square::{Direction, Square, BOARD_FILES, BOARD_RANKS};

const EARLY_GAME_VALUES: [i32; PIECE_KIND_COUNT] = [10_000_000, 2538, 825, 781, 1276, 124];
const END_GAME_VALUES: [i32; PIECE_KIND_COUNT] = [10_000_000, 2682, 915, 854, 1380, 206];

const EARLY_GAME_PSQT: [[[i32; BOARD_FILES / 2]; BOARD_RANKS]; PIECE_KIND_COUNT - 1] = [
    [
        [271, 327, 271, 198],
        [278, 303, 234, 179],
        [195, 258, 169, 120],
        [164, 190, 138, 98],
        [154, 179, 105, 70],
        [123, 145, 81, 31],
        [88, 120, 65, 33],
        [59, 89, 45, -1],
    ],
    [
        [3, -5, -5, 4],
        [-3, 5, 8, 12],
        [-3, 6, 13, 7],
        [4, 5, 9, 8],
        [0, 14, 12, 5],
        [-4, 10, 6, 8],
        [-5, 6, 10, 8],
        [-2, -2, 1, -2],
    ],
    [
        [-53, -5, -8, -23],
        [-15, 8, 19, 4],
        [-7, 21, -5, 17],
        [-5, 11, 25, 39],
        [-12, 29, 22, 31],
        [-16, 6, 1, 11],
        [-17, -14, 5, 0],
        [-48, 1, -14, -23],
    ],
    [
        [-175, -92, -74, -73],
        [-77, -41, -27, -15],
        [-61, -17, 6, 12],
        [-35, 8, 40, 49],
        [-34, 13, 44, 51],
        [-9, 22, 58, 53],
        [-67, -27, 4, 37],
        [-201, -83, -56, -26],
    ],
    [
        [-31, -20, -14, -5],
        [-21, -13, -8, 6],
        [-25, -11, -1, 3],
        [-13, -5, -4, -6],
        [-27, -15, -4, 3],
        [-22, -2, 6, 12],
        [-2, 12, 16, 18],
        [-17, -19, -1, 9],
    ],
];
const EARLY_GAME_PAWN_PSQT: [[i32; BOARD_FILES]; BOARD_RANKS] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [3, 3, 10, 19, 16, 19, 7, -5],
    [-9, -15, 11, 15, 32, 22, 5, -22],
    [-4, -23, 6, 20, 40, 17, 4, -8],
    [13, 0, -13, 1, 11, -2, -13, 5],
    [5, -12, -7, 22, -8, -5, -15, -8],
    [-7, 7, -3, -13, 5, -16, 10, -8],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const END_GAME_PSQT: [[[i32; BOARD_FILES / 2]; BOARD_RANKS]; PIECE_KIND_COUNT - 1] = [
    [
        [1, 45, 85, 76],
        [53, 100, 133, 135],
        [88, 130, 169, 175],
        [103, 156, 172, 172],
        [96, 166, 199, 199],
        [92, 172, 184, 191],
        [47, 121, 116, 131],
        [11, 59, 73, 78],
    ],
    [
        [-69, -57, -47, -26],
        [-55, -31, -22, -4],
        [-39, -18, -9, 3],
        [-23, -3, 13, 24],
        [-29, -6, 9, 21],
        [-38, -18, -12, 1],
        [-50, -27, -24, -8],
        [-75, -52, -43, -36],
    ],
    [
        [-57, -30, -37, -12],
        [-37, -13, -17, 1],
        [-16, -1, -2, 10],
        [-20, -6, 0, 17],
        [-17, -1, -14, 15],
        [-30, 6, 4, 6],
        [-31, -20, -1, 1],
        [-46, -42, -37, -24],
    ],
    [
        [-96, -65, -49, -21],
        [-67, -54, -18, 8],
        [-40, -27, -8, 29],
        [-35, -2, 13, 28],
        [-45, -16, 9, 39],
        [-51, -44, -16, 17],
        [-69, -50, -51, 12],
        [-100, -88, -56, -17],
    ],
    [
        [-9, -13, -10, -9],
        [-12, -9, -1, -2],
        [6, -8, -2, -6],
        [-6, 1, -9, 7],
        [-5, 8, 7, -6],
        [6, 1, -7, 10],
        [4, 5, 20, -5],
        [18, 0, 19, 13],
    ],
];
const END_GAME_PAWN_PSQT: [[i32; BOARD_FILES]; BOARD_RANKS] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [-10, -6, 10, 0, 14, 7, -5, -19],
    [-10, -10, -10, 4, 4, 3, -6, -4],
    [6, -2, -8, -4, -13, -12, -10, -9],
    [10, 5, 4, -5, -5, -5, 14, 9],
    [28, 20, 21, 28, 30, 7, 6, 13],
    [0, -11, 12, 21, 25, 19, 4, 7],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const EARLY_GAME_MOBILITY: [&[i32]; PIECE_KIND_COUNT - 2] = [
    &[
        -30, -12, -8, -9, 20, 23, 23, 35, 38, 53, 64, 65, 65, 66, 67, 67, 72, 72, 77, 79, 93, 108,
        108, 108, 110, 114, 114, 116,
    ],
    &[-48, -20, 16, 26, 38, 51, 55, 63, 63, 68, 81, 81, 91, 98],
    &[-62, -53, -12, -4, 3, 13, 22, 28, 33],
    &[-60, -20, 2, 3, 3, 11, 22, 31, 40, 40, 41, 48, 57, 57, 62],
];
const END_GAME_MOBILITY: [&[i32]; PIECE_KIND_COUNT - 2] = [
    &[
        -30, -12, -8, -9, 20, 23, 23, 35, 38, 53, 64, 65, 65, 66, 67, 67, 72, 72, 77, 79, 93, 108,
        108, 108, 110, 114, 114, 116,
    ],
    &[-48, -20, 16, 26, 38, 51, 55, 63, 63, 68, 81, 81, 91, 98],
    &[-62, -53, -12, -4, 3, 13, 22, 28, 33],
    &[-60, -20, 2, 3, 3, 11, 22, 31, 40, 40, 41, 48, 57, 57, 62],
];

const KING_ATTACKERS_WEIGHT: [i32; PIECE_KIND_COUNT] = [0, 10, 52, 81, 44, 0];
const KING_SAFE_CHECK: [[i32; 2]; PIECE_KIND_COUNT] = [
    [0, 0],
    [730, 1128],
    [650, 984],
    [805, 1292],
    [1071, 1886],
    [0, 0],
];

const KING_FLANK: [u64; BOARD_FILES] = [
    BITBOARD_QUEENSIDE.0 & !BITBOARD_FILE_D.0,
    BITBOARD_QUEENSIDE.0,
    BITBOARD_QUEENSIDE.0,
    BITBOARD_CENTER_FILES.0,
    BITBOARD_CENTER_FILES.0,
    BITBOARD_KINGSIDE.0,
    BITBOARD_KINGSIDE.0,
    BITBOARD_KINGSIDE.0 & !BITBOARD_FILE_E.0,
];
const SIDE_CAMPS: [u64; SIDE_COUNT] = [
    BITBOARD_RANK_6.0 | BITBOARD_RANK_7.0 | BITBOARD_RANK_8.0,
    BITBOARD_RANK_1.0 | BITBOARD_RANK_2.0 | BITBOARD_RANK_3.0,
];

const TEMPO: f64 = 28.0;

const WHITE: usize = Side::White as usize;
const BLACK: usize = Side::Black as usize;

const PAWN: usize = PieceKind::Pawn as usize;
const ROOK: usize = PieceKind::Rook as usize;
const KNIGHT: usize = PieceKind::Knight as usize;
const BISHOP: usize = PieceKind::Bishop as usize;
const QUEEN: usize = PieceKind::Queen as usize;
const KING: usize = PieceKind::King as usize;

#[allow(clippy::cast_lossless, clippy::cast_possible_truncation)]
pub(super) fn hce_evaluation(position: &Position, bb: &PositionBitboards) -> i32 {
    let early_game = game_evaluation(position, bb, false);
    let mut end_game = game_evaluation(position, bb, true) as f64;
    let phase = phase(bb);
    end_game = end_game * scale_factor(position, bb, end_game) / 64.0;
    let mut score = (early_game as f64 * phase + (end_game * (128.0 - phase))) / 128.0;
    score += TEMPO;
    score = score * (50.0 - position.halftime as f64) / 50.0;
    score as i32
}

fn game_evaluation(position: &Position, bb: &PositionBitboards, end_game: bool) -> i32 {
    let side = position.side_to_move;

    let mut score = 0;
    score += total_material(bb, side, end_game) - total_material(bb, !side, end_game);
    score += total_psqt(position, side, end_game) - total_psqt(position, !side, end_game);
    score += total_mobility(position, bb, side, end_game)
        - total_mobility(position, bb, !side, end_game);
    score += threats(position, bb, side, end_game) - threats(position, bb, !side, end_game);
    score += space(bb, side, end_game) - space(bb, !side, end_game);
    score += king(position, bb, side, end_game) - king(position, bb, !side, end_game);
    score
}

#[inline]
#[allow(clippy::cast_lossless)]
fn phase(bb: &PositionBitboards) -> f64 {
    const EARLY_GAME_LIMIT: f64 = 15258.0;
    const END_GAME_LIMIT: f64 = 3915.0;

    let mut npm = (non_pawn_material(bb, Side::White) + non_pawn_material(bb, Side::Black)) as f64;
    npm = END_GAME_LIMIT.max(EARLY_GAME_LIMIT.min(npm));

    ((npm - END_GAME_LIMIT) * 128.0) / (EARLY_GAME_LIMIT - END_GAME_LIMIT)
}

fn scale_factor(position: &Position, bb: &PositionBitboards, end_game: f64) -> f64 {
    let side = position.side_to_move;
    if end_game > 0.0 {
        scale_factor_for_side(side, position, bb)
    } else {
        scale_factor_for_side(!side, position, bb)
    }
}

#[allow(clippy::cast_precision_loss)]
fn scale_factor_for_side(side: Side, position: &Position, bb: &PositionBitboards) -> f64 {
    const EARLY_GAME_BISHOP_VALUE: i32 = EARLY_GAME_VALUES[BISHOP];
    const EARLY_GAME_ROOK_VALUE: i32 = EARLY_GAME_VALUES[ROOK];

    let friendly = side as usize;
    let hostile = !side as usize;

    let f_queens = bb.pieces[friendly][QUEEN].pop_count();
    let f_bishops = bb.pieces[friendly][BISHOP].pop_count();
    let f_knights = bb.pieces[friendly][KNIGHT].pop_count();
    let f_pawns = bb.pieces[friendly][PAWN].pop_count();

    let h_queens = bb.pieces[hostile][QUEEN].pop_count();
    let h_bishops = bb.pieces[hostile][BISHOP].pop_count();
    let h_knights = bb.pieces[hostile][KNIGHT].pop_count();
    let h_pawns = bb.pieces[hostile][PAWN].pop_count();

    let f_npm = non_pawn_material(bb, side);
    let h_npm = non_pawn_material(bb, !side);

    if f_pawns == 0 && f_npm - h_npm <= EARLY_GAME_BISHOP_VALUE {
        if f_npm < EARLY_GAME_ROOK_VALUE {
            0.0
        } else if h_npm <= EARLY_GAME_BISHOP_VALUE {
            4.0
        } else {
            14.0
        }
    } else {
        let are_opposite_bishops = opposite_bishops(bb);

        if are_opposite_bishops
            && f_npm == EARLY_GAME_BISHOP_VALUE
            && h_npm == EARLY_GAME_BISHOP_VALUE
        {
            22.0 + 4.0 * candidate_passed(side, position, bb)
        } else if are_opposite_bishops {
            22.0 + 3.0 * bb.sides[WHITE].pop_count() as f64
        } else {
            #[allow(clippy::cast_possible_wrap)]
            if f_npm == EARLY_GAME_ROOK_VALUE
                && h_npm == EARLY_GAME_ROOK_VALUE
                && f_pawns as isize - h_pawns as isize <= 1
            {
                let mut h_pawnking = false;
                let mut pcf_flank = [0, 0];
                for rank in 0..BOARD_RANKS {
                    for file in 0..BOARD_FILES {
                        let square = Square::from_rank_file(rank, file).expect("Is in bounds");
                        if position[square]
                            == Some(Piece {
                                kind: PieceKind::Pawn,
                                side: Side::White,
                            })
                        {
                            let i = usize::from(file < 4);
                            pcf_flank[i] = 1;
                        } else if position[square]
                            == Some(Piece {
                                kind: PieceKind::King,
                                side: Side::Black,
                            })
                        {
                            let attacks = movegen::get_attacks_bitboard(
                                PieceKind::King,
                                Side::Black,
                                square,
                                bb.occupied,
                            );
                            if bb.pieces[hostile][PAWN] & attacks != 0 {
                                h_pawnking = true;
                            }
                        }
                    }
                }

                if pcf_flank[0] != pcf_flank[1] && h_pawnking {
                    return 36.0;
                }
            }

            if f_queens + h_queens == 1 {
                let extra = if f_queens == 1 {
                    h_bishops + h_knights
                } else {
                    f_bishops + f_knights
                } as f64;

                37.0 + 3.0 * extra
            } else {
                64.0f64.min(36.0 + 7.0 * f_queens as f64)
            }
        }
    }
}

#[inline]
#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
const fn total_material(bb: &PositionBitboards, side: Side, end_game: bool) -> i32 {
    let friendly = side as usize;

    piece_value(PieceKind::Queen, end_game) * bb.pieces[friendly][QUEEN].pop_count() as i32
        + piece_value(PieceKind::Bishop, end_game) * bb.pieces[friendly][BISHOP].pop_count() as i32
        + piece_value(PieceKind::Knight, end_game) * bb.pieces[friendly][KNIGHT].pop_count() as i32
        + piece_value(PieceKind::Rook, end_game) * bb.pieces[friendly][ROOK].pop_count() as i32
        + piece_value(PieceKind::Pawn, end_game) * bb.pieces[friendly][PAWN].pop_count() as i32
}

#[inline]
#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
const fn non_pawn_material(bb: &PositionBitboards, side: Side) -> i32 {
    let friendly = side as usize;

    piece_value(PieceKind::Queen, false) * bb.pieces[friendly][QUEEN].pop_count() as i32
        + piece_value(PieceKind::Bishop, false) * bb.pieces[friendly][BISHOP].pop_count() as i32
        + piece_value(PieceKind::Knight, false) * bb.pieces[friendly][KNIGHT].pop_count() as i32
        + piece_value(PieceKind::Rook, false) * bb.pieces[friendly][ROOK].pop_count() as i32
}

fn total_psqt(position: &Position, side: Side, end_game: bool) -> i32 {
    position
        .squares
        .iter()
        .enumerate()
        .filter_map(|(square, content)| {
            if let Some(piece) = content {
                if piece.side == side {
                    Some((Square::try_from(square).ok()?, piece.kind))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .fold(0, |acc, (square, kind)| {
            acc + psqt(kind, side, square, end_game)
        })
}

fn total_mobility(position: &Position, bb: &PositionBitboards, side: Side, end_game: bool) -> i32 {
    position
        .squares
        .iter()
        .enumerate()
        .filter_map(|(square, content)| {
            if let Some(piece) = content {
                if piece.side == side {
                    Some((Square::try_from(square).ok()?, piece.kind))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .map(|(square, kind)| mobility_bonus(bb, kind, side, square, end_game))
        .sum()
}

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn threats(position: &Position, bb: &PositionBitboards, side: Side, end_game: bool) -> i32 {
    const PUSH_RANK: [Bitboard; SIDE_COUNT] = [BITBOARD_RANK_3, BITBOARD_RANK_6];

    let friendly = side as usize;
    let hostile = !side as usize;

    let mut score = 0;

    let non_pawn_hostile = bb.sides[hostile] & !bb.pieces[hostile][PAWN];
    let strongly_protected =
        bb.attacked_by[hostile][PAWN] | (bb.attacked_by_2[hostile] & !bb.attacked_by_2[friendly]);

    let defended_hostile = non_pawn_hostile & strongly_protected;
    let weak_hostile = bb.sides[hostile] & !strongly_protected & bb.attacked[friendly];

    if defended_hostile | weak_hostile != 0 {
        let mut attacked = (defended_hostile | weak_hostile)
            & (bb.attacked_by[friendly][KNIGHT] | bb.attacked_by[friendly][BISHOP]);

        while let Some(square) = attacked.pop_lsb_square() {
            let kind = position[square].unwrap().kind;
            score += minor_threat(kind, end_game);
        }

        attacked = weak_hostile & bb.attacked_by[friendly][ROOK];
        while let Some(square) = attacked.pop_lsb_square() {
            let kind = position[square].unwrap().kind;
            score += rook_threat(kind, end_game);
        }

        if weak_hostile & bb.attacked_by[friendly][KING] != 0 && end_game {
            score += 87;
        } else if weak_hostile & bb.attacked_by[friendly][KING] != 0 {
            score += 24;
        }

        attacked = !bb.attacked[hostile] | (non_pawn_hostile & bb.attacked_by_2[friendly]);
        if end_game {
            score += 69 * (weak_hostile & attacked).pop_count() as i32;
            score += 14 * (weak_hostile & bb.attacked_by[hostile][QUEEN]).pop_count() as i32;
        } else {
            score += 89 * (weak_hostile & attacked).pop_count() as i32;
        }
    }

    let attacked = bb.attacked[hostile] & !strongly_protected & bb.attacked[friendly];
    score += 7 * attacked.pop_count() as i32;

    let safe_squares = !bb.attacked[hostile] | bb.attacked[friendly];

    let mut pawns = bb.pieces[friendly][PAWN] & safe_squares;
    let mut safe_pawn_attacks = Bitboard(0);
    while let Some(square) = pawns.pop_lsb_square() {
        let pawn_push = movegen::get_attacks_bitboard(PieceKind::Pawn, side, square, bb.occupied);
        safe_pawn_attacks |= pawn_push & non_pawn_hostile;
    }

    let forward = side.forward().offset();
    if forward > 0 {
        pawns = (bb.pieces[friendly][PAWN] << forward) & !bb.occupied;
        pawns |= ((pawns & PUSH_RANK[friendly]) << forward) & !bb.occupied;
    } else {
        pawns = (bb.pieces[friendly][PAWN] >> -forward) & !bb.occupied;
        pawns |= ((pawns & PUSH_RANK[friendly]) >> -forward) & !bb.occupied;
    }
    pawns &= !bb.attacked_by[hostile][PAWN] & safe_squares;

    let mut safe_pawn_pushes = Bitboard(0);
    while let Some(square) = pawns.pop_lsb_square() {
        let pawn_push = movegen::get_attacks_bitboard(PieceKind::Pawn, side, square, bb.occupied);
        safe_pawn_pushes |= pawn_push & non_pawn_hostile;
    }

    if end_game {
        score += 99 * safe_pawn_attacks.pop_count() as i32;
        score += 39 * safe_pawn_pushes.pop_count() as i32;
    } else {
        score += 167 * safe_pawn_attacks.pop_count() as i32;
        score += 48 * safe_pawn_pushes.pop_count() as i32;
    }

    if bb.pieces[hostile][QUEEN].pop_count() == 1 {
        let queen_imbalance =
            i32::from((bb.pieces[hostile][QUEEN] | bb.pieces[friendly][QUEEN]).pop_count() == 1);

        let queen_square = bb.pieces[hostile][QUEEN].lsb_square().unwrap();
        let safe_squares =
            mobility_area(bb, side) & !bb.pieces[friendly][PAWN] & !strongly_protected;

        let knight_threat = bb.attacked_by[friendly][KNIGHT]
            & movegen::get_attacks_bitboard(PieceKind::Knight, side, queen_square, bb.occupied)
            & safe_squares;

        let slider_threat = (bb.attacked_by[friendly][BISHOP]
            & movegen::get_attacks_bitboard(PieceKind::Bishop, side, queen_square, bb.occupied))
            | (bb.attacked_by[friendly][ROOK]
                & movegen::get_attacks_bitboard(PieceKind::Rook, side, queen_square, bb.occupied))
                & safe_squares
                & bb.attacked_by_2[friendly];

        if end_game {
            score += 11 * knight_threat.pop_count() as i32 * (1 + queen_imbalance);
            score += 21 * slider_threat.pop_count() as i32 * (1 + queen_imbalance);
        } else {
            score += 16 * knight_threat.pop_count() as i32 * (1 + queen_imbalance);
            score += 62 * slider_threat.pop_count() as i32 * (1 + queen_imbalance);
        }
    }

    score
}

#[inline]
fn king(position: &Position, bb: &PositionBitboards, side: Side, end_game: bool) -> i32 {
    if end_game {
        king_end_game(position, bb, side)
    } else {
        king_early_game(position, bb, side)
    }
}

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn space(bb: &PositionBitboards, side: Side, end_game: bool) -> i32 {
    const SPACE_MASK: [Bitboard; SIDE_COUNT] = [
        Bitboard(
            BITBOARD_CENTER_FILES.0 & (BITBOARD_RANK_2.0 | BITBOARD_RANK_3.0 | BITBOARD_RANK_4.0),
        ),
        Bitboard(
            BITBOARD_CENTER_FILES.0 & (BITBOARD_RANK_7.0 | BITBOARD_RANK_6.0 | BITBOARD_RANK_5.0),
        ),
    ];

    if end_game || non_pawn_material(bb, side) + non_pawn_material(bb, !side) < 11551 {
        return 0;
    }

    let friendly = side as usize;
    let hostile = !side as usize;

    let safe_squares =
        SPACE_MASK[friendly] & !bb.pieces[friendly][PAWN] & !bb.attacked_by[hostile][PAWN];

    let backward = side.backward().offset();
    let forward = side.forward().offset();

    let mut behind = bb.pieces[friendly][PAWN];
    if backward <= 0 {
        behind |= behind >> -backward;
        behind |= behind >> (-backward * 2);
    } else {
        behind |= behind << backward;
        behind |= behind << (backward * 2);
    }

    let mut in_front = bb.pieces[friendly][PAWN];
    if forward >= 0 {
        in_front <<= forward;
    } else {
        in_front >>= -forward;
    }

    let blocked_count = (in_front & bb.pieces[hostile][PAWN]).pop_count();

    let bonus =
        safe_squares.pop_count() + (behind & safe_squares & !bb.attacked[hostile]).pop_count();
    let weight = bb.sides[friendly].pop_count() - 3 + blocked_count.min(9);

    let score = bonus * weight.pow(2) / 2;
    score as i32
}

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn king_early_game(position: &Position, bb: &PositionBitboards, side: Side) -> i32 {
    let friendly = side as usize;
    let hostile = !side as usize;

    let king_square = bb.pieces[friendly][KING]
        .lsb_square()
        .expect("King on board");
    let (_, king_file) = king_square.to_rank_file();

    let king_flank_once_attacked =
        bb.attacked[hostile] & KING_FLANK[king_file] & SIDE_CAMPS[friendly];
    let king_flank_twice_attacked = king_flank_once_attacked & bb.attacked_by_2[hostile];
    let king_flank_attack =
        king_flank_once_attacked.pop_count() + king_flank_twice_attacked.pop_count();

    let mut score = 0;

    if !((bb.pieces[friendly][PAWN] | bb.pieces[hostile][PAWN]) & KING_FLANK[king_file]) != 0 {
        score -= 17;
    }

    score -= king_flank_attack as i32 * 8;
    score -= king_danger(position, bb, side).pow(2) / 4096;

    score
}

fn king_end_game(position: &Position, bb: &PositionBitboards, side: Side) -> i32 {
    let friendly = side as usize;
    let hostile = !side as usize;

    let king_square = bb.pieces[friendly][KING]
        .lsb_square()
        .expect("King on board");
    let (_, king_file) = king_square.to_rank_file();

    let mut score = 0;

    if !((bb.pieces[friendly][PAWN] | bb.pieces[hostile][PAWN]) & KING_FLANK[king_file]) != 0 {
        score -= 95;
    }

    score -= king_danger(position, bb, side) / 16;
    score -= 16 * king_pawn_distance(bb, side);

    score
}

#[allow(
    clippy::too_many_lines,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]
fn king_danger(position: &Position, bb: &PositionBitboards, side: Side) -> i32 {
    let friendly = side as usize;
    let hostile = !side as usize;

    let mut king_danger = 0;

    let king_square = bb.pieces[friendly][KING]
        .lsb_square()
        .expect("King on board");
    let (_, king_file) = king_square.to_rank_file();

    let weak_squares = bb.attacked[hostile]
        & !bb.attacked_by_2[friendly]
        & (!bb.attacked[friendly]
            | bb.attacked_by[friendly][KING]
            | bb.attacked_by[friendly][QUEEN]);

    let safe_squares =
        !bb.sides[hostile] & (!bb.attacked[friendly] | (weak_squares & bb.attacked_by_2[hostile]));

    let mut unsafe_checks = Bitboard(0);

    let king_rook_attacks = movegen::get_attacks_bitboard(
        PieceKind::Rook,
        side,
        king_square,
        bb.occupied & !bb.pieces[friendly][QUEEN],
    );
    let king_bishop_attacks = movegen::get_attacks_bitboard(
        PieceKind::Bishop,
        side,
        king_square,
        bb.occupied & !bb.pieces[friendly][QUEEN],
    );

    let rook_checks = king_rook_attacks & bb.attacked_by[hostile][ROOK] & safe_squares;
    if rook_checks == 0 {
        unsafe_checks |= king_rook_attacks & bb.attacked_by[hostile][ROOK];
    } else {
        king_danger += KING_SAFE_CHECK[ROOK][usize::from(rook_checks.more_than_one())];
    }

    let queen_checks = (king_rook_attacks | king_bishop_attacks)
        & bb.attacked_by[hostile][QUEEN]
        & safe_squares
        & !(bb.attacked_by[friendly][QUEEN] | rook_checks);

    if queen_checks != 0 {
        king_danger += KING_SAFE_CHECK[QUEEN][usize::from(queen_checks.more_than_one())];
    }

    let bishop_checks =
        king_bishop_attacks & bb.attacked_by[hostile][BISHOP] & safe_squares & !queen_checks;
    if bishop_checks == 0 {
        unsafe_checks |= king_bishop_attacks & bb.attacked_by[hostile][BISHOP];
    } else {
        king_danger += KING_SAFE_CHECK[BISHOP][usize::from(bishop_checks.more_than_one())];
    }

    let knight_attacks =
        movegen::get_attacks_bitboard(PieceKind::Knight, side, king_square, bb.occupied);
    let knight_checks = knight_attacks & bb.attacked_by[hostile][KNIGHT] & safe_squares;
    if knight_attacks & safe_squares == 0 {
        unsafe_checks |= knight_attacks & bb.attacked_by[hostile][KNIGHT];
    } else {
        king_danger += KING_SAFE_CHECK[KNIGHT][usize::from(knight_checks.more_than_one())];
    }

    let king_ring = bb.pieces[friendly][KING] | bb.attacked_by[friendly][KING];
    let pinned_pieces = (bb.pinmask_hv[friendly] | bb.pinmask_d12[friendly]) & bb.occupied;
    let no_queen = bb.pieces[hostile][QUEEN] == 0;
    let knight_defender = bb.attacked_by[friendly][KNIGHT] & bb.attacked_by[friendly][KING] != 0;

    let king_flank_once_attacked =
        bb.attacked[hostile] & KING_FLANK[king_file] & SIDE_CAMPS[friendly];
    let king_flank_twice_attacked = king_flank_once_attacked & bb.attacked_by_2[hostile];
    let king_flank_attack =
        king_flank_once_attacked.pop_count() + king_flank_twice_attacked.pop_count();
    let king_flank_defence = bb.attacked[friendly] & KING_FLANK[king_file] & SIDE_CAMPS[friendly];

    let mut king_attackers_count = (king_ring & bb.attacked_by[hostile][PAWN]).pop_count() as i32;
    let mut king_attackers_weight = 0;
    let mut king_attacks_count = 0;
    for (i, mut pieces) in bb.pieces[hostile].into_iter().enumerate() {
        let kind = PieceKind::try_from(i).unwrap();
        let occupied = match kind {
            PieceKind::Rook => bb.occupied & !bb.pieces[friendly][QUEEN],
            PieceKind::Bishop => {
                bb.occupied & !bb.pieces[friendly][QUEEN] & !bb.pieces[friendly][ROOK]
            }
            _ => bb.occupied,
        };

        while let Some(square) = pieces.pop_lsb_square() {
            let attacks = movegen::get_attacks_bitboard(kind, !side, square, occupied);

            if attacks & king_ring != 0 {
                king_attackers_count += 1;
                king_attackers_weight += KING_ATTACKERS_WEIGHT[i];
                king_attacks_count += (attacks & bb.attacked_by[friendly][KING]).pop_count() as i32;
            }
        }
    }

    king_danger += king_attackers_count * king_attackers_weight;
    king_danger += 183 * (king_ring & weak_squares).pop_count() as i32;
    king_danger += 148 * unsafe_checks.pop_count() as i32;
    king_danger += 98 * pinned_pieces.pop_count() as i32;
    king_danger += 69 * king_attacks_count;
    king_danger += 3 * king_flank_attack.pow(2) as i32 / 8;
    king_danger += total_mobility(position, bb, !side, false);
    king_danger -= total_mobility(position, bb, side, false);
    king_danger -= 873 * i32::from(no_queen);
    king_danger -= 100 * i32::from(knight_defender);
    king_danger -= 4 * king_flank_defence.pop_count() as i32;
    king_danger += 37;

    if king_danger > 100 {
        king_danger
    } else {
        0
    }
}

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn king_pawn_distance(bb: &PositionBitboards, side: Side) -> i32 {
    let friendly = side as usize;
    let king_square = bb.pieces[friendly][KING]
        .lsb_square()
        .expect("King on board");
    let (king_rank, king_file) = king_square.to_rank_file();

    let mut closest = i32::MAX;
    let mut pawns = bb.pieces[friendly][PAWN];
    while let Some(square) = pawns.pop_lsb_square() {
        let (rank, file) = square.to_rank_file();
        let distance = rank.abs_diff(king_rank) + file.abs_diff(king_file);
        closest = closest.min(distance as i32);
    }

    closest.min(6)
}

#[allow(clippy::cast_precision_loss)]
fn candidate_passed(side: Side, position: &Position, bb: &PositionBitboards) -> f64 {
    position
        .squares
        .iter()
        .enumerate()
        .filter_map(|(square, content)| {
            if let Some(piece) = content {
                if piece.kind == PieceKind::Pawn && piece.side == side {
                    Square::try_from(square).ok()
                } else {
                    None
                }
            } else {
                None
            }
        })
        .filter(|&square| is_candidate(side, square, bb))
        .count() as f64
}

fn is_candidate(side: Side, square: Square, bb: &PositionBitboards) -> bool {
    let hostile = !side as usize;

    let free_map = {
        let own_file = square.file();
        let neighbour_files = [
            square.neighbour(Direction::West),
            square.neighbour(Direction::East),
        ]
        .into_iter()
        .flatten()
        .fold(Bitboard(0), |acc, square| acc | square.file());

        let mut passed_ranks = square.rank();
        let mut current = square;
        while let Some(next) = current.neighbour(side.backward()) {
            passed_ranks |= next.rank();
            current = next;
        }

        (own_file | neighbour_files) & !passed_ranks
    };
    let free_advance = free_map & bb.pieces[hostile][PAWN] == 0;
    let passed_defence = passed_defence(side, square, bb);

    free_advance || passed_defence
}

fn passed_defence(side: Side, mut square: Square, bb: &PositionBitboards) -> bool {
    let friendly = side as usize;
    let hostile = !side as usize;

    let mut distance = 0;
    while let Some(next) = square.neighbour(side.forward()) {
        if bb.pieces[friendly][PAWN].get(next) || bb.pieces[hostile][PAWN].get(next) {
            return false;
        }

        distance += 1;
        square = next;
    }

    distance < 4
}

const fn opposite_bishops(bb: &PositionBitboards) -> bool {
    if bb.pieces[WHITE][BISHOP].pop_count() != 1 || bb.pieces[BLACK][BISHOP].pop_count() != 1 {
        false
    } else {
        let mut bishops = [0, 0];

        if let Some(square) = bb.pieces[WHITE][BISHOP].lsb() {
            let rank = square / 8;
            let file = square % 8;
            bishops[WHITE] = rank + file;
        }

        if let Some(square) = bb.pieces[BLACK][BISHOP].lsb() {
            let rank = square / 8;
            let file = square % 8;
            bishops[BLACK] = rank + file;
        }

        bishops[0] != bishops[1]
    }
}

#[inline]
pub(crate) const fn piece_value(kind: PieceKind, end_game: bool) -> i32 {
    if end_game {
        END_GAME_VALUES[kind as usize]
    } else {
        EARLY_GAME_VALUES[kind as usize]
    }
}

#[inline]
const fn psqt(kind: PieceKind, side: Side, square: Square, end_game: bool) -> i32 {
    let pawn = kind as usize == PAWN;
    let (mut rank, mut file) = square.to_rank_file();

    if side as usize != WHITE {
        rank = BOARD_RANKS - (rank + 1);
    }

    if !pawn && file >= 4 {
        file = BOARD_FILES - (file + 1);
    }

    if end_game && pawn {
        END_GAME_PAWN_PSQT[rank][file]
    } else if end_game {
        END_GAME_PSQT[kind as usize][rank][file]
    } else if pawn {
        EARLY_GAME_PAWN_PSQT[rank][file]
    } else {
        EARLY_GAME_PSQT[kind as usize][rank][file]
    }
}

#[inline]
const fn minor_threat(kind: PieceKind, end_game: bool) -> i32 {
    const EARLY_GAME_THREAT_MINOR: [i32; PIECE_KIND_COUNT] = [0, 79, 77, 57, 88, 5];
    const END_GAME_THREAT_MINOR: [i32; PIECE_KIND_COUNT] = [0, 161, 56, 41, 119, 32];

    if end_game {
        END_GAME_THREAT_MINOR[kind as usize]
    } else {
        EARLY_GAME_THREAT_MINOR[kind as usize]
    }
}

#[inline]
const fn rook_threat(kind: PieceKind, end_game: bool) -> i32 {
    const EARLY_GAME_THREAT_MINOR: [i32; PIECE_KIND_COUNT] = [0, 58, 42, 37, 0, 3];
    const END_GAME_THREAT_MINOR: [i32; PIECE_KIND_COUNT] = [0, 79, 77, 57, 88, 5];

    if end_game {
        END_GAME_THREAT_MINOR[kind as usize]
    } else {
        EARLY_GAME_THREAT_MINOR[kind as usize]
    }
}

#[inline]
#[allow(clippy::cast_sign_loss)]
const fn mobility_bonus(
    bb: &PositionBitboards,
    kind: PieceKind,
    side: Side,
    square: Square,
    end_game: bool,
) -> i32 {
    let k = kind as usize;
    if k == KING || k == PAWN {
        0
    } else if end_game {
        END_GAME_MOBILITY[kind as usize - 1][mobility(bb, kind, side, square) as usize]
    } else {
        EARLY_GAME_MOBILITY[kind as usize - 1][mobility(bb, kind, side, square) as usize]
    }
}

#[inline]
#[allow(clippy::cast_possible_wrap)]
const fn mobility(bb: &PositionBitboards, kind: PieceKind, side: Side, square: Square) -> i32 {
    (movegen::get_attacks_bitboard(kind, side, square, bb.occupied).0 & mobility_area(bb, side).0)
        .count_ones() as i32
}

const fn mobility_area(bb: &PositionBitboards, side: Side) -> Bitboard {
    const LOW_RANKS: [u64; SIDE_COUNT] = [
        BITBOARD_RANK_2.0 | BITBOARD_RANK_3.0,
        BITBOARD_RANK_7.0 | BITBOARD_RANK_6.0,
    ];

    let friendly = side as usize;
    let hostile = 1 ^ friendly;

    let shifted = if friendly == WHITE {
        bb.sides[friendly].0 >> BOARD_FILES
    } else {
        bb.sides[friendly].0 << BOARD_FILES
    };

    let b = bb.pieces[friendly][PAWN].0 & (shifted | LOW_RANKS[friendly]);

    Bitboard(
        !(b | bb.pieces[friendly][KING].0
            | bb.pieces[friendly][QUEEN].0
            | bb.attacked_by[hostile][PAWN].0),
    )
}

#[cfg(test)]
#[allow(clippy::float_cmp)]
mod tests {
    use super::*;
    use crate::board::Chessboard;

    #[test]
    fn test_non_pawn_material() {
        let board = Chessboard::default();
        assert_eq!(non_pawn_material(&board.bitboards, Side::White), 8302);
        assert_eq!(non_pawn_material(&board.bitboards, Side::Black), 8302);

        let board = Chessboard::from_fen("rnbqkbnr/pppppppp/8/8/8/8/8/RNBQKBNR w KQkq - 0 1")
            .expect("Valid fen");
        assert_eq!(non_pawn_material(&board.bitboards, Side::White), 8302);
        assert_eq!(non_pawn_material(&board.bitboards, Side::Black), 8302);

        let board = Chessboard::from_fen("1n1qk1nr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1")
            .expect("Valid fen");
        assert_eq!(non_pawn_material(&board.bitboards, Side::White), 8302);
        assert_eq!(non_pawn_material(&board.bitboards, Side::Black), 5376);
    }

    #[test]
    fn test_scale_factor() {
        let board = Chessboard::from_fen("3qkb2/2pp1ppp/8/8/8/8/PPP1PP2/3QKB2 w - - 0 1")
            .expect("Valid fen");
        assert_eq!(
            scale_factor_for_side(Side::White, &board.position, &board.bitboards),
            46.0
        );

        let board =
            Chessboard::from_fen("4k3/2pP1ppp/3p4/8/8/8/PPP2P2/3QK3 b - - 0 1").expect("Valid fen");
        assert_eq!(
            scale_factor_for_side(Side::White, &board.position, &board.bitboards),
            37.0
        );

        let board = Chessboard::from_fen("1nb1k3/2pP1ppp/3p4/8/8/8/PPP2P2/3QK3 b - - 0 1")
            .expect("Valid fen");
        assert_eq!(
            scale_factor_for_side(Side::White, &board.position, &board.bitboards),
            43.0
        );

        let board = Chessboard::from_fen("2b1k3/1Pp1Pppp/3Pp3/3p4/8/8/PPP3PP/2B1K3 b - - 0 1")
            .expect("Valid fen");
        assert_eq!(
            scale_factor_for_side(Side::White, &board.position, &board.bitboards),
            38.0
        );
    }

    #[test]
    fn test_candidate_passed() {
        let board = Chessboard::from_fen("2b1k3/1Pp1Pp2/3Pp3/3p4/8/8/PPP3PP/2B1K3 b - - 0 1")
            .expect("Valid fen");
        assert_eq!(
            candidate_passed(Side::White, &board.position, &board.bitboards),
            5.0
        );
        assert_eq!(
            candidate_passed(Side::Black, &board.position, &board.bitboards),
            1.0
        );
    }

    #[test]
    fn test_psqt() {
        assert_eq!(psqt(PieceKind::Pawn, Side::White, Square::D4, false), 20);
        assert_eq!(psqt(PieceKind::King, Side::White, Square::D1, false), 198);
        assert_eq!(psqt(PieceKind::Bishop, Side::White, Square::C1, false), -8);
        assert_eq!(psqt(PieceKind::King, Side::Black, Square::E8, false), 198);
        assert_eq!(psqt(PieceKind::Rook, Side::Black, Square::D8, false), -5);
        assert_eq!(psqt(PieceKind::Bishop, Side::Black, Square::C1, false), -14);
        assert_eq!(psqt(PieceKind::Knight, Side::White, Square::C3, false), 6);

        let board = Chessboard::default();
        assert_eq!(total_psqt(&board.position, Side::White, false), 12);
        assert_eq!(total_psqt(&board.position, Side::Black, false), 12);

        let board = Chessboard::from_fen(
            "r1b1kb2/pp1p3p/2p2pp1/4p1q1/3P1P1r/2P3P1/PP2P2P/RNBQ1RK1 w q - 0 3",
        )
        .expect("Valid fen");
        assert_eq!(total_psqt(&board.position, Side::White, false), 256);
        assert_eq!(total_psqt(&board.position, Side::Black, false), 227);
    }
}
