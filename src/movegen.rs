#[allow(clippy::wildcard_imports)]
use crate::bitboards::constants::*;
use crate::bitboards::Bitboard;
use crate::piece::{PieceKind, Side, SIDE_COUNT};
use crate::position::{Position, PositionBitboards};
use crate::r#move::{Move, MoveKind};
use crate::square::{Square, BOARD_FILES, BOARD_RANKS};

include!(concat!(env!("OUT_DIR"), "/generated_move_tables.rs"));

pub(crate) fn generate_legal_moves(position: &Position, bb: &PositionBitboards) -> Vec<Move> {
    let side = position.side_to_move;
    let mut result = Vec::with_capacity(150);

    // If the halftime clock has expired, the game is over
    if position.halftime >= 50 {
        return result;
    }

    if bb.count_checkers() < 2 {
        generate_moves(PieceKind::Queen, bb, side, &mut result);
        generate_moves(PieceKind::Bishop, bb, side, &mut result);
        generate_moves(PieceKind::Knight, bb, side, &mut result);
        generate_moves(PieceKind::Rook, bb, side, &mut result);
        generate_pawn_moves(bb, side, position.en_passant, &mut result);
    }

    generate_king_moves(bb, side, position.castling, &mut result);

    result
}

pub(crate) fn get_attacks_bitboard(
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

pub(crate) fn find_attacks_on_square(
    kind: PieceKind,
    side: Side,
    on: Square,
    mut attackers: Bitboard,
    occupied: Bitboard,
) -> Bitboard {
    // Queens require some special fiddeling
    if kind == PieceKind::Queen {
        let as_rook = find_attacks_on_square(PieceKind::Rook, side, on, attackers, occupied);
        let as_bishop = find_attacks_on_square(PieceKind::Bishop, side, on, attackers, occupied);
        return as_rook | as_bishop;
    }

    let mut result = Bitboard(0);
    let from_defender = get_attacks_bitboard(kind, side, on, occupied);

    attackers &= from_defender;
    result |= attackers;

    while let Some(attacker) = attackers.pop_lsb_square() {
        result |= get_attacks_bitboard(kind, !side, attacker, occupied) & from_defender;
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
    while let Some(from) = pieces.pop_lsb_square() {
        let is_pinned_hv = bitboards.pinmask_hv[friendly].get(from);
        let is_pinned_d12 = bitboards.pinmask_d12[friendly].get(from);

        let pseudo_moves = get_attacks_bitboard(kind, side, from, bitboards.occupied);
        let mut legal_moves = pseudo_moves & bitboards.checkmask;

        if is_pinned_hv {
            legal_moves &= ROOK_MOVES.get(from, bitboards.occupied);
            legal_moves &= bitboards.pinmask_hv[friendly];
        } else if is_pinned_d12 {
            legal_moves &= BISHOP_MOVES.get(from, bitboards.occupied);
            legal_moves &= bitboards.pinmask_d12[friendly];
        }

        let quiets = legal_moves & !bitboards.occupied;
        let captures = legal_moves & bitboards.sides[hostile];

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

    let friendly = side as usize;
    let hostile = !side as usize;

    let ep_square = Bitboard(if let Some(offset) = ep {
        0b1 << offset as usize
    } else {
        0
    });

    let mut pawns = bitboards.pieces[friendly][PAWN];
    while let Some(from) = pawns.pop_lsb_square() {
        let promoting = PROMOTION_RANK[friendly].get(from);
        let is_pinned_hv = bitboards.pinmask_hv[friendly].get(from);
        let is_pinned_d12 = bitboards.pinmask_d12[friendly].get(from);

        let (mut moves, attacks) = PAWN_MOVES[friendly][from as usize];
        moves &= ROOK_MOVES.get(from, bitboards.occupied);

        let mut quiets = moves & !bitboards.occupied & bitboards.checkmask;
        let mut captures = attacks & bitboards.sides[hostile] & bitboards.checkmask;
        let mut ep_capture = attacks & ep_square;

        if is_pinned_hv {
            quiets &= bitboards.pinmask_hv[friendly];
            captures &= bitboards.pinmask_hv[friendly];
            ep_capture &= bitboards.pinmask_hv[friendly];
        } else if is_pinned_d12 {
            quiets &= bitboards.pinmask_d12[friendly];
            captures &= bitboards.pinmask_d12[friendly];
            ep_capture &= bitboards.pinmask_d12[friendly];
        }

        #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
        if let Some(ep) = ep_capture.lsb() {
            // If an en passant is possible, check that
            // it doesn't reveal a check
            let ep_pawn = (ep as isize + side.backward().offset()) as usize;

            let mut occupied = bitboards.occupied;
            occupied.off(ep_pawn);
            occupied.off(from);

            let king = bitboards.pieces[friendly][PieceKind::King as usize]
                .lsb_square()
                .unwrap();

            let hostile_queens = bitboards.pieces[hostile][PieceKind::Queen as usize];
            let hostile_bishops = bitboards.pieces[hostile][PieceKind::Bishop as usize];
            let hostile_rooks = bitboards.pieces[hostile][PieceKind::Rook as usize];

            let bishop_moves = BISHOP_MOVES.get(king, occupied);
            let rook_moves = ROOK_MOVES.get(king, occupied) & king.rank();

            if bishop_moves & (hostile_queens | hostile_bishops) != 0
                || rook_moves & (hostile_queens | hostile_rooks) != 0
            {
                ep_capture &= 0;
            }
        }

        if promoting {
            let all = quiets | captures;
            push_promotions(all, from, dest);
        } else {
            push_moves(MoveKind::Quiet, quiets, from, dest);
            push_moves(MoveKind::Capture, captures, from, dest);
            push_moves(MoveKind::EnPassant, ep_capture, from, dest);
        }
    }
}

fn generate_king_moves(
    bitboards: &PositionBitboards,
    side: Side,
    castling: [[bool; 2]; SIDE_COUNT],
    dest: &mut Vec<Move>,
) {
    let friendly = side as usize;
    let hostile = !side as usize;

    let mut kings = bitboards.pieces[friendly][PieceKind::King as usize];
    while let Some(from) = kings.pop_lsb_square() {
        let moves = get_attacks_bitboard(PieceKind::King, side, from, bitboards.occupied)
            & !bitboards.king_danger_squares[friendly];

        let quiets = moves & !bitboards.occupied;
        let captures = moves & bitboards.sides[hostile];

        push_moves(MoveKind::Quiet, quiets, from, dest);
        push_moves(MoveKind::Capture, captures, from, dest);

        if bitboards.checkers.pop_count() == 0 {
            const CASTLING_DESTS: [[Square; 2]; SIDE_COUNT] =
                [[Square::G1, Square::C1], [Square::G8, Square::C8]];

            for i in 0..2 {
                if can_castle(side, i, castling, bitboards) {
                    dest.push(Move {
                        from,
                        to: CASTLING_DESTS[friendly][i],
                        kind: MoveKind::Castling,
                    });
                }
            }
        }
    }
}

fn can_castle(
    side: Side,
    board_side: usize,
    permisions: [[bool; 2]; SIDE_COUNT],
    bitboards: &PositionBitboards,
) -> bool {
    const BLACK_OFFSET: usize = (BOARD_RANKS - 1) * BOARD_FILES;
    const REQUIRED_CLEARING: [[u64; 2]; SIDE_COUNT] = [
        [0b0110_0000, 0b0000_1110],
        [0b0110_0000 << BLACK_OFFSET, 0b0000_1110 << BLACK_OFFSET],
    ];
    const REQUIRED_CONTROL: [[u64; 2]; SIDE_COUNT] = [
        [0b0110_0000, 0b0000_1100],
        [0b0110_0000 << BLACK_OFFSET, 0b0000_1100 << BLACK_OFFSET],
    ];

    let friendly = side as usize;
    let hostile = !side as usize;

    permisions[friendly][board_side]
        && bitboards.occupied & REQUIRED_CLEARING[friendly][board_side] == 0
        && (bitboards.attacked[hostile] | bitboards.king_danger_squares[friendly])
            & REQUIRED_CONTROL[friendly][board_side]
            == 0
}

fn push_promotions(mut bb: Bitboard, from: Square, dest: &mut Vec<Move>) {
    while let Some(to) = bb.pop_lsb_square() {
        dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Queen)));
        dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Bishop)));
        dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Rook)));
        dest.push(Move::new(from, to, MoveKind::Promotion(PieceKind::Knight)));
    }
}

fn push_moves(kind: MoveKind, mut bb: Bitboard, from: Square, dest: &mut Vec<Move>) {
    while let Some(to) = bb.pop_lsb_square() {
        dest.push(Move::new(from, to, kind));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod movegen {
        use crate::board::Chessboard;
        use crate::fen::FEN_STARTING_POSITION;

        #[test]
        fn perft_test_startpos() {
            let mut chessboard = Chessboard::from_fen(FEN_STARTING_POSITION).unwrap();
            assert_eq!(chessboard.perft(1, false), 20);
            assert_eq!(chessboard.perft(2, false), 400);
        }

        #[test]
        fn perft_test_01() {
            let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 119_060_324);
        }

        #[test]
        fn perft_test_02() {
            let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5, false), 193_690_690);
        }

        #[test]
        fn perft_test_03() {
            let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(7, false), 178_633_661);
        }

        #[test]
        fn perft_test_04() {
            let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 0";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 706_045_033);
        }

        #[test]
        fn perft_test_05() {
            let fen = "1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5, false), 1_063_513);
        }

        #[test]
        fn perft_test_illegal_ep_1() {
            let fen = "3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 1_134_888);
        }

        #[test]
        fn perft_test_illegal_ep_2() {
            let fen = "8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 1_015_133);
        }

        #[test]
        fn perft_test_ep_capture_checks() {
            let fen = "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 1_440_467);
        }

        #[test]
        fn perft_test_short_castling_check() {
            let fen = "5k2/8/8/8/8/8/8/4K2R w K - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 661_072);
        }

        #[test]
        fn perft_test_long_castling_check() {
            let fen = "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 803_711);
        }

        #[test]
        fn perft_test_castle_rights() {
            let fen = "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4, false), 1_274_206);
        }

        #[test]
        fn perft_test_prevented_castle_rights() {
            let fen = "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4, false), 1_720_476);
        }

        #[test]
        fn perft_test_promote_out_of_check() {
            let fen = "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 3_821_001);
        }

        #[test]
        fn perft_test_discovered_check() {
            let fen = "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5, false), 1_004_658);
        }

        #[test]
        fn perft_test_promote_to_give_check() {
            let fen = "4k3/1P6/8/8/8/8/K7/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 217_342);
        }

        #[test]
        fn perft_test_under_promote_to_give_check() {
            let fen = "8/P1k5/K7/8/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 92683);
        }

        #[test]
        fn perft_test_self_stalemate() {
            let fen = "K1k5/8/P7/8/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(6, false), 2217);
        }

        #[test]
        fn perft_test_stalemate_and_checkmate_1() {
            let fen = "8/k1P5/8/1K6/8/8/8/8 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(7, false), 567_584);
        }

        #[test]
        fn perft_test_stalemate_and_checkmate_2() {
            let fen = "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(4, false), 23527);
        }

        #[test]
        fn perft_pinned_pawn_cant_capture() {
            let fen = "1n4k1/6pp/4R3/8/2P5/5r2/5KPq/RN6 w - - 0 1";
            let mut chessboard = Chessboard::from_fen(fen).unwrap();
            assert_eq!(chessboard.perft(5, false), 1_727_013);
        }
    }

    mod bitboard_generation {
        use crate::fen::FEN_STARTING_POSITION;
        use crate::piece::{PieceKind, Side};
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
