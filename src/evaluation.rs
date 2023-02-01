use crate::board::Position;
use crate::piece::{Piece, PieceKind, Side};
use binread::BinRead;
use lazy_static::lazy_static;
use nnue::stockfish::halfkp::{SfHalfKpFullModel, SfHalfKpState};
use std::io::Cursor;

lazy_static! {
    static ref EVALUATOR: SfHalfKpFullModel =
        create_evaluator(include_bytes!("nn-62ef826d1a6d.nnue"));
}

pub(crate) fn evaluate_position(position: &Position) -> i32 {
    let mut state = create_state(position).expect("Cannot evaluate invalid position.");

    let side_to_move = nnue_side(position.side_to_move);

    state.activate(side_to_move)[0] / 16
}

fn create_state(position: &Position) -> Option<SfHalfKpState> {
    let mut white_king = None;
    let mut black_king = None;

    for (square, content) in position.squares.iter().enumerate() {
        if let Some(piece) = content {
            match piece {
                Piece {
                    kind: PieceKind::King,
                    side: Side::White,
                } => white_king = nnue_square(square),
                Piece {
                    kind: PieceKind::King,
                    side: Side::Black,
                } => black_king = nnue_square(square),
                _ => (),
            }
        }
    }

    let mut state = EVALUATOR.model.new_state(white_king?, black_king?);

    for (square, content) in position.squares.iter().enumerate() {
        if let Some(piece) = content {
            if piece.kind == PieceKind::King {
                continue;
            }

            state.add(
                nnue_side(Side::White),
                nnue_piece(piece.kind),
                nnue_side(piece.side),
                nnue_square(square)?,
            );
            state.add(
                nnue_side(Side::Black),
                nnue_piece(piece.kind),
                nnue_side(piece.side),
                nnue_square(square)?,
            );
        }
    }

    Some(state)
}

fn create_evaluator(network: &[u8]) -> SfHalfKpFullModel {
    let mut reader = Cursor::new(network);
    SfHalfKpFullModel::read(&mut reader).expect("Failed to create NNUE network model.")
}

fn nnue_side(side: Side) -> nnue::Color {
    use nnue::Color;

    match side {
        Side::White => Color::White,
        Side::Black => Color::Black,
    }
}

fn nnue_piece(kind: PieceKind) -> nnue::Piece {
    use nnue::Piece;

    match kind {
        PieceKind::King => Piece::King,
        PieceKind::Queen => Piece::Queen,
        PieceKind::Bishop => Piece::Bishop,
        PieceKind::Knight => Piece::Knight,
        PieceKind::Rook => Piece::Rook,
        PieceKind::Pawn => Piece::Pawn,
    }
}

fn nnue_square(i: usize) -> Option<nnue::Square> {
    use nnue::Square;

    match i {
        0 => Some(Square::A1),
        1 => Some(Square::B1),
        2 => Some(Square::C1),
        3 => Some(Square::D1),
        4 => Some(Square::E1),
        5 => Some(Square::F1),
        6 => Some(Square::G1),
        7 => Some(Square::H1),
        8 => Some(Square::A2),
        9 => Some(Square::B2),
        10 => Some(Square::C2),
        11 => Some(Square::D2),
        12 => Some(Square::E2),
        13 => Some(Square::F2),
        14 => Some(Square::G2),
        15 => Some(Square::H2),
        16 => Some(Square::A3),
        17 => Some(Square::B3),
        18 => Some(Square::C3),
        19 => Some(Square::D3),
        20 => Some(Square::E3),
        21 => Some(Square::F3),
        22 => Some(Square::G3),
        23 => Some(Square::H3),
        24 => Some(Square::A4),
        25 => Some(Square::B4),
        26 => Some(Square::C4),
        27 => Some(Square::D4),
        28 => Some(Square::E4),
        29 => Some(Square::F4),
        30 => Some(Square::G4),
        31 => Some(Square::H4),
        32 => Some(Square::A5),
        33 => Some(Square::B5),
        34 => Some(Square::C5),
        35 => Some(Square::D5),
        36 => Some(Square::E5),
        37 => Some(Square::F5),
        38 => Some(Square::G5),
        39 => Some(Square::H5),
        40 => Some(Square::A6),
        41 => Some(Square::B6),
        42 => Some(Square::C6),
        43 => Some(Square::D6),
        44 => Some(Square::E6),
        45 => Some(Square::F6),
        46 => Some(Square::G6),
        47 => Some(Square::H6),
        48 => Some(Square::A7),
        49 => Some(Square::B7),
        50 => Some(Square::C7),
        51 => Some(Square::D7),
        52 => Some(Square::E7),
        53 => Some(Square::F7),
        54 => Some(Square::G7),
        55 => Some(Square::H7),
        56 => Some(Square::A8),
        57 => Some(Square::B8),
        58 => Some(Square::C8),
        59 => Some(Square::D8),
        60 => Some(Square::E8),
        61 => Some(Square::F8),
        62 => Some(Square::G8),
        63 => Some(Square::H8),
        _ => None,
    }
}
