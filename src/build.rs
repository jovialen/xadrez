#![allow(unused)]

mod gen_move_tables;
mod piece;
mod square;

// gen_move_tables depends on these modules
mod bitboards;
mod error;

use gen_move_tables::{init_bishop_moves, init_pawn_moves, init_pseudo_attacks, init_rook_moves};
use piece::PIECE_KIND_COUNT;
use rand::random;
use square::{BOARD_FILES, BOARD_SIZE};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn generate_move_tables() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=./gen_move_tables.rs");

    let out_path = format!(
        "{}/{}",
        env::var("OUT_DIR").unwrap(),
        "generated_move_tables.rs"
    );
    let mut out = File::create(Path::new(out_path.as_str()))?;

    let pseudo_attacks = init_pseudo_attacks();
    let pawn_moves = init_pawn_moves();
    let bishop_moves = init_bishop_moves();
    let rook_moves = init_rook_moves();

    // Include dependencies
    writeln!(
        out,
        "use crate::gen_move_tables::{{PieceMoveBitboards, PawnMoveBitboards, MagicTable, Magic}};"
    );
    writeln!(out, "use crate::bitboards::Bitboard;");

    // Write move tables
    writeln!(
        out,
        "pub(super) const PSEUDO_ATTACKS: PieceMoveBitboards = {:?};",
        pseudo_attacks
    )?;
    writeln!(
        out,
        "pub(super) const PAWN_MOVES: PawnMoveBitboards = {:?};",
        pawn_moves
    )?;
    writeln!(
        out,
        "pub(super) const BISHOP_MOVES: MagicTable<512> = {:?};",
        bishop_moves
    )?;
    writeln!(
        out,
        "pub(super) const ROOK_MOVES: MagicTable<4096> = {:?};",
        rook_moves
    )?;

    Ok(())
}

fn generate_zobrist_hashes() -> std::io::Result<()> {
    let out_path = format!(
        "{}/{}",
        env::var("OUT_DIR").unwrap(),
        "generated_zobrist_hashes.rs"
    );
    let mut out = File::create(Path::new(out_path.as_str()))?;

    let zobrist_piece_nums = init_rand_nums::<u64, { BOARD_SIZE * PIECE_KIND_COUNT * 2 }>();
    let zobrist_black_move: u64 = rand::random::<u64>();
    let zobrist_castling: [u64; 4] = init_rand_nums::<u64, 4>();
    let zobrist_en_passant: [u64; BOARD_FILES] = init_rand_nums::<u64, BOARD_FILES>();

    // Include dependencies
    writeln!(out, "use crate::square::{{BOARD_SIZE, BOARD_FILES}};");
    writeln!(out, "use crate::piece::PIECE_KIND_COUNT;");

    // Write hashes
    writeln!(
        out,
        "pub(super) const ZOBRIST_PIECE_NUMS: [u64; BOARD_SIZE * PIECE_KIND_COUNT * 2] = {:?};",
        zobrist_piece_nums
    )?;
    writeln!(
        out,
        "pub(super) const ZOBRIST_BLACK_MOVE: u64 = {};",
        zobrist_black_move
    )?;
    writeln!(
        out,
        "pub(super) const ZOBRIST_CASTLING: [u64; 4] = {:?};",
        zobrist_castling
    )?;
    writeln!(
        out,
        "pub(super) const ZOBRIST_EN_PASSANT: [u64; BOARD_FILES] = {:?};",
        zobrist_en_passant
    )?;

    Ok(())
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

fn main() {
    generate_move_tables().expect("Failed to generate move tables");
    generate_zobrist_hashes().expect("Failed to generate zobrist hashes");
}
