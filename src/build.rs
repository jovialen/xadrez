#![allow(unused)]

mod gen_move_tables;

// gen_move_tables depends on these modules
mod bitboards;
mod error;
mod piece;
mod square;

use gen_move_tables::{init_bishop_moves, init_pawn_moves, init_pseudo_attacks, init_rook_moves};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn generate_move_tables() -> std::io::Result<()> {
    let out_path = format!(
        "{}/{}",
        env::var("OUT_DIR").unwrap(),
        "generated_move_tables.rs"
    );
    let mut out = File::create(Path::new(out_path.as_str()))?;

    // Include dependencies
    writeln!(
        out,
        "use crate::gen_move_tables::{{PieceMoveBitboards, PawnMoveBitboards, MagicTable, Magic}};"
    );
    writeln!(
        out,
        "use crate::bitboards::Bitboard;"
    );

    {
        println!("Generating pseudo attacks");
        let pseudo_attacks = init_pseudo_attacks();

        writeln!(
            out,
            "pub(super) const PSEUDO_ATTACKS: PieceMoveBitboards = {:?};",
            pseudo_attacks
        )?;
    }

    {
        println!("Generating pawn moves");
        let pawn_moves = init_pawn_moves();

        writeln!(
            out,
            "pub(super) const PAWN_MOVES: PawnMoveBitboards = {:?};",
            pawn_moves
        )?;
    }

    {
        println!("Generating bishop moves");
        let bishop_moves = init_bishop_moves();
        writeln!(
            out,
            "pub(super) const BISHOP_MOVES: MagicTable<512> = {:?};",
            bishop_moves
        )?;
    }

    {
        println!("Generating rook moves");
        let rook_moves = init_rook_moves();

        writeln!(
            out,
            "pub(super) const ROOK_MOVES: MagicTable<4096> = {:?};",
            rook_moves
        )?;
    }

    Ok(())
}

fn main() {
    generate_move_tables().expect("Failed to generate move tables");
}
