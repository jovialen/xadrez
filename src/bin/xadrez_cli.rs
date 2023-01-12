// None of this is meant to be pretty or... good. Just a debug tool.

use itertools::Itertools;
use std::{io, str::FromStr};
use xadrez::{
    board::{Chessboard, Square},
    error::ParseFenError,
    fen::FEN_STARTING_POSITION,
    r#move::Move,
};

fn position<'a, I: Iterator<Item = &'a str>>(
    board: &mut Chessboard,
    mut args: I,
) -> Result<(), ParseFenError> {
    match args.next() {
        Some("startpos") => board.set_position(FEN_STARTING_POSITION)?,
        Some("fen") => board.set_position(&args.join(" "))?,
        Some(cmd) => eprintln!("Error: No such subcommand \"{}\"", cmd),
        _ => (),
    }

    println!("{}", board);

    Ok(())
}

fn pretty_print_position<'a, I: Iterator<Item = &'a str>>(board: &Chessboard, mut args: I) {
    let attack_square_char = args.next().unwrap_or(" ");

    println!("    A   B   C   D   E   F   G   H  ");
    println!("  +---+---+---+---+---+---+---+---+");
    for (rr, rank) in board.squares().chunks(8).enumerate().rev() {
        print!("{} |", rr + 1);
        for (f, piece) in rank.iter().enumerate() {
            let square = Square::from_rank_file(rr, f).unwrap();
            print!(
                " {} |",
                match piece {
                    Some(p) => p.to_string(),
                    None if board.moves().into_iter().any(|m| m.to == square) =>
                        attack_square_char.to_string(),
                    _ => " ".to_string(),
                }
            );
        }
        println!("\n  +---+---+---+---+---+---+---+---+");
    }
    println!("\nPosition: {}", board);
}

fn do_move<'a, I: Iterator<Item = &'a str>>(
    board: &mut Chessboard,
    mut args: I,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let m = Move::from_str(args.next().unwrap_or(""))?;
    board.make_move(m)?;
    Ok(())
}

fn perft<'a, I: Iterator<Item = &'a str>>(board: &mut Chessboard, mut args: I) {
    let depth = args
        .next()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(4);
    println!("Perft result: {}", board.perft(depth));
}

fn print_moves<'a, I: Iterator<Item = &'a str>>(board: &Chessboard, mut args: I) {
    let filter = args.next().unwrap_or("");
    for m in board
        .moves()
        .into_iter()
        .filter(|m| m.to_string().contains(filter))
    {
        println!("{}", m);
    }
}

macro_rules! print_if_err {
    ($res: expr) => {
        if let Err(err) = $res {
            eprintln!("Error: {}", err);
        }
    };
}

fn main() {
    let mut stdin = io::stdin().lines();
    let mut chessboard = Chessboard::default();
    println!("{}", chessboard);

    while let Some(Ok(line)) = stdin.next() {
        let mut tokens = line.split_whitespace();

        match tokens.next() {
            Some("quit") | Some("exit") | Some("q") => break,
            Some("position") => print_if_err!(position(&mut chessboard, tokens)),
            Some("display") | Some("d") => pretty_print_position(&chessboard, tokens),
            Some("move") | Some("m") => print_if_err!(do_move(&mut chessboard, tokens)),
            Some("moves") => print_moves(&chessboard, tokens),
            Some("perft") => perft(&mut chessboard, tokens),
            Some(cmd) => eprintln!("Error: Unknown command \"{}\"", cmd),
            None => continue,
        }
    }
}
