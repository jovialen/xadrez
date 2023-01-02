use itertools::Itertools;
use std::{io, str::FromStr};
use xadrez::{
    board::Chessboard,
    error::{ParseFenError, ParseLANError},
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

fn do_move<'a, I: Iterator<Item = &'a str>>(
    _board: &mut Chessboard,
    mut args: I,
) -> Result<(), ParseLANError> {
    let _m = Move::from_str(args.next().unwrap_or(""))?;
    todo!("Carry out move on board");
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
            Some("quit") | Some("exit") => break,
            Some("position") => print_if_err!(position(&mut chessboard, tokens)),
            Some("do") => print_if_err!(do_move(&mut chessboard, tokens)),
            Some(cmd) => eprintln!("Error: Unknown command \"{}\"", cmd),
            None => continue,
        }
    }
}
