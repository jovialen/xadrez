// None of this is meant to be pretty or... good. Just a debug tool.

use itertools::Itertools;
use std::io;
use std::str::FromStr;
use std::time::Duration;
use xadrez::board::{Chessboard, GameState};
use xadrez::error::ParseFenError;
use xadrez::fen::FEN_STARTING_POSITION;
use xadrez::r#move::Move;
use xadrez::search::SearchLimits;

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
    let move_square_char = args.next().unwrap_or(" ");

    println!("    A   B   C   D   E   F   G   H  ");
    println!("  +---+---+---+---+---+---+---+---+");
    for (rank_index, rank) in board.squares().chunks(8).enumerate().rev() {
        print!("{} |", rank_index + 1);
        for (square, content) in rank {
            let square_str = if let Some(piece) = content {
                piece.to_string()
            } else if board.moves().iter().any(|m| m.to == *square) {
                move_square_char.to_string()
            } else {
                " ".to_string()
            };

            print!(" {} |", square_str);
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
    println!("\nNodes searched: {}", board.perft(depth, true));
}

fn print_moves<'a, I: Iterator<Item = &'a str>>(board: &Chessboard, mut args: I) {
    let filter = args.next().unwrap_or("");
    for m in board
        .moves()
        .iter()
        .filter(|m| m.to_string().contains(filter))
    {
        println!("{}", m);
    }
}

fn search<'a, I: Iterator<Item = &'a str>>(board: &Chessboard, mut args: I) {
    let time_ms = args.next().and_then(|x| x.parse().ok()).unwrap_or(1000);
    let depth = args
        .next()
        .and_then(|x| x.parse().ok())
        .unwrap_or(usize::MAX);

    let limits = SearchLimits {
        max_time: Some(Duration::from_millis(time_ms)),
        max_depth: Some(depth),
        ..Default::default()
    };

    let search_result = board.search(limits);

    if let Ok((best, search_data)) = search_result {
        println!(
            "Best move: {best} (Score: {}, Depth: {}, Nodes: {}, QNodes: {}, Transposition Hits: {}, Prunes: {}, Reductions: {})",
            search_data.score,
            search_data.depth,
            search_data.nodes,
            search_data.qnodes,
            search_data.transposition_hits,
            search_data.prunes,
            search_data.reductions,
        );
    } else {
        println!("Failed to find a move. Try giving more time to search.");
    }
}

fn play<'a, I: Iterator<Item = &'a str>>(board: &mut xadrez::board::Chessboard, mut args: I) {
    let rounds = args.next().and_then(|x| x.parse().ok()).unwrap_or(1);
    let time_ms = args.next().and_then(|x| x.parse().ok()).unwrap_or(1000);
    let depth = args
        .next()
        .and_then(|x| x.parse().ok())
        .unwrap_or(usize::MAX);

    let limits = SearchLimits {
        max_time: Some(Duration::from_millis(time_ms)),
        max_depth: Some(depth),
        ..Default::default()
    };

    for _ in 0..rounds {
        if board.game_state() != GameState::Playing {
            println!("{}", board.game_state());
            return;
        }

        let search_result = board.search(limits);

        if let Ok((best, search_data)) = search_result {
            println!("{best} ({})", search_data.depth);

            board
                .make_move(best)
                .expect("All found moves should be valid");
        } else {
            println!("Can't make a move. {}", board.game_state());
            return;
        }
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
            Some("undo") => chessboard.undo(),
            Some("evaluate") => println!("{}", chessboard.evaluate()),
            Some("state") => println!("{}", chessboard.game_state()),
            Some("search") => search(&chessboard, tokens),
            Some("play") => play(&mut chessboard, tokens),
            Some(cmd) => eprintln!("Error: Unknown command \"{}\"", cmd),
            None => continue,
        }
    }
}
