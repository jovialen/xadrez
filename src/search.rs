//! Move searching.
//!
//! This crate contains the implementation of the move search algorithm used to
//! find the best possible move for a position.

use crate::board::Chessboard;
use crate::position::Position;
use crate::r#move::{Move, MoveKind};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Configurable move searcher for a [`Chessboard`].
pub struct MoveSearcher {
    board: Chessboard,
    depth: Option<usize>,
    time: Option<Duration>,
}

impl MoveSearcher {
    /// Create a new move searcher for the current position.
    #[must_use]
    pub fn new(board: &Chessboard) -> Self {
        Self {
            board: board.clone(),
            depth: None,
            time: None,
        }
    }

    /// Set the max depth of the search.
    ///
    /// If no max depth is set, then the search will have no max depth at all.
    ///
    /// # Arguments
    ///
    /// * `depth` - Max depth to search to.
    #[must_use]
    pub fn max_depth(mut self, depth: usize) -> Self {
        self.depth = Some(depth);
        self
    }

    /// Set the max time the search can take.
    ///
    /// If no max time is set, then the search will not stop after any time.
    ///
    /// # Arguments
    ///
    /// * `time` - Max length of the search.
    #[must_use]
    pub fn max_time(mut self, time: Duration) -> Self {
        self.time = Some(time);
        self
    }

    /// Search for the best possible move.
    #[must_use]
    pub fn search(mut self) -> Option<Move> {
        let max_depth = self.depth.unwrap_or(usize::MAX).max(1);
        let start_time = Instant::now();

        let moves = self.board.moves().clone();
        let mut scores = vec![0; moves.len()];

        let mut best = None;
        'search: for depth in 0..max_depth {
            let (mut best_iteration_move, mut best_iteration_score) = (None, -i32::MAX);
            let mut transposition = HashMap::new();

            for (i, &m) in moves.iter().enumerate() {
                self.board.make_move(m).expect("All moves should be legal");
                scores[i] = -mtdf(&mut self.board, &mut transposition, scores[i], depth);
                self.board.undo();

                if scores[i] > best_iteration_score {
                    best_iteration_score = scores[i];
                    best_iteration_move = Some(m);
                }

                if let Some(max_time) = self.time {
                    if start_time.elapsed() > max_time {
                        break 'search;
                    }
                }
            }

            println!("{depth}");
            best = best_iteration_move;
        }

        best
    }
}

fn mtdf(
    board: &mut Chessboard,
    transposition: &mut HashMap<Position, i32>,
    mut f: i32,
    depth: usize,
) -> i32 {
    if depth == 0 {
        return quiesce(board, transposition, -i32::MAX, i32::MAX);
    }

    let mut upper = i32::MAX;
    let mut lower = -i32::MAX;

    while lower < upper {
        let beta = f.max(lower + 1);

        f = -alpha_beta(board, transposition, depth, beta - 1, beta);

        if f < beta {
            upper = f;
        } else {
            lower = f;
        }
    }

    f
}

fn alpha_beta(
    board: &mut Chessboard,
    transposition: &mut HashMap<Position, i32>,
    depth: usize,
    mut alpha: i32,
    beta: i32,
) -> i32 {
    if let Some(value) = transposition.get(&board.position) {
        return *value;
    }

    if depth == 0 {
        return quiesce(board, transposition, -beta, -alpha);
    }

    let moves = board.moves().clone();
    for m in moves {
        board.make_move(m).expect("All moves should be legal");
        let score = -alpha_beta(board, transposition, depth - 1, -beta, -alpha);
        transposition.insert(board.position, score);
        board.undo();

        if score >= beta {
            return beta;
        }
        alpha = alpha.max(score);
    }

    alpha
}

fn quiesce(
    board: &mut Chessboard,
    transposition: &mut HashMap<Position, i32>,
    mut alpha: i32,
    beta: i32,
) -> i32 {
    if let Some(score) = transposition.get(&board.position) {
        return *score;
    }

    let moves: Vec<_> = board
        .moves()
        .clone()
        .into_iter()
        .filter(|m| matches!(m.kind, MoveKind::Capture | MoveKind::Promotion(_)))
        .collect();

    if moves.is_empty() {
        return board.evaluate_relative();
    }

    for m in moves {
        board.make_move(m).expect("All moves should be legal");
        let score = -quiesce(board, transposition, -beta, -alpha);
        board.undo();

        if score >= beta {
            return beta;
        }
        alpha = alpha.max(score);
    }

    alpha
}
