//! Move searching.
//!
//! This crate contains the implementation of the move search algorithm used to
//! find the best possible move for a position.

use crate::board::Chessboard;
use crate::evaluation;
use crate::position::Position;
use crate::r#move::{Move, MoveKind};
use itertools::Itertools;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Configurable move searcher for a [`Chessboard`].
pub struct MoveSearcher {
    board: Chessboard,
    depth: Option<usize>,
    time: Option<Duration>,
    debug: bool,

    transposition: HashMap<Position, i32>,
}

impl MoveSearcher {
    /// Create a new move searcher for the current position.
    #[must_use]
    pub fn new(board: &Chessboard) -> Self {
        Self {
            board: board.clone(),
            depth: None,
            time: None,
            debug: false,
            transposition: HashMap::new(),
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

    /// If the search should print its progress to the console.
    ///
    /// Initially set to `false`.
    ///
    /// # Arguments
    ///
    /// * `debug` - Whether to enable debugging.
    #[must_use]
    pub fn debug(mut self, debug: bool) -> Self {
        self.debug = debug;
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
            self.transposition.clear();

            for (i, &m) in moves.iter().enumerate() {
                self.board.make_move(m).expect("All moves should be legal");
                scores[i] = -self.mtdf(-scores[i], depth);
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

            if self.debug {
                println!(
                    "Search: Depth: {depth}, Move: {}, Score: {}",
                    best_iteration_move.map_or(String::from("None"), |m| m.to_string()),
                    best_iteration_score
                );
            }

            best = best_iteration_move;
        }

        best
    }

    fn score_move(m: Move) -> i32 {
        let mut score = 0;

        if matches!(m.kind, MoveKind::Capture | MoveKind::Promotion(_)) {
            score += 100;
        }

        if let MoveKind::Promotion(to) = m.kind {
            score += evaluation::hce::piece_value(to, false);
        }

        -score
    }

    fn mtdf(&mut self, mut f: i32, depth: usize) -> i32 {
        let mut upper = i32::MAX;
        let mut lower = -i32::MAX;

        while lower < upper {
            let beta = f.max(lower + 1);

            f = self.alpha_beta(depth, beta - 1, beta);

            if f < beta {
                upper = f;
            } else {
                lower = f;
            }
        }

        f
    }

    fn alpha_beta(&mut self, depth: usize, mut alpha: i32, beta: i32) -> i32 {
        if let Some(value) = self.transposition.get(&self.board.position) {
            return *value;
        }

        if depth == 0 {
            return self.quiesce(alpha, beta);
        }

        let mut moves = self.board.moves().clone();
        moves.sort_by_key(|m| Self::score_move(*m));

        for m in moves {
            self.board.make_move(m).expect("All moves should be legal");
            let score = -self.alpha_beta(depth - 1, -beta, -alpha);
            self.transposition.insert(self.board.position, score);
            self.board.undo();

            if score >= beta {
                return beta;
            }
            alpha = alpha.max(score);
        }

        alpha
    }

    fn quiesce(&mut self, mut alpha: i32, beta: i32) -> i32 {
        if let Some(score) = self.transposition.get(&self.board.position) {
            return *score;
        }

        let evaluation = self.board.evaluate_relative();
        if evaluation >= beta {
            return beta;
        }
        alpha = alpha.max(evaluation);

        let moves: Vec<_> = self
            .board
            .moves()
            .clone()
            .into_iter()
            .filter(|m| matches!(m.kind, MoveKind::Capture | MoveKind::Promotion(_)))
            .sorted_by_key(|m| Self::score_move(*m))
            .collect();

        #[allow(clippy::cast_possible_wrap)]
        if moves.is_empty() {
            return evaluation + self.board.position.fulltime as i32;
        }

        for m in moves {
            self.board.make_move(m).expect("All moves should be legal");
            let score = -self.quiesce(-beta, -alpha);
            self.transposition.insert(self.board.position, score);
            self.board.undo();

            if score >= beta {
                return beta;
            }
            alpha = alpha.max(score);
        }

        alpha
    }
}
