//! Move searching.
//!
//! This crate contains the implementation of the move search algorithm used to
//! find the best possible move for a position.

use crate::board::Chessboard;
use crate::evaluation;
use crate::piece::PieceKind;
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

/// Output data from a move search with the [`MoveSearcher::search`].
#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(clippy::module_name_repetitions)]
pub struct SearchData {
    /// The best move found in the search.
    ///
    /// Will be [`None`] if no move is found.
    pub best_move: Option<Move>,
    /// The score given to the best move.
    ///
    /// The score is the expected relative evaluation [`SearchData::depth`]
    /// moves in the future.
    pub score: i32,
    /// How many moves into the future the search looked.
    pub depth: usize,
    /// When the search started.
    pub start_time: Instant,
    /// How long the search lasted.
    pub duration: Duration,
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
    pub fn search(mut self) -> SearchData {
        let max_depth = self.depth.unwrap_or(usize::MAX).max(1);

        let mut result = SearchData::new();

        let moves = self.board.moves().clone();
        let mut scores = vec![0; moves.len()];

        if moves.is_empty() {
            return result;
        }

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
                    if result.start_time.elapsed() > max_time {
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

            if best_iteration_move.is_some() {
                result.best_move = best_iteration_move;
                result.score = best_iteration_score;

                // If checkmate is found
                if best_iteration_score >= 10_000_000 {
                    break 'search;
                }
            }
            result.depth = depth;
        }

        result.duration = result.start_time.elapsed();

        result
    }

    fn score_move(&self, m: Move) -> i32 {
        let friendly = self.board.position.side_to_move as usize;
        let hostile = 1 ^ friendly;

        let piece = self.board.position[m.from].unwrap();

        let mut score = 0;

        if let Some(capture) = self.board.position[m.to] {
            let exchange = evaluation::hce::piece_value(capture.kind, false)
                - evaluation::hce::piece_value(piece.kind, false);
            score += 10 * exchange;
        }

        if let MoveKind::Promotion(to) = m.kind {
            score += evaluation::hce::piece_value(to, false);
        }

        if self.board.bitboards.attacked_by[hostile][PieceKind::Pawn as usize].get(m.to) {
            score -= 350;
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
        moves.sort_by_key(|m| self.score_move(*m));

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
            .sorted_by_key(|m| self.score_move(*m))
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

impl SearchData {
    pub(crate) fn new() -> Self {
        Self {
            best_move: None,
            score: 0,
            depth: 0,
            start_time: Instant::now(),
            duration: Duration::default(),
        }
    }
}
