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

    data: SearchData,
    transposition: HashMap<Position, TranspositionEntry>,
}

#[derive(Default)]
struct TranspositionEntry {
    at_depth: usize,
    score: i32,
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

    /// How many nodes where searched.
    pub nodes: usize,
    /// How many times the transposition table was used instead of evaluating
    /// the branch.
    pub transposition_hits: usize,
    /// How many branches where pruned from the search.
    pub prunes: usize,
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
            data: SearchData::new(),
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

        let moves = self.board.moves().clone();
        if moves.is_empty() {
            return self.data;
        }

        'search: for depth in 0..max_depth {
            let (mut best_iteration_move, mut best_iteration_score) = (None, -i32::MAX);

            for &m in &moves {
                self.board.make_move(m).expect("All moves should be legal");
                let score = -self.alpha_beta(depth, -i32::MAX, i32::MAX);
                self.board.undo();

                if score > best_iteration_score {
                    best_iteration_score = score;
                    best_iteration_move = Some(m);
                }

                if let Some(max_time) = self.time {
                    if self.data.start_time.elapsed() > max_time {
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
                self.data.best_move = best_iteration_move;
                self.data.score = best_iteration_score;

                // If forced checkmate is found
                if best_iteration_score.abs() >= 10_000_000 {
                    break 'search;
                }
            }
            self.data.depth = depth;
        }

        self.data.duration = self.data.start_time.elapsed();

        self.data
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

    fn alpha_beta(&mut self, depth: usize, mut alpha: i32, beta: i32) -> i32 {
        if let Some(entry) = self.transposition.get(&self.board.position) {
            if entry.at_depth >= depth {
                self.data.transposition_hits += 1;
                return alpha.max(beta.min(entry.score));
            }
        }

        let mut moves = self.board.moves().clone();

        if depth == 0 || moves.is_empty() {
            return self.quiesce(alpha, beta);
        }

        moves.sort_by_key(|m| self.score_move(*m));

        for m in moves {
            self.data.nodes += 1;

            self.board.make_move(m).expect("All moves should be legal");
            let score = -self.alpha_beta(depth - 1, -beta, -alpha);
            self.board.undo();

            if score >= beta {
                self.data.prunes += 1;
                self.transposition
                    .insert(self.board.position, TranspositionEntry::new(depth, beta));
                return beta;
            }
            alpha = alpha.max(score);
        }

        self.transposition
            .insert(self.board.position, TranspositionEntry::new(depth, alpha));
        alpha
    }

    fn quiesce(&mut self, mut alpha: i32, beta: i32) -> i32 {
        let evaluation = self.board.evaluate_relative();
        if evaluation >= beta {
            self.data.prunes += 1;
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
            self.data.nodes += 1;

            self.board.make_move(m).expect("All moves should be legal");
            let score = -self.quiesce(-beta, -alpha);
            self.board.undo();

            if score >= beta {
                self.data.prunes += 1;
                return beta;
            }
            alpha = alpha.max(score);
        }

        alpha
    }
}

impl TranspositionEntry {
    fn new(at_depth: usize, score: i32) -> Self {
        Self { at_depth, score }
    }
}

impl SearchData {
    fn new() -> Self {
        Self {
            best_move: None,
            score: 0,
            depth: 0,
            start_time: Instant::now(),
            duration: Duration::default(),
            nodes: 0,
            transposition_hits: 0,
            prunes: 0,
        }
    }
}
