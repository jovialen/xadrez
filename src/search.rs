//! Move searching.
//!
//! This crate contains the implementation of the move search algorithm used to
//! find the best possible move for a position.

use crate::board::Chessboard;
use crate::evaluation;
use crate::evaluation::score::Score;
use crate::piece::PieceKind;
use crate::position::Position;
use crate::r#move::{Move, MoveKind};
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::time::{Duration, Instant};

/// Configurable move searcher for a [`Chessboard`].
pub struct MoveSearcher {
    board: Chessboard,
    depth: Option<usize>,
    time: Option<Duration>,
    debug: bool,

    razoring_depth: usize,
    razoring_margin: i32,

    mc_reduction: usize,
    mc_moves: usize,
    mc_limit: usize,

    data: SearchData,
    transposition: FxHashMap<Position, TranspositionEntry>,
}

struct TranspositionEntry {
    at_depth: usize,
    score: Score,
    node_type: NodeType,
    best_move: Option<Move>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum NodeType {
    Pv,
    Cut,
    All,
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
    pub score: Score,
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

            razoring_depth: 3,
            razoring_margin: 300,

            mc_reduction: 1,
            mc_moves: 6,
            mc_limit: 3,

            data: SearchData::new(),
            transposition: FxHashMap::default(),
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

    /// Configure the razoring.
    ///
    /// Razoring works by checking if low-depth nodes are bad enough to consider
    /// pruning, without being in check. If razoring is triggered, a quiesce
    /// search will check if the position can be turned around.
    ///
    /// Initially the searcher will start checking for razoring at depth `3`
    /// with a `300` centi-pawn margin.
    ///
    /// # Arguments
    ///
    /// * `depth` - At which depth razoring gets enabled. Set to `0` to disable.
    /// * `margin` - How far below `beta` the current position has to be to
    ///   trigger razoring in centi-pawns.
    #[must_use]
    pub fn razoring(mut self, depth: usize, margin: i32) -> Self {
        self.razoring_depth = depth;
        self.razoring_margin = margin;
        self
    }

    /// Configure the multi-cut pruning.
    ///
    /// Prune some cut nodes by returning early by searching the first `moves`
    /// children of the node to a reduced depth (`reduction`). If more than
    /// `limit` children trigger a beta cutoff, prune the entire tree.
    ///
    /// Initially the searcher will search `6` nodes to a reduced depth of `1`,
    /// and prune the tree if more than `3` children trigger a cutoff.
    ///
    /// # Arguments
    ///
    /// * `moves` - Number of moves to look at when checking for multi-cut
    ///   prune. Set to `0` to disable multi-cutting.
    /// * `limit` - Number of cutoffs to trigger a multi-cut prune.
    /// * `reduction` - Depth reduction for search in multi-cut prune.
    #[must_use]
    pub fn multi_cut(mut self, moves: usize, limit: usize, reduction: usize) -> Self {
        self.mc_moves = moves;
        self.mc_limit = limit;
        self.mc_reduction = reduction;
        self
    }

    /// Search for the best possible move.
    #[must_use]
    pub fn search(mut self) -> SearchData {
        let max_depth = self.depth.unwrap_or(usize::MAX).max(1);
        let side = self.board.position.side_to_move;

        let moves = self.board.moves().clone();
        if moves.is_empty() {
            return self.data;
        }

        'search: for depth in 0..max_depth {
            let (mut best_iteration_move, mut best_iteration_score) = (None, Score::min(side));

            for &m in &moves {
                self.board.make_move(m).expect("All moves should be legal");
                let score = -self.pv_search(Score::min(!side), Score::max(!side), depth, 1);
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
                if best_iteration_score.is_mate() {
                    break 'search;
                }
            }

            self.data.depth = depth;
        }

        self.data.duration = self.data.start_time.elapsed();

        self.data
    }

    #[inline]
    fn exchange(&self, m: Move) -> i32 {
        let piece = self.board.position[m.from].unwrap();

        if let Some(capture) = self.board.position[m.to] {
            evaluation::hce::piece_value(capture.kind, false)
                - evaluation::hce::piece_value(piece.kind, false)
        } else {
            0
        }
    }

    fn score_move(&self, m: Move) -> i32 {
        let friendly = self.board.position.side_to_move as usize;
        let hostile = 1 ^ friendly;

        let mut score = 0;

        if self.board.position[m.to].is_some() {
            score += 10 * self.exchange(m);
        }

        if let MoveKind::Promotion(to) = m.kind {
            score += evaluation::hce::piece_value(to, false);
        }

        if self.board.bitboards.attacked_by[hostile][PieceKind::Pawn as usize].get(m.to) {
            score -= 350;
        }

        if let Some(entry) = self.transposition.get(&self.board.position) {
            if entry.best_move == Some(m) && self.board.is_legal(m) {
                // Always search hash moves first
                score += 100_000;
            }
        }

        -score
    }

    #[inline]
    fn evaluate(&self, distance_from_root: usize) -> Score {
        let evaluation = self.board.evaluate();
        if evaluation.is_mate() {
            Score::mated_in(evaluation.relative_to, distance_from_root)
        } else {
            evaluation
        }
    }

    #[inline]
    fn fetch_transposition(&self, alpha: Score, beta: Score, depth: usize) -> Option<Score> {
        let entry = self
            .transposition
            .get(&self.board.position)
            .filter(|e| e.at_depth >= depth)?;

        match entry.node_type {
            NodeType::Pv => Some(entry.score),
            NodeType::Cut if entry.score >= beta => Some(entry.score),
            NodeType::All if entry.score <= alpha => Some(entry.score),
            _ => None,
        }
    }

    fn save_transposition(
        &mut self,
        at_depth: usize,
        score: Score,
        node_type: NodeType,
        best_move: Option<Move>,
    ) {
        if let Some(entry) = self.transposition.get(&self.board.position) {
            // Prefer higher depth transpositions
            if entry.at_depth > at_depth {
                return;
            }

            // Prefer entries with a best move
            if entry.at_depth == at_depth && entry.best_move.is_some() && best_move.is_none() {
                return;
            }
        }

        self.transposition.insert(
            self.board.position,
            TranspositionEntry::new(at_depth, score, node_type, best_move),
        );
    }

    fn pv_search(
        &mut self,
        mut alpha: Score,
        mut beta: Score,
        depth: usize,
        distance_from_root: usize,
    ) -> Score {
        self.data.nodes += 1;

        let side = self.board.side_to_move();

        if let Some(score) = self.fetch_transposition(alpha, beta, depth) {
            self.data.transposition_hits += 1;
            return score;
        }

        if depth == 0 {
            return self.quiesce(alpha, beta, distance_from_root);
        }

        // Mate distance pruning
        alpha = alpha.max(Score::mated_in(side, distance_from_root));
        beta = beta.min(Score::mate_in(side, distance_from_root + 1));
        if alpha >= beta {
            return alpha;
        }

        let next_depth = depth - 1;
        let next_distance = distance_from_root + 1;

        let mut moves = self.board.moves().clone();
        moves.sort_by_key(|m| self.score_move(*m));

        let mut best_move = None;
        for m in moves {
            self.board.make_move(m).expect("Legal move");

            let score = if best_move.is_none() {
                // All left-most nodes are Pv nodes.
                -self.pv_search(-beta, -alpha, next_depth, next_distance)
            } else {
                // All sibling-nodes to Pv nodes are cut nodes.
                let mut score = -self.zw_search(NodeType::Cut, -alpha, next_depth, next_distance);
                if score > alpha && score < beta {
                    score = -self.pv_search(-beta, -alpha, next_depth, next_distance);
                }
                score
            };

            self.board.undo();

            if score >= beta {
                // We're cutting the node, so save it as a cut node in the transposition table.
                self.save_transposition(depth, beta, NodeType::Cut, Some(m));

                self.data.prunes += 1;
                return beta;
            }

            if score > alpha {
                alpha = score;
                best_move = Some(m);
            }
        }

        // No cut occured, so this is a Pv node.
        self.save_transposition(depth, alpha, NodeType::Pv, best_move);
        alpha
    }

    fn zw_search(
        &mut self,
        node_type: NodeType,
        mut beta: Score,
        depth: usize,
        distance_from_root: usize,
    ) -> Score {
        self.data.nodes += 1;

        let side = self.board.side_to_move();
        let mut alpha = beta - 1;

        if let Some(score) = self.fetch_transposition(alpha, beta, depth) {
            self.data.transposition_hits += 1;
            return score;
        }

        if depth == 0 {
            return self.quiesce(alpha, beta, distance_from_root);
        }

        // Mate distance pruning
        alpha = alpha.max(Score::mated_in(side, distance_from_root));
        beta = beta.min(Score::mate_in(side, distance_from_root + 1));
        if alpha >= beta {
            return alpha;
        }

        // Razoring
        if depth <= self.razoring_depth && !self.board.in_check() {
            let current_eval = self.board.evaluate();

            if current_eval + self.razoring_margin < beta {
                let quiesce_eval = self.quiesce(alpha, beta, distance_from_root);

                if quiesce_eval < beta {
                    self.data.prunes += 1;
                    return quiesce_eval;
                }
            }
        }

        // All children of cut nodes are all nodes, and vice-versa.
        #[allow(clippy::match_wildcard_for_single_variants)]
        let next_node_type = match node_type {
            NodeType::Cut => NodeType::All,
            NodeType::All => NodeType::Cut,
            _ => unreachable!("zw_search should only be used for cut and all nodes"),
        };
        let next_distance = distance_from_root + 1;
        let next_beta = -beta + 1;
        let next_depth = depth - 1;

        let mut moves = self.board.moves().clone();
        moves.sort_by_key(|m| self.score_move(*m));

        // Multi-cut pruning.
        if node_type == NodeType::Cut
            && next_depth >= self.mc_reduction
            && moves.len() > self.mc_moves
            && !self.board.in_check()
        {
            let mc_depth = next_depth - self.mc_reduction;

            let mut count = 0;
            for m in moves.iter().take(self.mc_moves) {
                self.board.make_move(*m).expect("Legal move");
                let score = -self.zw_search(next_node_type, next_beta, mc_depth, next_distance);
                self.board.undo();

                if score >= beta {
                    count += 1;
                    if count >= self.mc_limit {
                        self.data.prunes += 1;
                        return beta;
                    }
                }
            }
        }

        // Search children.
        for m in moves {
            self.board.make_move(m).expect("Legal move");
            let score = -self.zw_search(next_node_type, next_beta, next_depth, next_distance);
            self.board.undo();

            if score >= beta {
                // We're cutting the node, so save it as a cut node in the transposition table.
                self.save_transposition(depth, beta, NodeType::Cut, Some(m));

                self.data.prunes += 1;
                return beta;
            }
        }

        // No cut occured, so save this as an all node.
        self.save_transposition(depth, beta, NodeType::All, None);
        alpha
    }

    fn quiesce(&mut self, mut alpha: Score, beta: Score, distance_from_root: usize) -> Score {
        self.data.nodes += 1;

        let evaluation = self.evaluate(distance_from_root);
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
            .sorted_by_key(|m| -self.exchange(*m))
            .collect();

        if moves.is_empty() {
            return evaluation;
        }

        for m in moves {
            self.board.make_move(m).expect("Legal move");
            let score = -self.quiesce(-beta, -alpha, distance_from_root + 1);
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
    fn new(at_depth: usize, score: Score, node_type: NodeType, best_move: Option<Move>) -> Self {
        Self {
            at_depth,
            score,
            node_type,
            best_move,
        }
    }
}

impl SearchData {
    fn new() -> Self {
        Self {
            best_move: None,
            score: Score::default(),
            depth: 0,
            start_time: Instant::now(),
            duration: Duration::default(),
            nodes: 0,
            transposition_hits: 0,
            prunes: 0,
        }
    }
}
