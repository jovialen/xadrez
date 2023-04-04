//! Move searching.

#![allow(clippy::module_name_repetitions)]

use crate::error::SearchError;
use crate::evaluation::evaluate_position;
use crate::evaluation::hce::piece_value;
use crate::evaluation::score::Score;
use crate::piece::PieceKind;
use crate::position::Position;
use crate::r#move::{Move, MoveKind};
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::time::{Duration, Instant};

/// Configures and limits the chess move search.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SearchLimits {
    /// The maximum amout of time the search can last.
    pub max_time: Option<Duration>,
    /// The maximum search depth.
    pub max_depth: Option<usize>,

    /// How many moves to evaluate in a multi-cut pruning.
    pub mc_moves: usize,
    /// How many cutoffs must occur to prune a branch.
    pub mc_limit: usize,
    /// How much to reduce the search depth in a multi-cut search.
    pub mc_reduction: usize,

    /// The depth after which razoring is enabled.
    pub razoring_depth: usize,
    /// The evaluation margin used for razoring cutoffs.
    pub razoring_margin: i32,
}

/// Search data from a chess move search.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SearchData {
    start_time: Instant,
    /// How long the search lasted.
    pub duration: Duration,
    /// Deepest search depth reached during the search.
    pub depth: usize,
    /// The score of the best move found in the search.
    pub score: Score,

    /// How many regular nodes where evaluated in the search.
    pub nodes: usize,
    /// How many quiesce nodes where evaluated in the search.
    pub qnodes: usize,
    /// How many nodes where the transposition table was used instead of
    /// re-searching the node.
    pub transposition_hits: usize,
    /// How many nodes where pruned.
    pub prunes: usize,
    /// Total amount of depth reduced.
    pub reductions: usize,
    /// Total amound of depth extended.
    pub extensions: usize,
}

struct SearchGraph {
    limits: SearchLimits,
    data: SearchData,
    ttable: TranspositionTable,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NodeType {
    Pv,
    Cut,
    All,
}

#[derive(Default)]
struct TranspositionTable(FxHashMap<Position, TranspositionEntry>);

struct TranspositionEntry {
    score: Score,
    depth: usize,
    node_type: NodeType,
    best_move: Option<Move>,
}

pub(crate) fn search(
    position: &Position,
    limits: SearchLimits,
) -> Result<(Move, SearchData), SearchError> {
    let side = position.data.side_to_move;
    let mut graph = SearchGraph {
        limits,
        data: SearchData::default(),
        ttable: TranspositionTable::default(),
    };

    let mut moves = position.generate_moves();

    if moves.is_empty() {
        return Err(SearchError::NoMoves);
    }

    if moves.len() == 1 {
        // If there is only one possible move, no amount of searching is going
        // to find a better one.
        return Ok((moves[0], graph.data));
    }

    let mut best_move = moves[0];
    'search: for depth in 0..graph.limits.max_depth.unwrap_or(usize::MAX) {
        let max_depth = depth * 2;

        moves.sort_by_key(|m| score_move(position, &graph, *m));

        let mut best_iteration_move = moves[0];
        let mut best_iteration_score = Score::min(side);
        for m in moves.iter() {
            let next_position = position.make_move(*m);

            let score = if let Ok(score) = pv_search(
                &next_position,
                &mut graph,
                Score::min(!side),
                Score::max(!side),
                depth,
                max_depth,
                1,
            ) {
                -score
            } else {
                break 'search;
            };

            if score > best_iteration_score {
                best_iteration_score = score;
                best_iteration_move = *m;
            }
        }

        best_move = best_iteration_move;

        graph.data.depth = depth;
        graph.data.score = best_iteration_score;
        graph.ttable.insert(
            *position,
            depth,
            best_iteration_score,
            NodeType::Pv,
            Some(best_iteration_move),
        );

        if best_iteration_score.is_mate() {
            break 'search;
        }
    }

    graph.data.duration = graph.data.start_time.elapsed();
    Ok((best_move, graph.data))
}

fn pv_search(
    position: &Position,
    graph: &mut SearchGraph,
    mut alpha: Score,
    mut beta: Score,
    depth: usize,
    max_depth: usize,
    distance_from_root: usize,
) -> Result<Score, SearchError> {
    graph.data.nodes += 1;

    if depth == 0 {
        return quiesce_search(position, graph, alpha, beta, distance_from_root);
    }

    if is_out_of_time(graph) {
        return Err(SearchError::OutOfTime);
    }

    let side = position.data.side_to_move;
    let next_distance = distance_from_root + 1;
    let next_max_depth = max_depth.saturating_sub(1);

    // Mate distance pruning
    alpha = alpha.max(Score::mated_in(side, distance_from_root));
    beta = beta.min(Score::mate_in(side, distance_from_root + 1));
    if alpha >= beta {
        graph.data.prunes += 1;
        return Ok(alpha);
    }

    let mut moves = position.generate_moves();
    moves.sort_by_key(|m| score_move(position, graph, *m));

    let extensions = extensions(position, &moves);
    let reductions = reductions(position, graph, NodeType::Pv);

    graph.data.reductions += reductions;
    graph.data.extensions += extensions;

    let next_depth = (depth + extensions)
        .saturating_sub(reductions + 1)
        .min(max_depth);

    let mut best_move = None;
    for m in moves {
        let next_position = position.make_move(m);

        let score = if best_move.is_none() {
            // All left-most nodes are Pv nodes.
            -pv_search(
                &next_position,
                graph,
                -beta,
                -alpha,
                next_depth,
                next_max_depth,
                next_distance,
            )?
        } else {
            // All sibling-nodes to Pv nodes are cut nodes.
            let mut score = -zw_search(
                &next_position,
                graph,
                NodeType::Cut,
                -alpha,
                next_depth,
                next_max_depth,
                next_distance,
            )?;
            if score > alpha && score < beta {
                score = -pv_search(
                    &next_position,
                    graph,
                    -beta,
                    -alpha,
                    next_depth,
                    next_max_depth,
                    next_distance,
                )?;
            }
            score
        };

        if score >= beta {
            // We're cutting the node, so save it as a cut node in the transposition table.
            graph
                .ttable
                .insert(*position, depth, beta, NodeType::Cut, Some(m));

            graph.data.prunes += 1;
            return Ok(beta);
        }

        if score > alpha {
            alpha = score;
            best_move = Some(m);
        }
    }

    // No cut occured, so this is a Pv node.
    graph
        .ttable
        .insert(*position, depth, alpha, NodeType::Pv, best_move);
    Ok(alpha)
}

fn zw_search(
    position: &Position,
    graph: &mut SearchGraph,
    node_type: NodeType,
    mut beta: Score,
    depth: usize,
    max_depth: usize,
    distance_from_root: usize,
) -> Result<Score, SearchError> {
    graph.data.nodes += 1;

    let side = position.data.side_to_move;
    let mut alpha = beta - 1;

    if let Some(score) = graph.ttable.get(position, alpha, beta, depth) {
        graph.data.transposition_hits += 1;
        return Ok(score);
    }

    if depth == 0 {
        return quiesce_search(position, graph, alpha, beta, distance_from_root);
    }

    if is_out_of_time(graph) {
        return Err(SearchError::OutOfTime);
    }

    // Mate distance pruning
    alpha = alpha.max(Score::mated_in(side, distance_from_root));
    beta = beta.min(Score::mate_in(side, distance_from_root + 1));
    if alpha >= beta {
        graph.data.prunes += 1;
        return Ok(alpha);
    }

    let current_eval = evaluate(position, distance_from_root);

    // Razoring
    if depth <= graph.limits.razoring_depth
        && current_eval + graph.limits.razoring_margin < beta
        && !position.in_check()
    {
        let quiesce_eval = quiesce_search(position, graph, alpha, beta, distance_from_root)?;

        if quiesce_eval < beta {
            graph.data.prunes += 1;
            return Ok(quiesce_eval);
        }
    }

    // All children of cut nodes are all nodes, and vice-versa.
    let next_node_type = match node_type {
        NodeType::Cut => NodeType::All,
        NodeType::All => NodeType::Cut,
        NodeType::Pv => unreachable!("zw_search should only be used for cut and all nodes"),
    };
    let next_max_depth = max_depth.saturating_sub(1);
    let next_distance = distance_from_root + 1;
    let next_beta = -beta + 1;

    let mut moves = position.generate_moves();
    moves.sort_by_key(|m| score_move(position, graph, *m));

    let extensions = extensions(position, &moves);
    let mut reductions = reductions(position, graph, node_type);

    // Null-move reduction
    if current_eval >= beta && !position.in_check() {
        let next_depth = depth.saturating_sub(3 + usize::from(depth > 6));

        let next_position = position.make_null_move();
        let null_eval = -zw_search(
            &next_position,
            graph,
            next_node_type,
            next_beta,
            next_depth,
            next_max_depth,
            next_distance,
        )?;

        if null_eval >= beta && !null_eval.is_mate() {
            reductions += 6;

            if depth <= 6 {
                graph.data.prunes += 1;
                return Ok(current_eval);
            }
        }
    }

    // Multi-cut pruning.
    if node_type == NodeType::Cut && moves.len() > graph.limits.mc_moves && !position.in_check() {
        let next_depth = depth.saturating_sub(graph.limits.mc_reduction + 1);

        let mut count = 0;
        for m in moves.iter().take(graph.limits.mc_moves) {
            let next_position = position.make_move(*m);
            let score = -zw_search(
                &next_position,
                graph,
                next_node_type,
                next_beta,
                next_depth,
                next_max_depth,
                next_distance,
            )?;

            if score >= beta {
                count += 1;
                if count >= graph.limits.mc_limit {
                    graph.data.prunes += 1;
                    return Ok(beta);
                }
            }
        }
    }

    graph.data.reductions += reductions;
    graph.data.extensions += extensions;

    let next_depth = (depth + extensions)
        .saturating_sub(reductions + 1)
        .min(max_depth);

    // Search children.
    for m in moves {
        let next_position = position.make_move(m);
        let score = -zw_search(
            &next_position,
            graph,
            next_node_type,
            next_beta,
            next_depth,
            next_max_depth,
            next_distance,
        )?;

        if score >= beta {
            // We're cutting the node, so save it as a cut node in the transposition table.
            graph
                .ttable
                .insert(*position, depth, beta, NodeType::Cut, Some(m));

            graph.data.prunes += 1;
            return Ok(beta);
        }
    }

    // No cut occured, so save this as an all node.
    graph
        .ttable
        .insert(*position, depth, beta, NodeType::All, None);
    Ok(alpha)
}

fn quiesce_search(
    position: &Position,
    graph: &mut SearchGraph,
    mut alpha: Score,
    beta: Score,
    distance_from_root: usize,
) -> Result<Score, SearchError> {
    graph.data.qnodes += 1;

    let evaluation = evaluate(position, distance_from_root);
    if evaluation >= beta {
        graph.data.prunes += 1;
        return Ok(beta);
    }
    alpha = alpha.max(evaluation);

    if let Some(score) = graph.ttable.get(position, alpha, beta, 0) {
        return Ok(score);
    }

    let moves: Vec<_> = position
        .generate_moves()
        .into_iter()
        .filter(|m| matches!(m.kind, MoveKind::Capture | MoveKind::Promotion(_)))
        .sorted_by_key(|m| score_move(position, graph, *m))
        .collect();

    if moves.is_empty() {
        return Ok(evaluation);
    }

    for m in moves {
        let next_position = position.make_move(m);
        let score = -quiesce_search(&next_position, graph, -beta, -alpha, distance_from_root + 1)?;

        if score >= beta {
            graph.data.prunes += 1;
            return Ok(beta);
        }
        alpha = alpha.max(score);
    }

    Ok(alpha)
}

fn is_out_of_time(graph: &SearchGraph) -> bool {
    if let Some(max) = graph.limits.max_time {
        return graph.data.start_time.elapsed() > max;
    }

    false
}

fn extensions(position: &Position, moves: &Vec<Move>) -> usize {
    let mut extensions = 0;

    // Check extension
    if position.in_check() {
        extensions += 1;
    }

    // One reply extension
    if moves.len() == 1 {
        extensions += 1;
    }

    extensions
}

fn reductions(position: &Position, graph: &SearchGraph, node_type: NodeType) -> usize {
    // No reductions when in check.
    if position.in_check() {
        return 0;
    }

    let mut reductions = 0;

    // Reduce Pv nodes that aren't in the transposition table
    if !graph.ttable.0.contains_key(position) && node_type as usize == NodeType::Pv as usize {
        reductions += 3;
    }

    // Cut node reduction
    if node_type as usize == NodeType::Cut as usize {
        reductions += 2;
    }

    reductions
}

fn score_move(position: &Position, graph: &SearchGraph, m: Move) -> i32 {
    let side = position.data.side_to_move;
    let hostile = !side as usize;

    let mut score = 0;

    if position[m.to].is_some() {
        score += 10 * exchange(position, m);
    }

    if let MoveKind::Promotion(to) = m.kind {
        score += piece_value(to, false);
    }

    if position.bb.attacked_by[hostile][PieceKind::Pawn as usize].get(m.to) {
        score -= 350;
    }

    if let Some(entry) = graph.ttable.0.get(position) {
        if entry.best_move == Some(m) {
            // Always search hashed moves first
            score += 1_000_000;
        }
    }

    -score
}

#[inline]
fn exchange(position: &Position, m: Move) -> i32 {
    let piece = position[m.from].unwrap();

    if let Some(capture) = position[m.to] {
        piece_value(capture.kind, false) - piece_value(piece.kind, false)
    } else {
        0
    }
}

#[inline]
fn evaluate(position: &Position, distance_from_root: usize) -> Score {
    let static_evaluation = evaluate_position(position);
    if static_evaluation.is_mate() {
        Score::mated_in(static_evaluation.relative_to, distance_from_root)
    } else {
        static_evaluation
    }
}
impl SearchLimits {
    /// No search limits.
    #[must_use]
    pub fn none() -> Self {
        Self::default()
    }

    /// Time-limited search.
    ///
    /// The generated [`SearchLimits`] will not have a depth constraint
    ///
    /// # Arguments
    ///
    /// * `max_time` - How long the search can last.
    #[must_use]
    pub fn from_duration(max_time: Duration) -> Self {
        Self {
            max_time: Some(max_time),
            ..Default::default()
        }
    }

    /// Depth-limited search.
    ///
    /// The generated [`SearchLimits`] will not have a time constraint
    ///
    /// # Arguments
    ///
    /// * `max_depth` - How deep the search can get.
    #[must_use]
    pub fn from_depth(max_depth: usize) -> Self {
        Self {
            max_depth: Some(max_depth),
            ..Default::default()
        }
    }
}

impl Default for SearchLimits {
    fn default() -> Self {
        Self {
            max_depth: None,
            max_time: None,

            mc_reduction: 1,
            mc_moves: 6,
            mc_limit: 3,

            razoring_depth: 3,
            razoring_margin: 300,
        }
    }
}

impl Default for SearchData {
    fn default() -> Self {
        Self {
            start_time: Instant::now(),

            score: Score::equal(),
            duration: Duration::default(),
            depth: 0,
            nodes: 0,
            qnodes: 0,
            transposition_hits: 0,
            prunes: 0,
            reductions: 0,
            extensions: 0,
        }
    }
}

impl TranspositionTable {
    fn get(&self, position: &Position, alpha: Score, beta: Score, depth: usize) -> Option<Score> {
        let entry = self.0.get(&position).filter(|e| e.depth >= depth)?;

        match entry.node_type {
            NodeType::Pv => Some(entry.score),
            NodeType::Cut if entry.score >= beta => Some(entry.score),
            NodeType::All if entry.score <= alpha => Some(entry.score),
            _ => None,
        }
    }

    fn insert(
        &mut self,
        position: Position,
        depth: usize,
        score: Score,
        node_type: NodeType,
        best_move: Option<Move>,
    ) {
        if let Some(entry) = self.0.get(&position) {
            // Prefer higher depth transpositions
            if entry.depth > depth {
                return;
            }

            // Prefer entries with a best move
            if entry.depth == depth && entry.best_move.is_some() && best_move.is_none() {
                return;
            }
        }

        self.0.insert(
            position,
            TranspositionEntry {
                depth,
                score,
                node_type,
                best_move,
            },
        );
    }
}
