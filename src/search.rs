use crate::board::Chessboard;
use crate::r#move::{Move, MoveKind};

pub struct MoveSearcher {
    board: Chessboard,
    depth: Option<usize>,
}

impl MoveSearcher {
    #[must_use]
    pub fn new(board: &Chessboard) -> Self {
        Self {
            board: board.clone(),
            depth: None,
        }
    }

    #[must_use]
    pub fn max_depth(mut self, depth: usize) -> Self {
        self.depth = Some(depth);
        self
    }

    #[must_use]
    pub fn search(mut self) -> Option<Move> {
        let depth = self.depth.unwrap_or(usize::MAX).max(1);

        self.board
            .moves()
            .clone()
            .into_iter()
            .max_by_key(move |&m| {
                self.board.make_move(m).expect("All moves should be legal");
                let eval = -alpha_beta(&mut self.board, depth - 1, -i32::MAX, i32::MAX);
                self.board.undo();
                eval
            })
    }
}

fn alpha_beta(board: &mut Chessboard, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 {
        return quiesce(board, -beta, -alpha);
    }

    let moves = board.moves().clone();
    for m in moves {
        board.make_move(m).expect("All moves should be legal");
        let score = -alpha_beta(board, depth - 1, -beta, -alpha);
        board.undo();

        if score >= beta {
            return beta;
        }
        alpha = alpha.max(score);
    }

    alpha
}

fn quiesce(board: &mut Chessboard, mut alpha: i32, beta: i32) -> i32 {
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
        let score = -quiesce(board, -beta, -alpha);
        board.undo();

        if score >= beta {
            return beta;
        }
        alpha = alpha.max(score);
    }

    alpha
}
