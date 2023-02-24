use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};
use xadrez::board::Chessboard;
use xadrez::search::MoveSearcher;

fn search_move_benchmark(c: &mut Criterion) {
    let board = Chessboard::default();

    let mut low_depth = c.benchmark_group("move-search-low-depth");
    low_depth.measurement_time(Duration::from_secs(10));
    low_depth.bench_function("search depth 1", |b| b.iter(|| search_depth(&board, 1)));
    low_depth.bench_function("search depth 2", |b| b.iter(|| search_depth(&board, 2)));
    low_depth.bench_function("search depth 3", |b| b.iter(|| search_depth(&board, 3)));
    low_depth.finish();

    let mut high_depth = c.benchmark_group("move-search-high-depth");
    high_depth.sample_size(10);
    high_depth.bench_function("search depth 4", |b| b.iter(|| search_depth(&board, 4)));
    high_depth.bench_function("search depth 5", |b| b.iter(|| search_depth(&board, 5)));
    high_depth.finish();

    let mut checkmate = c.benchmark_group("move-search-find-checkmate");
    checkmate.sample_size(30);
    let board = Chessboard::from_fen("3r4/4r3/k7/8/8/6K1/8/8 b - - 0 1").expect("Valid FEN");
    checkmate.bench_function("search Kvrrk", |b| b.iter(|| search(&board)));
    checkmate.finish();
}

#[allow(unused_must_use)]
fn search(board: &Chessboard) {
    MoveSearcher::new(board).search();
}

#[allow(unused_must_use)]
fn search_depth(board: &Chessboard, depth: usize) {
    MoveSearcher::new(board).max_depth(depth).search();
}

criterion_group!(benches, search_move_benchmark);
criterion_main!(benches);
