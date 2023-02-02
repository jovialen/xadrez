use criterion::{criterion_group, criterion_main, Criterion};
use xadrez::board::Chessboard;
use xadrez::search::MoveSearcher;

fn search_move_benchmark(c: &mut Criterion) {
    let board = Chessboard::default();

    let mut s = c.benchmark_group("move-search");
    s.bench_function("search depth 1", |b| b.iter(|| search(&board, 1)));
    s.bench_function("search depth 2", |b| b.iter(|| search(&board, 2)));
    s.bench_function("search depth 3", |b| b.iter(|| search(&board, 3)));
    s.bench_function("search depth 4", |b| b.iter(|| search(&board, 4)));
    s.bench_function("search depth 5", |b| b.iter(|| search(&board, 5)));
    s.finish();
}

#[allow(unused_must_use)]
fn search(board: &Chessboard, depth: usize) {
    MoveSearcher::new(board).max_depth(depth).search();
}

criterion_group!(benches, search_move_benchmark);
criterion_main!(benches);
