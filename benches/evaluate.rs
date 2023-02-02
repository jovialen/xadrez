use criterion::{criterion_group, criterion_main, Criterion};
use xadrez::board::Chessboard;

fn nnue_eval_benchmark(c: &mut Criterion) {
    let board = Chessboard::default();

    let mut eval = c.benchmark_group("board-evaluation");
    eval.bench_function("nnue evaluation", |b| b.iter(|| board.evaluate()));
}

criterion_group!(benches, nnue_eval_benchmark);
criterion_main!(benches);
