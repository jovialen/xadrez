use criterion::{criterion_group, criterion_main, Criterion};
use xadrez::board::Chessboard;

fn eval_benchmark(c: &mut Criterion) {
    let board = Chessboard::default();

    let mut eval = c.benchmark_group("board-evaluation");
    eval.bench_function("evaluate", |b| b.iter(|| board.evaluate()));
    eval.finish();
}

criterion_group!(benches, eval_benchmark);
criterion_main!(benches);
