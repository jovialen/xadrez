use criterion::{black_box, criterion_group, criterion_main, Criterion};
use xadrez::board::Chessboard;

fn perft_benchmark(c: &mut Criterion) {
    let mut board = Chessboard::default();

    c.bench_function("perft 1", |b| b.iter(|| board.perft(black_box(1))));
    c.bench_function("perft 2", |b| b.iter(|| board.perft(black_box(2))));
    c.bench_function("perft 3", |b| b.iter(|| board.perft(black_box(3))));
    c.bench_function("perft 4", |b| b.iter(|| board.perft(black_box(4))));

    let mut group = c.benchmark_group("perft-high-depth");
    group.sample_size(10);
    group.bench_function("perft 5", |b| b.iter(|| board.perft(black_box(5))));
    group.bench_function("perft 6", |b| b.iter(|| board.perft(black_box(6))));
    group.finish();
}

criterion_group!(benches, perft_benchmark);
criterion_main!(benches);
