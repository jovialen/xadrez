use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::time::Duration;
use xadrez::board::Chessboard;

fn perft_benchmark(c: &mut Criterion) {
    let board = Chessboard::default();

    let mut low_depth = c.benchmark_group("perft-low-depth");
    low_depth.bench_function("perft 1", |b| b.iter(|| board.perft(black_box(1), false)));
    low_depth.bench_function("perft 2", |b| b.iter(|| board.perft(black_box(2), false)));
    low_depth.finish();

    let mut mid_depth = c.benchmark_group("perft-mid-depth");
    mid_depth.measurement_time(Duration::from_secs(10));
    mid_depth.bench_function("perft 3", |b| b.iter(|| board.perft(black_box(3), false)));
    mid_depth.bench_function("perft 4", |b| b.iter(|| board.perft(black_box(4), false)));
    mid_depth.finish();

    let mut high_depth = c.benchmark_group("perft-high-depth");
    high_depth.sample_size(10);
    high_depth.bench_function("perft 5", |b| b.iter(|| board.perft(black_box(5), false)));
    high_depth.bench_function("perft 6", |b| b.iter(|| board.perft(black_box(6), false)));
    high_depth.finish();
}

criterion_group!(benches, perft_benchmark);
criterion_main!(benches);
