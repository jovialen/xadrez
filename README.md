# Xadrez

A chess engine written in Rust.

## Usage

**Xadrez** uses Rusts built-in package manager [Cargo](https://doc.rust-lang.org/cargo/).

To use **Xadrez** in your projects, first include it as a dependency in your `Cargo.toml`.

```toml
[dependencies]
xadrez = { path = "path/to/xadrez" }
```

```rust
use xadrez::prelude::*;

fn main() {
	let chessboard = Chessboard::default();

	let moves: Vec<Move> = chessboard.moves();
	let pieces: Vec<(Piece, Square)> = chessboard.pieces();

	chessboard.make_move(moves[1])

	let fen = chessboard.to_string();

	chessboard.set_position("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

	chessboard.make_move(Move::from_str("e2e4"));
}
```

Use the `cargo doc` command to generate the full documentation.

## Tests

To run the full test suite, run [Cargos](https://doc.rust-lang.org/cargo/) test command.

```console
foo@bar:~$ cargo test
```

It is also recommended to use the `--release` flag as some of the tests take a long time to run.

## Benchmarks

**Xadrez** also has benchmarks which you can use to test the performance of the engine on your system.

```console
foo@bar:~$ cargo bench
```
