use xadrez::prelude::*;

fn main() {
	let chessboard = BoardBuilder::new()
		.start_position()
		.position(FEN_STARTING_POSITION)? // Result<BoardBuilder, ParseFenError>
		.piece(Square::A7, None)
		.piece(Square::A5, Some(Piece::new(PieceKind::Pawn, Color::Black)))
		.en_passant(Square::A6)
		.castling(Color::White, Castling::Kingside, false)
		.halftime(0)
		.fulltime(1)
		.build()                          // Result<Chessboard, BoardError>
		.expect();

	//               Chessboard::default();
	//               Chessboard::from_str("fen");
	//               Chessboard::empty();

	chessboard.pieces();     // Vec<(Square, Piece)>
	chessboard.squares();    // [(Square, Option<Piece>); 64]
	chessboard.moves();      // Vec<Move>
	chessboard.game_state(); // GameState
	chessboard[Square::E1] == Some(Piece::new(PieceKind::King, Color::White));
	for (square, piece) in chessboard {}

	// Result<Move, MoveError>
	chessboard.make_move(Move::from_str("e2-e4"));
	chessboard.make_move(Move::from_str("e7e5"));

	// Result<SearchData, SearchError>
	chessboard.search(SearchLimits::from_duration(Duration::from_secs(5)));
	chessboard.search(SearchLimits::from_depth(10));

	chessboard.ponder();    // ()
	chessboard.ponderhit();

	// Option<Move>
	chessboard.best_move();
}