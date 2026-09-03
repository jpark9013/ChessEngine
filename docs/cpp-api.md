# C++ API

Umbrella header:

```cpp
#include "chess.hpp"
```

Everything lives in namespace `chess`. Link against `chess_lib`.

## Types

### `Square`

- `Square(index)` with `0 <= index < 64`
- `Square(rank, file)` each `0..7`
- `Square::from_algebraic("e4")`
- `.index()`, `.rank()`, `.file()`, `.algebraic()`, `.valid()`

### `Piece` / `Color` / `PieceType`

Helpers: `color_of`, `type_of`, `make_piece`, `opposite`, `piece_to_fen`, `piece_from_fen`.

Castling rights are bits on the board:

- `kCastleWK = 1`, `kCastleWQ = 2`, `kCastleBK = 4`, `kCastleBQ = 8`

### `Move`

```cpp
struct Move {
  Square from, to;
  PieceType promotion = PieceType::None;
  MoveFlag flag = MoveFlag::Normal;
  std::string uci() const;
};
```

`Move::make(from, to, flag, promotion)` is the usual constructor helper.

### `MoveList`

Stack array of up to 256 moves. Range-for works. `.to_vector()` if you need a `std::vector`.

## `Board`

```cpp
Board board;                              // start position
Board b = Board::from_fen(fen_string);
std::string fen = b.fen();

b.piece_at(Square::from_algebraic("e2"));
b.side_to_move();
b.castling_rights();
b.ep_square();                            // invalid square if none
b.halfmove_clock();
b.fullmove_number();
b.king_square(Color::White);
b.hash();

b.make(move);
b.unmake();

b.in_check();
b.is_attacked(sq, Color::Black);
b.legal_moves();
b.legal_captures();
b.is_legal(move);
b.gives_check(move);

GameStatus st = b.status();               // result, draw reason, checkmate flag
int stm_eval = b.evaluate();              // centipawns, side to move
int white_eval = b.evaluate_white();

Move m = b.parse_uci("e2e4");
Move n = b.parse_san("Nf3");
std::string san = b.to_san(m);
std::cout << b;                           // ASCII board
b.to_string(true);                        // unicode pieces
```

`parse_uci` / `parse_san` throw `std::invalid_argument` if the move is not legal in the current position. SAN is matched against generated legal moves, so disambiguation and `O-O` / `O-O-O` work without a second parser.

FEN must include at least placement, side, castling, and en passant. Halfmove and fullmove default to `0` and `1` if omitted. Black knights are `n`.

## Search

```cpp
SearchLimits limits;
limits.depth = 5;                               // max ply (iterative deepening 1..depth)
limits.mode = SearchMode::AlphaBetaQuiescence;  // or Minimax, AlphaBeta
limits.max_seconds = 0;                         // hard abort; 0 = no cap
limits.target_seconds = 0;                      // soft stop; 0 = 70% of max if max is set

SearchResult r = search(board, limits);
// r.best_move, r.score, r.depth, r.nodes, r.seconds
```

`r.depth` is the last **completed** iteration. Score is from the side to move at the root, in centipawns. Mate scores are near `±100000`. The board is left unchanged.

## Perft

```cpp
std::uint64_t n = perft(board, 5);
perft_divide(board, 2, &std::cout);   // UCI move → child nodes
```

The board is restored. These numbers must match the published suites in `tests/test_perft.cpp`.
