# Python API

The module name is `chessengine` so it does not clash with the `chess` package on PyPI.

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
export PYTHONPATH="$PWD/build"
python3 -c "import chessengine; print(chessengine.Board())"
```

On macOS the extension is typically `chessengine.cpython-3xx-darwin.so` in `build/`.

## Quick start

```python
import chessengine as ce

board = ce.Board()
print(board)
print(board.fen())

board.push_san("e4")
board.push_uci("e7e5")
board.push_san("Nf3")

for move in board.legal_moves():
    print(move.uci(), board.to_san(move))

result = board.search(depth=4)
print(result.best_move.uci(), result.score, result.nodes)

board.pop()  # unmake
```

Load any position with `Board.from_fen(...)`.

## `Board`

| Method | Meaning |
|---|---|
| `Board()` | Starting position |
| `Board.from_fen(fen)` | Parse FEN |
| `fen()` | Emit FEN |
| `piece_at(square)` | `Piece` on that square |
| `side_to_move()` | `Color.WHITE` or `Color.BLACK` |
| `castling_rights()` | Bitmask `1=WK, 2=WQ, 4=BK, 8=BQ` |
| `ep_square()` | Target square (invalid if none) |
| `halfmove_clock()` / `fullmove_number()` | Clocks |
| `king_square(color)` | King location |
| `hash()` | Zobrist key as an integer |
| `push(move)` / `pop()` | Make / unmake |
| `push_uci(str)` / `push_san(str)` | Parse and make; returns the `Move` |
| `parse_uci` / `parse_san` / `to_san` | Convert without moving |
| `legal_moves()` / `legal_captures()` | Lists of `Move` |
| `in_check()` / `is_attacked(sq, color)` | Attacks |
| `is_legal(move)` / `gives_check(move)` | Predicates |
| `status()` | `GameStatus` with `result`, `draw`, `checkmate` |
| `evaluate()` / `evaluate_white()` | Centipawns |
| `perft(depth)` | Node count |
| `search(depth=4, mode=..., max_seconds=0)` | `SearchResult` |
| `copy()` | Snapshot |

Illegal SAN/UCI raises a C++ `std::invalid_argument`, which pybind11 turns into a Python `RuntimeError`.

## `Move` and `Square`

- `move.uci()`, `move.from_sq`, `move.to_sq`, `move.promotion`, `move.flag`
- `Square.from_algebraic("e4")`, `square.algebraic()`, `square.rank()`, `square.file()`, `square.index()`

`from` is a Python keyword, so the origin square is `from_sq`.

## Search

```python
result = board.search(
    depth=5,
    mode=ce.SearchMode.ALPHABETA_QUIESCENCE,
    max_seconds=1.0,
)
print(result.best_move, result.score, result.nodes, result.seconds)
```

`SearchMode.MINIMAX`, `ALPHABETA`, and `ALPHABETA_QUIESCENCE` match the C++ enum.

Score is from the side to move. The board is not modified.

## Enums

`Color`, `PieceType`, `Piece`, `MoveFlag`, `Result`, `DrawReason`, `SearchMode` are all exported.

Helpers: `ce.opposite(color)`, `ce.make_piece(color, piece_type)`, `ce.color_of(piece)`, `ce.type_of(piece)`, `ce.perft(board, depth)`, `ce.search(board, depth=4)`.

## Example: play until the game ends

```python
import chessengine as ce

board = ce.Board()
while board.status().result == ce.Result.ONGOING:
    result = board.search(depth=3)
    print(board.to_san(result.best_move))
    board.push(result.best_move)
print(board.status().result)
```
