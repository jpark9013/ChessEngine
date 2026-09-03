# ChessEngine

A from-scratch C++ chess library: legal move generation, FEN, SAN/UCI, evaluation, and alpha-beta search. There is a command-line player and a Python module via pybind11.

<!-- elo-estimate:start -->
Estimated strength: **2458 Elo** (±50, vs Stockfish `UCI_Elo`, 8 games at 60+0)
<!-- elo-estimate:end -->

This is a rewrite of the original single-header engine. The old design packed flags into coordinates, ignored pawn checks, leaked make/unmake on search cutoffs, and did not round-trip FEN. This tree splits those concerns into a small static library and locks the rules with perft.

## Layout

```
src/               paired .hpp/.cpp (library, CLI, Python bindings)
tests/             C++ unit tests (including standard perft suites)
bot/               lichess-bot homemade adapter and config
docs/              architecture and API notes
scripts/           test runner
```

## Build

Needs CMake 3.16+, a C++20 compiler, and (for the Python module) Python 3 development headers plus network access the first time CMake fetches pybind11.

If you have Conda on `PATH` and CMake picks a Python without headers, point it at a full install:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
  -DPython_EXECUTABLE="$(command -v python3)"
```

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

Turn pieces off if you only want the library:

```bash
cmake -S . -B build -DCHESS_BUILD_PYTHON=OFF -DCHESS_BUILD_TESTS=OFF
```

Outputs:

| Target | Path |
|---|---|
| Library | `build/libchess_lib.a` |
| CLI | `build/chess` |
| Tests | `build/chess_tests` |
| Python module | `build/chessengine*.so` (or `.dylib`) |

## Tests

```bash
./scripts/run_tests.sh
```

That script configures and builds if needed, runs GoogleTest via `ctest`, ruff, and `python -m unittest`. See [docs/testing.md](docs/testing.md).

You can still invoke pieces yourself. Run the C++ tests in Release — several perft cases walk millions of nodes.

```bash
./build/chess_tests
PYTHONPATH=build:bot uv run python -m unittest discover -s tests -p 'test_*.py' -v
```

## CLI

```bash
./build/chess
```

You pick depth, search mode, and AI vs AI or AI vs human. Moves can be SAN (`Nf3`), UCI (`g1f3`), or a list index.

Search modes:

1. Minimax (correct but slow; only useful at tiny depth)
2. Alpha-beta
3. Alpha-beta plus capture quiescence (default in the Python API)

## Python

```python
import chessengine as ce

board = ce.Board()
board.push_san("e4")
board.push_uci("e7e5")
print(board.fen())
print([m.uci() for m in board.legal_moves()])

result = board.search(depth=4)
print(result.best_move, result.score)
```

See [docs/python-api.md](docs/python-api.md).

## C++

```cpp
#include "chess.hpp"

chess::Board board;
board.make(board.parse_san("e4"));
auto result = chess::search(board, {.depth = 4});
```

See [docs/cpp-api.md](docs/cpp-api.md) and [docs/architecture.md](docs/architecture.md).

## Lichess bot

[docs/bot.md](docs/bot.md) — homemade adapter in `bot/`, always-on Fly worker (`fly.toml`, 2 GB). The live bot only plays bullet with base ≤ 120s (1+0, 2+1, …). Outgoing matchmaking is **rated 1+0** against other bots, preferring equal/stronger once our rating is established; declined challenges are cooled down for 7 days (`bot/challenge_cooldown.json`, gitignored). Put `LICHESS_TOKEN` in `.env` (never commit it) and start with `uv run python scripts/run_lichess_bot.py`.

CI runs tests on every branch, a Stockfish Elo-2200 gauntlet (8× 60+0 / 1+0, Stockfish on the remaining game clock), and deploys from `main` when `FLY_API_TOKEN` is set and the gauntlet scores at least 4 points in 8 games. After that, `estimate-elo` binary-searches Stockfish `UCI_Elo` (same 8-game 60+0 protocol) and writes the ±50 estimate into the markers above.

## What this engine is not

Hybrid bitboards, no opening book, no NNUE. Strength is a classical engine (tapered HCE, PVS, LMR, SEE, check extensions, IIR) aimed at the Stockfish 11-class band. Live Lichess and the CI gauntlet both use a Stockfish-style remaining-clock protocol on 1+0.
