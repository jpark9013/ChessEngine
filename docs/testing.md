# Testing

C++ uses [GoogleTest](https://google.github.io/googletest/). Python uses the stdlib `unittest` runner, [ruff](https://docs.astral.sh/ruff/) for lint, and [uv](https://docs.astral.sh/uv/) to install and run those tools.

```bash
./scripts/run_tests.sh
```

That configures CMake, builds, runs `ctest` (GoogleTest), `clang-tidy` if installed, `ruff check`, and `python -m unittest`.

## C++

```bash
cmake -S . -B build -DCHESS_BUILD_TESTS=ON -DCHESS_BUILD_PYTHON=OFF
cmake --build build -j
ctest --test-dir build --output-on-failure
```

Cases live in `tests/test_board.cpp` and `tests/test_perft.cpp`:

```cpp
TEST(Board, NameInCamelCase) {
  EXPECT_EQ(actual, expected);
  EXPECT_TRUE(condition);
}
```

Lint is clang-tidy (`.clang-tidy`) over `src/*.cpp`.

## Python

```bash
uv sync --group dev
uv run ruff check bot tests
PYTHONPATH=build:bot uv run python -m unittest discover -s tests -p 'test_*.py' -v
```

`tests/test_python.py` covers the pybind module. `tests/test_adapter.py` covers the FEN/UCI bot contract (no live Lichess). `tests/test_gauntlet.py` covers gauntlet score math (no Stockfish).

## Strength floor (Stockfish gauntlet)

CI job `strength` plays our engine against **Stockfish 17.1** with `UCI_LimitStrength` and `UCI_Elo=2200`. Four games are **30+0** and four are **60+0** (those numbers are the **whole-game** clock per side, not think time per move). Each ply uses the same two-layer allocator as the live bot (optimum ~35ms, maximum ~100ms if the PV is unstable), including Stockfish. Two games run at a time. Deploy requires **≥ 4 points** over 8 games (win = 1, draw = 0.5, loss = 0). Any crash or illegal move fails the job immediately.

```bash
uv sync --group strength
PYTHONPATH=build:bot uv run python scripts/gauntlet.py \
  --stockfish /path/to/stockfish \
  --games 8 --elo 2200 --min-points 4 --concurrency 2 \
  --pgn gauntlet.pgn
```

4/8 vs Elo 2200 is a 50% score (even). Recalibrate `--elo` / `--min-points` after a few hundred local games if the gate is too tight or too loose. PGN from CI is uploaded as the `gauntlet-pgn` artifact.

## Perft

Published counts from [Chess Programming Wiki: Perft Results](https://www.chessprogramming.org/Perft_Results):

| Position | Depths checked |
|---|---|
| Start | 1–5 (4,865,609 at 5) |
| Kiwipete | 1–4 |
| Position 3 | 1–5 |
| Position 4 (promotions) | 1–4 |
| Position 5 | 1–4 |
| Position 6 | 1–4 |
