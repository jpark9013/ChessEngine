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

CI job `strength` plays our engine against **Stockfish 17.1** with `UCI_LimitStrength` and `UCI_Elo=1600`. Both sides get **200 ms** per move. Deploy from `main` requires a score of **≥ 35%** over 40 games (20 as white, 20 as black). Any crash or illegal move fails the job immediately. Pull requests run 8 games with no score floor so rating variance cannot block a PR, but a crash or illegal move still fails.

```bash
uv sync --group strength
PYTHONPATH=build:bot uv run python scripts/gauntlet.py \
  --stockfish /path/to/stockfish \
  --games 40 --elo 1600 --movetime-ms 200 --min-score 0.35 \
  --pgn gauntlet.pgn
```

35% vs Elo 1600 is roughly 100–150 points weaker (logistic). Recalibrate `--elo` / `--min-score` after a few hundred local games if the gate is too tight or too loose. PGN from CI is uploaded as the `gauntlet-pgn` artifact.

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
