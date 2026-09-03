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

`tests/test_python.py` covers the pybind module. `tests/test_adapter.py` covers the FEN/UCI bot contract (no live Lichess). `tests/test_matchmaking.py` covers outgoing 1+0 targeting and reject cooldowns (no live Lichess). `tests/test_gauntlet.py` covers gauntlet score math, time-control parsing, and Elo binary-search logic (no Stockfish).

## Strength floor (Stockfish gauntlet)

CI job `strength` plays our engine against **Stockfish 17.1** with `UCI_LimitStrength` and `UCI_Elo=2200`. Four games are **30+0** and four are **60+0** (those numbers are the **whole-game** clock per side, not think time per move). Each ply uses the same two-layer allocator as the live bot (optimum ~35ms, maximum ~100ms if the PV is unstable), including Stockfish. Two games run at a time. Deploy requires **≥ 4 points** over 8 games (win = 1, draw = 0.5, loss = 0); exactly 4.0 passes. The match stops early if the floor is already met or can no longer be reached, cancelling games that have not started. Any crash or illegal move fails the job immediately.

```bash
uv sync --group strength
PYTHONPATH=build:bot uv run python scripts/gauntlet.py \
  --stockfish /path/to/stockfish \
  --games 8 --elo 2200 --min-points 4 --clocks 30+0,60+0 --concurrency 2 \
  --pgn gauntlet.pgn
```

4/8 vs Elo 2200 is a 50% score (even). Recalibrate `--elo` / `--min-points` after a few hundred local games if the gate is too tight or too loose. PGN from CI is uploaded as the `gauntlet-pgn` artifact.

`--clocks` is a comma-separated list cycled by game index. Bare integers are **seconds** (so `30` is 30s sudden death, `3+2` is 3s+2s increment, not 3 minutes). Minutes need a suffix: `5m+0`, `5min+0`, `1m30s+2`. Increment is Fischer (added after a completed move) and flows into `allocate_time` as remaining clock + increment — not the whole base dumped on one ply.

## Elo estimate (after deploy)

CI job `estimate-elo` runs only after **`strength` and `deploy` both succeed** (`needs: [strength, deploy]`), so it is main-push-only. It binary-searches Stockfish `UCI_Elo` with the same 8-game protocol as the strength gate (default `--clocks 30+0,60+0`, `allocate_time` ~35ms/100ms, not 30s/move). Each probe is a match vs `UCI_Elo=mid`; ≥ 4.0 points means we are at least `mid` (`lo = mid`), else `hi = mid`. It stops when `hi - lo <= 100` and reports `(lo+hi)//2`, i.e. **±50**. Eight games are noisy; the number is a CI snapshot, not a 400-game rating.

Probes are **sequential** (each needs the previous `lo`/`hi`). Only the games *inside* one probe run in parallel.

`--concurrency` is **1–4**. The strength gate stays at **2**. Calibration defaults to **4**. A GitHub `ubuntu-latest` runner has about 4 vCPU; each game is this engine (single-thread search, ~16 MB TT) plus 1-thread Stockfish (16 MB hash). Four pairs is ~8 threads on 4 cores — roughly 2× oversubscription, around 50–100 Elo of NPS loss at 35–100ms/move, which is the budget. More than 4 would exceed that, so the CLI rejects it.

```bash
PYTHONPATH=build:bot uv run python scripts/gauntlet.py \
  --calibrate --stockfish /path/to/stockfish \
  --games 8 --min-points 4 --clocks 30+0,60+0 --concurrency 4 \
  --tolerance 50 --update-readme README.md
```

`--dry-run` prints the plan without Stockfish. The job writes `$GITHUB_STEP_SUMMARY` and replaces the `<!-- elo-estimate -->` block in `README.md`. If the number changed, it commits as `github-actions[bot]` with `[skip ci]` and pushes to the same branch. Jobs also skip `github-actions[bot]` so a README-only push cannot loop tests/strength/deploy/calibrate.

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
