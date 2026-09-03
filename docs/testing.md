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

`tests/test_python.py` covers the pybind module. `tests/test_adapter.py` covers the FEN/UCI bot contract (no live Lichess).

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
