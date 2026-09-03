#!/usr/bin/env bash
# Build the engine, run GoogleTest, ruff, and Python unittest.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

BUILD_DIR="${CHESS_BUILD_DIR:-$ROOT/build}"
JOBS="${CHESS_JOBS:-}"
if [[ -z "$JOBS" ]]; then
  if command -v sysctl >/dev/null 2>&1; then
    JOBS="$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"
  else
    JOBS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"
  fi
fi

if command -v uv >/dev/null 2>&1; then
  PYTHON="${PYTHON:-$(uv python find 2>/dev/null || command -v python3)}"
else
  PYTHON="${PYTHON:-$(command -v python3)}"
fi

echo "==> configure ($BUILD_DIR)"
cmake -S "$ROOT" -B "$BUILD_DIR" \
  -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE:-Release}" \
  -DCHESS_BUILD_TESTS=ON \
  -DCHESS_BUILD_PYTHON=ON \
  -DPython_EXECUTABLE="$PYTHON"

echo "==> build"
cmake --build "$BUILD_DIR" -j "$JOBS"

echo "==> C++ GoogleTest"
ctest --test-dir "$BUILD_DIR" --output-on-failure

if command -v clang-tidy >/dev/null 2>&1; then
  echo "==> clang-tidy"
  clang-tidy -p "$BUILD_DIR" \
    src/attacks.cpp src/board.cpp src/perft.cpp src/search.cpp src/zobrist.cpp src/main.cpp
fi

if command -v uv >/dev/null 2>&1; then
  echo "==> uv sync"
  uv sync --group dev
  echo "==> ruff"
  uv run ruff check bot tests
  echo "==> python unittest"
  PYTHONPATH="$BUILD_DIR:$ROOT/bot${PYTHONPATH:+:$PYTHONPATH}" uv run python -m unittest discover -s tests -p 'test_*.py' -v
else
  echo "==> python unittest (uv not found; using $PYTHON)"
  PYTHONPATH="$BUILD_DIR:$ROOT/bot${PYTHONPATH:+:$PYTHONPATH}" "$PYTHON" -m unittest discover -s tests -p 'test_*.py' -v
fi

echo "==> all tests passed"
