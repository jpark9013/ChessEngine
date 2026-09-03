"""FEN search contract used by the lichess-bot homemade adapter.

Two-layer bullet clocks. The allocator sets an *optimum* (`target`) and a
*maximum* (`hard`). Iterative deepening in C++ stops at the optimum when the
PV is stable, and spends toward the maximum only when the best move or eval
keeps swinging.
"""

from __future__ import annotations

from typing import NamedTuple

import chessengine as ce

START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

# Optimum / maximum for a healthy 30–60s clock. Unstable positions may use hard.
_MAX_TARGET = 0.035
_MAX_HARD = 0.10


class TimeBudget(NamedTuple):
    target: float
    hard: float
    max_depth: int


def _expected_moves(ply: int) -> int:
    """Moves we still budget for. Stays conservative so we never dump the clock."""
    made = max(0, ply // 2)
    return max(10, 32 - made // 2)


def _depth_for_budget(hard: float) -> int:
    if hard < 0.04:
        return 4
    if hard < 0.08:
        return 8
    if hard < 0.12:
        return 12
    return 16


def _depth_for_clock(clock: float) -> int:
    if clock < 0.4:
        return 1
    if clock < 1.0:
        return 3
    if clock < 3.0:
        return 6
    if clock < 10.0:
        return 12
    return 16


def allocate_time(
    our: float | None,
    inc: float | None,
    sudden: float | None,
    ply: int = 0,
) -> TimeBudget:
    """Optimum and maximum think time from the UCI clock.

    `target` is how long a stable position should take. `hard` is the extra
    budget search may spend if the PV is unstable. Increment raises both;
    a 0-increment bullet clock stays stingy.
    """
    if sudden is not None and sudden > 0:
        hard = max(0.03, min(_MAX_HARD, sudden * 0.85))
        target = max(0.012, min(_MAX_TARGET, hard * 0.45))
        return TimeBudget(target, hard, _depth_for_budget(hard))

    clock = float(our if our is not None else 5.0)
    increment = float(inc if inc is not None else 0.0)
    ply = max(0, int(ply))

    if clock <= 0.20:
        hard = min(0.04, max(0.015, clock * 0.35))
        return TimeBudget(min(0.012, hard * 0.6), hard, 2)
    if clock <= 0.60:
        hard = min(0.06, clock * 0.12)
        target = min(0.025, hard * 0.50)
        return TimeBudget(target, max(target, hard), 4)

    reserve = 0.20
    usable = max(0.05, clock - reserve)
    base = usable / _expected_moves(ply) + increment * 0.45
    hard = min(_MAX_HARD, max(0.04, base * 1.8), usable * 0.06)
    target = min(_MAX_TARGET, max(0.012, hard * 0.40, base * 0.25))
    if clock < 8.0:
        scale = max(0.35, clock / 8.0)
        target *= scale
        hard *= scale
    hard = min(hard, max(0.04, clock - 0.08))
    target = min(target, hard * 0.75)
    return TimeBudget(target, hard, _depth_for_budget(hard))


def choose_depth(our: float | None, requested: int | None) -> int:
    clock = our if our is not None else 60.0
    cap = _depth_for_clock(clock)
    if requested is not None and requested > 0:
        return max(1, min(int(requested), cap))
    return cap


def search_position(
    fen: str,
    *,
    max_seconds: float = 0.0,
    target_seconds: float = 0.0,
    depth: int = 4,
    root_moves: list[str] | None = None,
) -> dict:
    board = ce.Board.from_fen(fen)
    legal = [move.uci() for move in board.legal_moves()]
    if not legal:
        return {
            "uci": "0000",
            "score_cp": 0,
            "depth": 0,
            "nodes": 0,
            "seconds": 0.0,
            "draw_offered": False,
            "resigned": False,
        }

    def fallback() -> str:
        if root_moves:
            allowed = [uci for uci in root_moves if uci in legal]
            if allowed:
                return allowed[0]
        return legal[0]

    try:
        result = board.search(
            depth=depth,
            mode=ce.SearchMode.ALPHABETA_QUIESCENCE,
            max_seconds=max_seconds,
            target_seconds=target_seconds,
        )
        uci = result.best_move.uci()
        if uci not in legal:
            uci = fallback()
        elif root_moves:
            allowed = [move for move in root_moves if move in legal]
            if allowed and uci not in allowed:
                uci = allowed[0]
        return {
            "uci": uci,
            "score_cp": int(result.score),
            "depth": int(result.depth),
            "nodes": int(result.nodes),
            "seconds": float(result.seconds),
            "draw_offered": abs(int(result.score)) <= 20,
            "resigned": False,
        }
    except Exception:
        return {
            "uci": fallback(),
            "score_cp": 0,
            "depth": 0,
            "nodes": 0,
            "seconds": 0.0,
            "draw_offered": False,
            "resigned": False,
        }
