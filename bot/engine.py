"""FEN search contract used by the lichess-bot homemade adapter.

Time management is for 1+0 and faster (base <= 60s). Iterative deepening
in C++ spends up to `target` seconds and aborts at `hard`.
"""

from __future__ import annotations

from typing import NamedTuple

import chessengine as ce

START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

# Never dump this much of a 60s clock on one move; ID uses the rest of the budget
# across later plies instead of a single fixed depth.
_MAX_TARGET = 2.5
_MAX_HARD = 4.0


class TimeBudget(NamedTuple):
    target: float
    hard: float
    max_depth: int


def _depth_for_clock(clock: float) -> int:
    if clock < 0.4:
        return 1
    if clock < 1.0:
        return 3
    if clock < 3.0:
        return 6
    if clock < 10.0:
        return 12
    return 24


def allocate_time(
    our: float | None,
    inc: float | None,
    sudden: float | None,
) -> TimeBudget:
    """Soft/hard think time for ultrabullet and 1+0-class games."""
    if sudden is not None and sudden > 0:
        hard = max(0.04, min(_MAX_HARD, sudden * 0.90 - 0.05))
        target = max(0.03, min(hard * 0.75, sudden * 0.70))
        return TimeBudget(target, hard, _depth_for_clock(sudden))

    clock = our if our is not None else 5.0
    increment = inc if inc is not None else 0.0

    if clock <= 0.25:
        return TimeBudget(0.02, min(0.06, max(0.03, clock * 0.5)), _depth_for_clock(clock))
    if clock <= 0.8:
        hard = min(0.12, max(0.04, clock * 0.18))
        return TimeBudget(max(0.02, hard * 0.6), hard, _depth_for_clock(clock))
    if clock <= 2.0:
        hard = min(0.22, clock * 0.12)
        return TimeBudget(max(0.04, hard * 0.65), hard, _depth_for_clock(clock))

    if increment <= 0:
        target = clock / 30.0
        hard = min(clock * 0.10, target * 2.8, clock - 0.20)
    else:
        target = clock / 28.0 + increment * 0.70
        hard = min(clock * 0.12, target * 2.5, clock - 0.15)

    target = max(0.04, min(_MAX_TARGET, target))
    hard = max(target + 0.03, min(_MAX_HARD, hard))
    hard = min(hard, max(0.05, clock - 0.12))
    target = min(target, hard * 0.8)
    return TimeBudget(target, hard, _depth_for_clock(clock))


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
