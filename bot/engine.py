"""FEN search contract used by the lichess-bot homemade adapter.

Two-layer clocks, Stockfish-style. The allocator sets an *optimum* (`target`)
and a *maximum* (`hard`). Iterative deepening in C++ stops at the optimum when
the PV is stable, and spends toward the maximum only when the best move or eval
keeps swinging.

Live Lichess, the Python CLI, and the Stockfish gauntlet / Elo CI job use
the full remaining game clock (``mode="live"``). ``mode="gauntlet"`` is the
old 35ms/100ms handicap, kept for local experiments.
"""

from __future__ import annotations

import os
from typing import NamedTuple

import chessengine as ce

START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

# Matches bot/config.yml move_overhead (network + process lag on Lichess).
_MOVE_OVERHEAD = 0.35
_FLAG_BUFFER = 0.08
_PANIC_CLOCK = 0.40
_INC_FRAC = 0.65
_MAX_USABLE_FRAC = 0.75
_MIN_HORIZON = 20
_MAX_HORIZON = 40

# Strength-gate / Elo-estimate match fairness. Not used on Lichess.
GAUNTLET_TARGET = 0.035
GAUNTLET_HARD = 0.10
GAUNTLET_DEPTH = 12


class TimeBudget(NamedTuple):
    target: float
    hard: float
    max_depth: int


def _horizon(ply: int) -> int:
    """Remaining moves we still budget for. Shrinks with ply, 40 → 20."""
    return max(_MIN_HORIZON, _MAX_HORIZON - max(0, ply) // 4)


def _max_scale(ply: int) -> float:
    """SF-ish 5.5–8× optimum for an unstable PV."""
    return min(8.0, 5.5 + max(0, ply) / 16.0)


def _depth_for_budget(hard: float) -> int:
    """ID ceiling. Time, not a 100ms-era depth=12 clamp, is the real limit."""
    if hard < 0.04:
        return 4
    if hard < 0.08:
        return 8
    if hard < 0.15:
        return 16
    if hard < 0.40:
        return 32
    return 64


def _depth_for_clock(clock: float) -> int:
    if clock < 0.4:
        return 1
    if clock < 1.0:
        return 3
    if clock < 3.0:
        return 12
    if clock < 10.0:
        return 32
    return 64


def _panic_budget(clock: float) -> TimeBudget:
    """Tiny remaining time: a fixed short think, never dump the clock."""
    hard = min(0.05, max(0.012, clock * 0.25))
    hard = min(hard, max(0.008, clock - 0.05))
    target = min(hard * 0.60, hard)
    return TimeBudget(max(0.004, target), max(target, hard), 2 if clock <= 0.20 else 4)


def _gauntlet_budget(
    our: float | None,
    inc: float | None,
    sudden: float | None,
    ply: int,
) -> TimeBudget:
    """Fixed 35/100ms handicap for local experiments. CI uses mode=live."""
    del inc, ply
    if sudden is not None and sudden > 0:
        hard = min(GAUNTLET_HARD, max(0.03, sudden * 0.85))
        target = min(GAUNTLET_TARGET, hard * 0.45)
        return TimeBudget(target, hard, GAUNTLET_DEPTH)
    clock = float(our if our is not None else 5.0)
    if clock <= _PANIC_CLOCK:
        return _panic_budget(clock)
    return TimeBudget(GAUNTLET_TARGET, GAUNTLET_HARD, GAUNTLET_DEPTH)


def _clamp_to_clock(target: float, hard: float, clock: float, usable: float) -> TimeBudget:
    hard = min(hard, usable * _MAX_USABLE_FRAC, max(0.012, clock - _FLAG_BUFFER))
    if clock > _MOVE_OVERHEAD:
        hard = min(hard, usable)
    target = min(target, hard)
    target = max(0.008, target)
    hard = max(hard, target)
    return TimeBudget(target, hard, _depth_for_budget(hard))


def allocate_time(
    our: float | None,
    inc: float | None,
    sudden: float | None,
    ply: int = 0,
    *,
    mode: str | None = None,
) -> TimeBudget:
    """Optimum and maximum think time from the UCI clock.

    `target` is how long a stable position should take. `hard` is the extra
    budget search may spend if the PV is unstable. Increment raises both.

    ``mode="live"`` is the Stockfish-style allocator used on Lichess and in
    the CI gauntlet. ``mode="gauntlet"`` is the old 35ms/100ms handicap. If
    ``mode`` is omitted, ``GAUNTLET=1`` selects that handicap; otherwise live.
    """
    if mode is None:
        mode = "gauntlet" if os.environ.get("GAUNTLET") == "1" else "live"
    if mode == "gauntlet":
        return _gauntlet_budget(our, inc, sudden, ply)

    ply = max(0, int(ply))

    if sudden is not None and sudden > 0:
        hard = max(0.03, sudden * 0.85)
        target = max(0.012, hard * 0.50)
        if sudden <= _PANIC_CLOCK:
            return _panic_budget(sudden)
        return _clamp_to_clock(target, hard, sudden, max(0.05, sudden - _FLAG_BUFFER))

    clock = float(our if our is not None else 5.0)
    increment = float(inc if inc is not None else 0.0)

    if clock <= _PANIC_CLOCK:
        return _panic_budget(clock)

    usable = max(0.05, clock - _MOVE_OVERHEAD)
    horizon = _horizon(ply)
    optimum = usable / horizon
    if increment > 0:
        optimum += increment * _INC_FRAC
    maximum = optimum * _max_scale(ply)
    return _clamp_to_clock(optimum, maximum, clock, usable)


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
