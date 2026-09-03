#!/usr/bin/env python3
"""Gauntlet: our engine vs strength-limited Stockfish.

Used as the CI publish floor. Four games are 30+0 and four are 60+0, both
sides on the same clock. Openings are short fixed lines so games are not
identical. Requires python-chess, a Stockfish binary, and the chessengine
module on PYTHONPATH.
"""

from __future__ import annotations

import argparse
import atexit
import math
import multiprocessing
import os
import shutil
import signal
import sys
from collections.abc import Iterator
from concurrent.futures import FIRST_COMPLETED, Future, ProcessPoolExecutor, wait
from dataclasses import dataclass
from pathlib import Path

DEFAULT_GAMES = 8
DEFAULT_ELO = 2200
DEFAULT_MIN_POINTS = 4.0
DEFAULT_CONCURRENCY = 2
DEFAULT_MAX_PLY = 180
CLOCK_SHORT_S = 30.0
CLOCK_LONG_S = 60.0
SF_THREADS = 1
SF_HASH_MB = 16

_WORKER_ENGINE: object | None = None

OPENINGS: list[list[str]] = [
    [],
    ["e2e4"],
    ["e2e4", "e7e5"],
    ["e2e4", "c7c5"],
    ["e2e4", "e7e6"],
    ["e2e4", "c7c6"],
    ["d2d4"],
    ["d2d4", "d7d5"],
    ["d2d4", "g8f6"],
    ["c2c4"],
    ["g1f3"],
    ["e2e4", "e7e5", "g1f3", "b8c6"],
]


@dataclass(frozen=True)
class GameRecord:
    index: int
    result: str
    ply: int
    we_are_white: bool
    clock_s: float
    pgn: str


@dataclass(frozen=True)
class MatchScore:
    wins: int
    draws: int
    losses: int
    crashes: int = 0
    illegal: int = 0

    @property
    def games(self) -> int:
        return self.wins + self.draws + self.losses

    @property
    def points(self) -> float:
        return self.wins + 0.5 * self.draws

    @property
    def fraction(self) -> float:
        return score_fraction(self.wins, self.draws, self.losses)


def score_fraction(wins: int, draws: int, losses: int) -> float:
    n = wins + draws + losses
    if n <= 0:
        return 0.0
    return (wins + 0.5 * draws) / n


def meets_floor(points: float, min_points: float) -> bool:
    """True when points meet the CI floor. Exactly min_points passes."""
    return points >= min_points


def early_stop_decision(points: float, remaining: int, min_points: float) -> str | None:
    """Return 'pass' or 'fail' if leftover games cannot change the gate, else None.

    Remaining games can only add points, so P >= floor is already a pass. If
    P + remaining < floor, even winning every leftover game cannot reach the floor.
    """
    if meets_floor(points, min_points):
        return "pass"
    if points + remaining < min_points:
        return "fail"
    return None


def elo_from_score(score: float) -> float | None:
    """Elo difference (us minus opponent) from a logistic score."""
    if score <= 0.0 or score >= 1.0:
        return None
    return -400.0 * math.log10(1.0 / score - 1.0)


def _find_stockfish(explicit: str | None) -> Path:
    if explicit:
        path = Path(explicit)
        if not path.is_file():
            raise FileNotFoundError(f"stockfish not found: {path}")
        return path
    env = os.environ.get("STOCKFISH")
    if env:
        path = Path(env)
        if path.is_file():
            return path
    which = shutil.which("stockfish")
    if which:
        return Path(which)
    raise FileNotFoundError(
        "Stockfish binary not found. Pass --stockfish, set STOCKFISH, or install it on PATH."
    )


def _configure_stockfish(engine: object, elo: int) -> str:
    opts = engine.options  # type: ignore[attr-defined]
    config: dict[str, object] = {}
    if "Threads" in opts:
        config["Threads"] = SF_THREADS
    if "Hash" in opts:
        config["Hash"] = SF_HASH_MB
    if "UCI_LimitStrength" in opts and "UCI_Elo" in opts:
        config["UCI_LimitStrength"] = True
        lo = int(opts["UCI_Elo"].min) if opts["UCI_Elo"].min is not None else 1320
        hi = int(opts["UCI_Elo"].max) if opts["UCI_Elo"].max is not None else 3190
        config["UCI_Elo"] = min(hi, max(lo, elo))
        engine.configure(config)  # type: ignore[attr-defined]
        return f"UCI_Elo={config['UCI_Elo']} (requested {elo})"
    if "Skill Level" in opts:
        # Rough map onto Stockfish's 0–20 skill slider.
        skill = min(20, max(0, round((elo - 1320) / 90)))
        config["Skill Level"] = skill
        engine.configure(config)  # type: ignore[attr-defined]
        return f"Skill Level={skill} (requested Elo {elo})"
    raise RuntimeError("Stockfish has neither UCI_Elo nor Skill Level")


def _clock_for_game(index: int) -> float:
    return CLOCK_SHORT_S if index % 2 == 0 else CLOCK_LONG_S


def _our_move(fen: str, clock_s: float, ply: int) -> str:
    from engine import allocate_time, search_position

    budget = allocate_time(clock_s, 0.0, None, ply=ply)
    result = search_position(
        fen,
        max_seconds=budget.hard,
        target_seconds=budget.target,
        depth=budget.max_depth,
    )
    return str(result["uci"])


def _play_game(
    *,
    stockfish: object,
    opening: list[str],
    we_are_white: bool,
    clock_s: float,
    max_ply: int,
) -> tuple[str, object]:
    """Play one 30+0 or 60+0 game. `clock_s` is the whole-game budget per side.

    Each ply gets a short think from `allocate_time` (tens of ms). Do not send
    the remaining 30s/60s to Stockfish as wtime/btime — it would then spend
    seconds on a single move.
    """
    import time

    import chess
    import chess.engine

    from engine import allocate_time

    board = chess.Board()
    for uci in opening:
        board.push_uci(uci)

    white_clock = clock_s
    black_clock = clock_s

    while not board.is_game_over(claim_draw=True) and board.ply() < max_ply:
        mover_white = board.turn == chess.WHITE
        ours = mover_white == we_are_white
        side_clock = white_clock if mover_white else black_clock
        budget = allocate_time(side_clock, 0.0, None, ply=board.ply())
        started = time.perf_counter()
        if ours:
            try:
                uci = _our_move(board.fen(), side_clock, board.ply())
                move = chess.Move.from_uci(uci)
            except Exception:
                return "crash", board
            if move not in board.legal_moves:
                return "illegal", board
            board.push(move)
        else:
            played = stockfish.play(  # type: ignore[attr-defined]
                board,
                chess.engine.Limit(time=max(0.01, budget.hard)),
            )
            if played.move is None:
                return "crash", board
            board.push(played.move)

        elapsed = time.perf_counter() - started
        if mover_white:
            white_clock -= elapsed
            flagged = white_clock <= 0.0
        else:
            black_clock -= elapsed
            flagged = black_clock <= 0.0
        if flagged:
            return ("0-1" if ours else "1-0"), board

    outcome = board.outcome(claim_draw=True)
    if outcome is None:
        return "1/2-1/2", board
    if outcome.winner is None:
        return "1/2-1/2", board
    we_won = (outcome.winner == chess.WHITE) == we_are_white
    return ("1-0" if we_won else "0-1"), board


def _board_pgn(board: object, we_are_white: bool) -> str:
    import chess.pgn

    game = chess.pgn.Game.from_board(board)
    game.headers["Event"] = "ChessEngine gauntlet"
    game.headers["White"] = "ChessEngine" if we_are_white else "Stockfish"
    game.headers["Black"] = "Stockfish" if we_are_white else "ChessEngine"
    outcome = board.outcome(claim_draw=True)  # type: ignore[attr-defined]
    game.headers["Result"] = outcome.result() if outcome else "1/2-1/2"
    return str(game)


def _record_from_play(
    index: int,
    stockfish: object,
    max_ply: int,
) -> GameRecord:
    we_are_white = index % 2 == 0
    clock_s = _clock_for_game(index)
    result, board = _play_game(
        stockfish=stockfish,
        opening=OPENINGS[index % len(OPENINGS)],
        we_are_white=we_are_white,
        clock_s=clock_s,
        max_ply=max_ply,
    )
    return GameRecord(
        index=index,
        result=result,
        ply=board.ply(),
        we_are_white=we_are_white,
        clock_s=clock_s,
        pgn=_board_pgn(board, we_are_white),
    )


def _apply_result(
    result: str, wins: int, draws: int, losses: int, crashes: int, illegal: int
) -> tuple[str, int, int, int, int, int]:
    if result == "crash":
        return "crash-loss", wins, draws, losses + 1, crashes + 1, illegal
    if result == "illegal":
        return "illegal-loss", wins, draws, losses + 1, crashes, illegal + 1
    if result == "1-0":
        return "win", wins + 1, draws, losses, crashes, illegal
    if result == "0-1":
        return "loss", wins, draws, losses + 1, crashes, illegal
    return "draw", wins, draws + 1, losses, crashes, illegal


def _print_game(
    record: GameRecord, games: int, tag: str, wins: int, draws: int, losses: int
) -> None:
    color = "white" if record.we_are_white else "black"
    print(
        f"  game {record.index + 1}/{games}  us={color}  {tag}  "
        f"clock={record.clock_s:.0f}s  ply={record.ply}  W-D-L {wins}-{draws}-{losses}",
        flush=True,
    )


def _write_pgn(path: Path, records: list[GameRecord | None]) -> None:
    with path.open("w", encoding="utf-8") as fh:
        for record in records:
            if record is None:
                continue
            print(record.pgn, file=fh, end="\n\n")


def _kill_engine(engine: object) -> None:
    """Drop Stockfish immediately. UCI quit can block the process forever."""
    transport = getattr(engine, "transport", None)
    pid = None
    if transport is not None:
        get_pid = getattr(transport, "get_pid", None)
        if callable(get_pid):
            try:
                pid = int(get_pid())
            except Exception:
                pid = None
        for meth in ("kill", "abort", "close"):
            fn = getattr(transport, meth, None)
            if callable(fn):
                try:
                    fn()
                except Exception:
                    pass
                break
    closer = getattr(engine, "close", None)
    if callable(closer):
        try:
            closer()
        except Exception:
            pass
    if pid:
        try:
            os.kill(pid, signal.SIGKILL)
        except Exception:
            pass


def _stop_pool(pool: ProcessPoolExecutor, pending: set[Future[GameRecord]]) -> None:
    for fut in pending:
        fut.cancel()
    for proc in list(getattr(pool, "_processes", {}).values()):
        try:
            if proc.is_alive():
                proc.kill()
        except Exception:
            pass
    pool.shutdown(wait=False, cancel_futures=True)


def _probe_strength(sf_path: Path, elo: int) -> str:
    import chess.engine

    engine = chess.engine.SimpleEngine.popen_uci(str(sf_path), timeout=2.0)
    try:
        return _configure_stockfish(engine, elo)
    finally:
        _kill_engine(engine)


def _init_worker(sf_path: str, elo: int) -> None:
    global _WORKER_ENGINE
    import chess.engine

    _WORKER_ENGINE = chess.engine.SimpleEngine.popen_uci(sf_path, timeout=2.0)
    _configure_stockfish(_WORKER_ENGINE, elo)
    atexit.register(_shutdown_worker)


def _shutdown_worker() -> None:
    global _WORKER_ENGINE
    engine = _WORKER_ENGINE
    _WORKER_ENGINE = None
    if engine is not None:
        _kill_engine(engine)


def _worker_play(index: int, max_ply: int) -> GameRecord:
    assert _WORKER_ENGINE is not None
    return _record_from_play(index, _WORKER_ENGINE, max_ply)


def run_gauntlet(args: argparse.Namespace) -> MatchScore:
    sf_path = _find_stockfish(args.stockfish)
    pgn_path = Path(args.pgn) if args.pgn else None
    concurrency = min(args.concurrency, args.games)

    strength = _probe_strength(sf_path, args.elo)
    print(f"Stockfish: {sf_path}")
    print(f"Strength: {strength}")
    n_short = (args.games + 1) // 2
    n_long = args.games // 2
    print(
        f"Games: {args.games}  concurrency: {concurrency}  "
        f"game clocks: {CLOCK_SHORT_S:.0f}s+0 x{n_short}, "
        f"{CLOCK_LONG_S:.0f}s+0 x{n_long}  "
        f"min points: {args.min_points:g}"
    )

    if concurrency <= 1:
        stream = _iter_serial(args.games, str(sf_path), args.elo, args.max_ply)
    else:
        stream = _iter_parallel(
            args.games, str(sf_path), args.elo, args.max_ply, concurrency
        )

    wins = draws = losses = crashes = illegal = 0
    ordered: list[GameRecord | None] = [None] * args.games
    try:
        for record in stream:
            ordered[record.index] = record
            tag, wins, draws, losses, crashes, illegal = _apply_result(
                record.result, wins, draws, losses, crashes, illegal
            )
            _print_game(record, args.games, tag, wins, draws, losses)
            if record.result in {"crash", "illegal"}:
                print(f"aborting gauntlet after {record.result}", flush=True)
                break
            played = wins + draws + losses
            remaining = args.games - played
            points = wins + 0.5 * draws
            decision = early_stop_decision(points, remaining, args.min_points)
            if remaining <= 0 or decision is None:
                continue
            if decision == "pass":
                print(
                    f"early PASS: {points:.1f} points already meet floor "
                    f"{args.min_points:g}; skipping {remaining} remaining game(s)",
                    flush=True,
                )
            else:
                print(
                    f"early FAIL: {points:.1f} points + {remaining} remaining "
                    f"< floor {args.min_points:g}",
                    flush=True,
                )
            break
    finally:
        close = getattr(stream, "close", None)
        if close is not None:
            close()

    if pgn_path is not None:
        _write_pgn(pgn_path, ordered)

    return MatchScore(wins=wins, draws=draws, losses=losses, crashes=crashes, illegal=illegal)


def _iter_serial(
    games: int, sf_path: str, elo: int, max_ply: int
) -> Iterator[GameRecord]:
    import chess.engine

    engine = chess.engine.SimpleEngine.popen_uci(sf_path, timeout=2.0)
    try:
        _configure_stockfish(engine, elo)
        for i in range(games):
            record = _record_from_play(i, engine, max_ply)
            yield record
            if record.result in {"crash", "illegal"}:
                return
    finally:
        _kill_engine(engine)


def _iter_parallel(
    games: int,
    sf_path: str,
    elo: int,
    max_ply: int,
    concurrency: int,
) -> Iterator[GameRecord]:
    ctx = multiprocessing.get_context("spawn")
    pool = ProcessPoolExecutor(
        max_workers=concurrency,
        mp_context=ctx,
        initializer=_init_worker,
        initargs=(sf_path, elo),
    )
    pending: set[Future[GameRecord]] = set()
    next_index = 0

    def fill() -> None:
        nonlocal next_index
        while next_index < games and len(pending) < concurrency:
            pending.add(pool.submit(_worker_play, next_index, max_ply))
            next_index += 1

    try:
        fill()
        while pending:
            done, _ = wait(pending, return_when=FIRST_COMPLETED)
            fut = next(iter(done))
            pending.remove(fut)
            record = fut.result()
            yield record
            if record.result in {"crash", "illegal"}:
                return
            fill()
    finally:
        _stop_pool(pool, pending)


def _write_summary(score: MatchScore, args: argparse.Namespace) -> None:
    diff = elo_from_score(score.fraction)
    diff_s = f"{diff:+.0f} vs opponent" if diff is not None else "n/a (0% or 100%)"
    lines = [
        f"W-D-L {score.wins}-{score.draws}-{score.losses}  "
        f"score {score.points:.1f}/{score.games} ({score.fraction:.1%})",
        f"estimated Elo difference: {diff_s}",
        f"crashes: {score.crashes}  illegal: {score.illegal}",
        f"opponent Elo setting: {args.elo}  "
        f"game clocks: {CLOCK_SHORT_S:.0f}s+0 / {CLOCK_LONG_S:.0f}s+0",
    ]
    text = "\n".join(lines)
    print(text)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        body = "\n".join(
            [
                "## Stockfish gauntlet",
                "",
                f"- Result: **{score.wins}-{score.draws}-{score.losses}** "
                f"({score.fraction:.1%})",
                f"- Floor: ≥ {args.min_points:g} points vs UCI_Elo {args.elo}",
                f"- Clocks: {CLOCK_SHORT_S:.0f}s x4, {CLOCK_LONG_S:.0f}s x4",
                f"- Elo diff (logistic): {diff_s}",
                f"- Crashes: {score.crashes}, illegal moves: {score.illegal}",
                "",
            ]
        )
        with open(summary, "a", encoding="utf-8") as fh:
            fh.write(body)


def _parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--games", type=int, default=DEFAULT_GAMES)
    p.add_argument("--elo", type=int, default=DEFAULT_ELO, help="Stockfish UCI_Elo")
    p.add_argument(
        "--min-points",
        type=float,
        default=DEFAULT_MIN_POINTS,
        help="Minimum points (win=1, draw=0.5, loss=0)",
    )
    p.add_argument(
        "--concurrency",
        type=int,
        default=DEFAULT_CONCURRENCY,
        help="Games to play in parallel (default 2)",
    )
    p.add_argument("--max-ply", type=int, default=DEFAULT_MAX_PLY)
    p.add_argument("--stockfish", default=None, help="Path to Stockfish binary")
    p.add_argument("--pgn", default=None, help="Write games to this PGN path")
    p.add_argument(
        "--max-crashes",
        type=int,
        default=0,
        help="Fail if crashes exceed this (default 0)",
    )
    p.add_argument(
        "--max-illegal",
        type=int,
        default=0,
        help="Fail if illegal moves exceed this (default 0: any illegal move fails)",
    )
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    if args.games < 1:
        print("games must be >= 1", file=sys.stderr)
        return 2
    if args.concurrency < 1:
        print("concurrency must be >= 1", file=sys.stderr)
        return 2
    try:
        score = run_gauntlet(args)
    except FileNotFoundError as exc:
        print(exc, file=sys.stderr)
        return 2
    except Exception as exc:
        print(f"gauntlet failed: {exc}", file=sys.stderr)
        return 2

    _write_summary(score, args)

    if score.crashes > args.max_crashes:
        print(f"FAIL: {score.crashes} crashes (max {args.max_crashes})", file=sys.stderr)
        return 1
    if score.illegal > args.max_illegal:
        print(
            f"FAIL: {score.illegal} illegal move(s) (max {args.max_illegal})",
            file=sys.stderr,
        )
        return 1
    if not meets_floor(score.points, args.min_points):
        print(
            f"FAIL: score {score.points:.1f}/{score.games} < floor {args.min_points:g} "
            f"(need >= {args.min_points:g})",
            file=sys.stderr,
        )
        return 1
    print(f"PASS: {score.points:.1f} >= floor {args.min_points:g}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
