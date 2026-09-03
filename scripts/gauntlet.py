#!/usr/bin/env python3
"""Gauntlet: our engine vs strength-limited Stockfish.

Used as the CI publish floor and (with --calibrate) to binary-search
Stockfish UCI_Elo until an 8-game match is about even (±50). Default
clocks are 30+0 and 60+0; --clocks accepts any base+increment list.
Games inside one probe run in parallel (max 4); search iterations are
sequential.
Requires python-chess, a Stockfish binary, and the chessengine module on
PYTHONPATH.
"""

from __future__ import annotations

import argparse
import atexit
import math
import multiprocessing
import os
import re
import shutil
import signal
import sys
from collections.abc import Callable, Iterator
from concurrent.futures import FIRST_COMPLETED, Future, ProcessPoolExecutor, wait
from dataclasses import dataclass
from pathlib import Path

DEFAULT_GAMES = 8
DEFAULT_ELO = 2200
DEFAULT_MIN_POINTS = 4.0
DEFAULT_CONCURRENCY = 2
DEFAULT_CALIBRATE_CONCURRENCY = 4
# 4 vCPU GHA runner: 4 games ≈ 8 threads (engine + 1-thread SF). More than
# this oversubscribes past the ~100 Elo contention budget at 35–100ms/move.
MAX_CONCURRENCY = 4
DEFAULT_MAX_PLY = 180
DEFAULT_TOLERANCE = 50
DEFAULT_CLOCKS_SPEC = "30+0,60+0"
SF_THREADS = 1
SF_HASH_MB = 16
# Stockfish 16/17 UCI_Elo range when the binary cannot be probed.
SF_UCI_ELO_MIN = 1320
SF_UCI_ELO_MAX = 3190
ELO_MARK_START = "<!-- elo-estimate:start -->"
ELO_MARK_END = "<!-- elo-estimate:end -->"

_WORKER_ENGINE: object | None = None
_WORKER_CLOCKS: list[TimeControl] | None = None

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
class TimeControl:
    """Whole-game clock per side. Bare integers in CLI are seconds."""

    base_s: float
    increment_s: float = 0.0
    spec: str = ""

    def display(self) -> str:
        """Canonical `base+inc` in seconds (`30+0`, `60+1`)."""
        return f"{_fmt_seconds(self.base_s)}+{_fmt_seconds(self.increment_s)}"


def _fmt_seconds(value: float) -> str:
    if value == int(value):
        return str(int(value))
    return f"{value:g}"


def parse_duration_seconds(token: str) -> float:
    """Parse one duration. Bare numbers are seconds; `5m` / `1m30s` / `2:00` allowed."""
    raw = token.strip()
    if not raw:
        raise ValueError("empty duration in time control")
    if raw.startswith("-"):
        raise ValueError(f"invalid duration {raw!r}: must be non-negative")
    text = (
        raw.lower()
        .replace("mins", "m")
        .replace("min", "m")
        .replace("secs", "s")
        .replace("sec", "s")
    )
    colon = re.fullmatch(r"(\d+):([0-5]?\d(?:\.\d+)?)", text)
    if colon:
        return int(colon.group(1)) * 60 + float(colon.group(2))
    if re.fullmatch(r"\d+(?:\.\d+)?", text):
        return float(text)
    match = re.fullmatch(
        r"(?:(\d+(?:\.\d+)?)h)?(?:(\d+(?:\.\d+)?)m)?(?:(\d+(?:\.\d+)?)s)?",
        text,
    )
    if match and any(match.groups()):
        hours = float(match.group(1) or 0)
        minutes = float(match.group(2) or 0)
        seconds = float(match.group(3) or 0)
        return hours * 3600 + minutes * 60 + seconds
    raise ValueError(
        f"invalid duration {raw!r}: use seconds (30, 60, 300), "
        f"base+increment (30+0, 60+1), or 5m+0 / 1m30s+2"
    )


def parse_time_control(text: str) -> TimeControl:
    """Parse one clock: `30`, `30+0`, `60+1`, `5m+0`, `1m30s+2`, `3+2` (seconds)."""
    raw = text.strip()
    if not raw:
        raise ValueError("empty time control")
    if raw.count("+") > 1:
        raise ValueError(
            f"invalid time control {raw!r}: expected base or base+increment, not {raw!r}"
        )
    if "+" in raw:
        base_tok, inc_tok = raw.split("+", 1)
        if not base_tok.strip() or not inc_tok.strip():
            raise ValueError(
                f"invalid time control {raw!r}: expected base+increment "
                f"(e.g. 30+0, 60+1, 5m+0)"
            )
        base_s = parse_duration_seconds(base_tok)
        increment_s = parse_duration_seconds(inc_tok)
    else:
        base_s = parse_duration_seconds(raw)
        increment_s = 0.0
    if base_s <= 0:
        raise ValueError(f"invalid time control {raw!r}: base clock must be > 0 seconds")
    if increment_s < 0:
        raise ValueError(f"invalid time control {raw!r}: increment must be >= 0")
    return TimeControl(base_s=base_s, increment_s=increment_s, spec=raw)


def parse_clocks(text: str) -> list[TimeControl]:
    """Parse comma-separated clocks, cycled by game index (`30+0,60+0`)."""
    parts = [part.strip() for part in text.split(",")]
    if not parts or any(not part for part in parts):
        raise ValueError(
            f"invalid --clocks {text!r}: expected a comma-separated list "
            f"such as 30+0,60+0"
        )
    return [parse_time_control(part) for part in parts]


def clocks_label(clocks: list[TimeControl]) -> str:
    return " / ".join(tc.display() for tc in clocks)


def clock_for_game(index: int, clocks: list[TimeControl] | None = None) -> TimeControl:
    seq = clocks if clocks is not None else parse_clocks(DEFAULT_CLOCKS_SPEC)
    if not seq:
        raise ValueError("clocks list is empty")
    return seq[index % len(seq)]


def _clocks_arg(text: str) -> list[TimeControl]:
    try:
        return parse_clocks(text)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(str(exc)) from exc


@dataclass(frozen=True)
class GameRecord:
    index: int
    result: str
    ply: int
    we_are_white: bool
    clock_s: float
    pgn: str
    increment_s: float = 0.0
    clock_label: str = ""


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


@dataclass(frozen=True)
class EloEstimate:
    """Result of a UCI_Elo binary search.

    `bound` is `range` when the window sits inside the legal UCI_Elo limits,
    `at_most` after a loss at the minimum, and `at_least` after a win at the
    maximum. `lo`/`hi` are the final search window.
    """

    lo: int
    hi: int
    elo_min: int
    elo_max: int
    bound: str = "range"

    @property
    def midpoint(self) -> int:
        return (self.lo + self.hi) // 2

    @property
    def window(self) -> int:
        return self.hi - self.lo

    @property
    def value(self) -> int:
        if self.bound == "at_most":
            return self.elo_min
        if self.bound == "at_least":
            return self.elo_max
        return self.midpoint

    def display_elo(self) -> str:
        if self.bound == "at_most":
            return f"<= {self.elo_min}"
        if self.bound == "at_least":
            return f">= {self.elo_max}"
        return str(self.midpoint)


PlayMatchFn = Callable[[int], float | MatchScore]
LogFn = Callable[[str], None]


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


def uci_elo_limits_from_options(opts: object) -> tuple[int, int]:
    """Read Stockfish UCI_Elo min/max, falling back to the documented range."""
    try:
        present = "UCI_Elo" in opts  # type: ignore[operator]
    except TypeError:
        present = False
    if not present:
        return SF_UCI_ELO_MIN, SF_UCI_ELO_MAX
    opt = opts["UCI_Elo"]  # type: ignore[index]
    lo = int(opt.min) if getattr(opt, "min", None) is not None else SF_UCI_ELO_MIN
    hi = int(opt.max) if getattr(opt, "max", None) is not None else SF_UCI_ELO_MAX
    return lo, hi


def resolve_elo_bounds(
    elo_min: int | None,
    elo_max: int | None,
    uci_min: int = SF_UCI_ELO_MIN,
    uci_max: int = SF_UCI_ELO_MAX,
) -> tuple[int, int]:
    """Clamp an optional search window onto a legal UCI_Elo range."""
    lo = uci_min if elo_min is None else elo_min
    hi = uci_max if elo_max is None else elo_max
    lo = min(uci_max, max(uci_min, lo))
    hi = min(uci_max, max(uci_min, hi))
    if lo > hi:
        lo, hi = hi, lo
    return lo, hi


def _probe_result(result: float | MatchScore) -> tuple[float, str]:
    if isinstance(result, MatchScore):
        return result.points, f"W-D-L {result.wins}-{result.draws}-{result.losses}"
    points = float(result)
    return points, f"points {points:.1f}"


def binary_search_elo(
    play_match: PlayMatchFn,
    elo_min: int,
    elo_max: int,
    *,
    tolerance: int = DEFAULT_TOLERANCE,
    min_points: float = DEFAULT_MIN_POINTS,
    log: LogFn | None = None,
) -> EloEstimate:
    """Find the Stockfish UCI_Elo where `play_match` is about even.

    `play_match(elo)` returns match points (or a MatchScore). Points >=
    `min_points` means we are at least `elo` (same rule as the strength
    floor). Iterations are sequential; parallelism belongs inside
    `play_match`. Stops when `hi - lo <= 2 * tolerance` (±tolerance around
    the midpoint).
    """
    if elo_min > elo_max:
        raise ValueError("elo_min must be <= elo_max")
    if tolerance < 1:
        raise ValueError("tolerance must be >= 1")

    def emit(msg: str) -> None:
        if log is not None:
            log(msg)

    cache: dict[int, tuple[float, str]] = {}

    def probe(elo: int) -> tuple[float, str]:
        if elo not in cache:
            cache[elo] = _probe_result(play_match(elo))
        return cache[elo]

    lo, hi = elo_min, elo_max
    max_window = tolerance * 2

    while hi - lo > max_window:
        mid = (lo + hi) // 2
        if mid <= lo:
            mid = lo + 1
        if mid >= hi:
            mid = hi - 1
        if mid <= lo or mid >= hi:
            break
        points, detail = probe(mid)
        if meets_floor(points, min_points):
            lo = mid
            decision = "engine >= mid → lo = mid"
        else:
            hi = mid
            decision = "engine < mid → hi = mid"
        emit(
            f"probe UCI_Elo={mid}  {detail}  points={points:.1f}  "
            f"lo={lo} hi={hi}  ({decision})"
        )

    if lo == elo_min:
        points, detail = probe(elo_min)
        emit(
            f"edge probe UCI_Elo={elo_min}  {detail}  points={points:.1f}  "
            f"lo={lo} hi={hi}"
        )
        if not meets_floor(points, min_points):
            emit(f"lost to minimum UCI_Elo={elo_min} → <= {elo_min}")
            return EloEstimate(
                lo=elo_min, hi=elo_min, elo_min=elo_min, elo_max=elo_max, bound="at_most"
            )

    if hi == elo_max:
        points, detail = probe(elo_max)
        emit(
            f"edge probe UCI_Elo={elo_max}  {detail}  points={points:.1f}  "
            f"lo={lo} hi={hi}"
        )
        if meets_floor(points, min_points):
            emit(f"beat maximum UCI_Elo={elo_max} → >= {elo_max}")
            return EloEstimate(
                lo=elo_max, hi=elo_max, elo_min=elo_min, elo_max=elo_max, bound="at_least"
            )

    emit(
        f"estimate { (lo + hi) // 2 }  window=[{lo}, {hi}]  "
        f"half-width={(hi - lo) / 2:.0f}"
    )
    return EloEstimate(lo=lo, hi=hi, elo_min=elo_min, elo_max=elo_max, bound="range")


def format_strength_block(
    estimate: EloEstimate | None,
    *,
    tolerance: int = DEFAULT_TOLERANCE,
    games: int = DEFAULT_GAMES,
    clocks: str = "30+0 / 60+0",
) -> str:
    """README block between the elo-estimate HTML markers."""
    label = "unmeasured" if estimate is None else f"{estimate.display_elo()} Elo"
    return (
        f"{ELO_MARK_START}\n"
        f"Estimated strength: **{label}** "
        f"(±{tolerance}, vs Stockfish `UCI_Elo`, {games} games at {clocks})\n"
        f"{ELO_MARK_END}"
    )


def replace_readme_estimate(text: str, block: str) -> str:
    """Replace or insert the marked strength block. Idempotent."""
    pattern = re.compile(
        re.escape(ELO_MARK_START) + r".*?" + re.escape(ELO_MARK_END),
        flags=re.DOTALL,
    )
    compact = block.strip()
    if pattern.search(text):
        return pattern.sub(compact, text, count=1)
    match = re.match(r"(# [^\n]+\n\n(?:[^\n]+\n)+)\n", text)
    if match:
        return f"{match.group(1)}\n{compact}\n\n{text[match.end():]}"
    return text.rstrip() + "\n\n" + compact + "\n"


def update_readme_estimate(path: Path, block: str) -> bool:
    """Write `block` into README. True when the file contents changed."""
    text = path.read_text(encoding="utf-8")
    new_text = replace_readme_estimate(text, block)
    if new_text == text:
        return False
    path.write_text(new_text, encoding="utf-8")
    return True


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
        lo, hi = uci_elo_limits_from_options(opts)
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


def _clock_for_game(index: int, clocks: list[TimeControl] | None = None) -> TimeControl:
    """Game clock by index. Default cycles 30+0 / 60+0."""
    return clock_for_game(index, clocks)


def _our_move(fen: str, clock_s: float, increment_s: float, ply: int) -> str:
    from engine import allocate_time, search_position

    budget = allocate_time(clock_s, increment_s, None, ply=ply)
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
    clock: TimeControl,
    max_ply: int,
) -> tuple[str, object]:
    """Play one game on `clock` (whole-game base + increment per side).

    Each ply gets a short think from `allocate_time` (tens of ms). Do not send
    the remaining base clock to Stockfish as wtime/btime — it would then spend
    seconds on a single move.
    """
    import time

    import chess
    import chess.engine

    from engine import allocate_time

    board = chess.Board()
    for uci in opening:
        board.push_uci(uci)

    white_clock = clock.base_s
    black_clock = clock.base_s
    increment = clock.increment_s

    while not board.is_game_over(claim_draw=True) and board.ply() < max_ply:
        mover_white = board.turn == chess.WHITE
        ours = mover_white == we_are_white
        side_clock = white_clock if mover_white else black_clock
        budget = allocate_time(side_clock, increment, None, ply=board.ply())
        started = time.perf_counter()
        if ours:
            try:
                uci = _our_move(board.fen(), side_clock, increment, board.ply())
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
            if not flagged:
                white_clock += increment
        else:
            black_clock -= elapsed
            flagged = black_clock <= 0.0
            if not flagged:
                black_clock += increment
        if flagged:
            return ("0-1" if ours else "1-0"), board

    outcome = board.outcome(claim_draw=True)
    if outcome is None:
        return "1/2-1/2", board
    if outcome.winner is None:
        return "1/2-1/2", board
    we_won = (outcome.winner == chess.WHITE) == we_are_white
    return ("1-0" if we_won else "0-1"), board


def _board_pgn(board: object, we_are_white: bool, clock: TimeControl | None = None) -> str:
    import chess.pgn

    game = chess.pgn.Game.from_board(board)
    game.headers["Event"] = "ChessEngine gauntlet"
    game.headers["White"] = "ChessEngine" if we_are_white else "Stockfish"
    game.headers["Black"] = "Stockfish" if we_are_white else "ChessEngine"
    if clock is not None:
        game.headers["TimeControl"] = clock.display()
    outcome = board.outcome(claim_draw=True)  # type: ignore[attr-defined]
    game.headers["Result"] = outcome.result() if outcome else "1/2-1/2"
    return str(game)


def _record_from_play(
    index: int,
    stockfish: object,
    max_ply: int,
    clocks: list[TimeControl] | None = None,
) -> GameRecord:
    we_are_white = index % 2 == 0
    clock = _clock_for_game(index, clocks)
    result, board = _play_game(
        stockfish=stockfish,
        opening=OPENINGS[index % len(OPENINGS)],
        we_are_white=we_are_white,
        clock=clock,
        max_ply=max_ply,
    )
    return GameRecord(
        index=index,
        result=result,
        ply=board.ply(),
        we_are_white=we_are_white,
        clock_s=clock.base_s,
        pgn=_board_pgn(board, we_are_white, clock),
        increment_s=clock.increment_s,
        clock_label=clock.display(),
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
    clock = record.clock_label or f"{record.clock_s:.0f}+{_fmt_seconds(record.increment_s)}"
    print(
        f"  game {record.index + 1}/{games}  us={color}  {tag}  "
        f"clock={clock}  ply={record.ply}  W-D-L {wins}-{draws}-{losses}",
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


def probe_uci_elo_range(sf_path: Path) -> tuple[int, int]:
    import chess.engine

    engine = chess.engine.SimpleEngine.popen_uci(str(sf_path), timeout=2.0)
    try:
        return uci_elo_limits_from_options(engine.options)
    finally:
        _kill_engine(engine)


def _init_worker(sf_path: str, elo: int, clocks: list[TimeControl]) -> None:
    global _WORKER_ENGINE, _WORKER_CLOCKS
    import chess.engine

    _WORKER_ENGINE = chess.engine.SimpleEngine.popen_uci(sf_path, timeout=2.0)
    _configure_stockfish(_WORKER_ENGINE, elo)
    _WORKER_CLOCKS = list(clocks)
    atexit.register(_shutdown_worker)


def _shutdown_worker() -> None:
    global _WORKER_ENGINE
    engine = _WORKER_ENGINE
    _WORKER_ENGINE = None
    if engine is not None:
        _kill_engine(engine)


def _worker_play(index: int, max_ply: int) -> GameRecord:
    assert _WORKER_ENGINE is not None
    return _record_from_play(index, _WORKER_ENGINE, max_ply, _WORKER_CLOCKS)


def _args_clocks(args: argparse.Namespace) -> list[TimeControl]:
    clocks = getattr(args, "clocks", None)
    if isinstance(clocks, list) and clocks:
        return clocks
    return parse_clocks(DEFAULT_CLOCKS_SPEC)


def run_gauntlet(args: argparse.Namespace) -> MatchScore:
    sf_path = _find_stockfish(args.stockfish)
    pgn_path = Path(args.pgn) if args.pgn else None
    clocks = _args_clocks(args)
    concurrency = min(args.concurrency, args.games, MAX_CONCURRENCY)

    strength = _probe_strength(sf_path, args.elo)
    print(f"Stockfish: {sf_path}")
    print(f"Strength: {strength}")
    print(
        f"Games: {args.games}  concurrency: {concurrency}/{MAX_CONCURRENCY}  "
        f"clocks: {clocks_label(clocks)} (cycle by game index)  "
        f"min points: {args.min_points:g}"
    )

    if concurrency <= 1:
        stream = _iter_serial(args.games, str(sf_path), args.elo, args.max_ply, clocks)
    else:
        stream = _iter_parallel(
            args.games, str(sf_path), args.elo, args.max_ply, concurrency, clocks
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
    games: int,
    sf_path: str,
    elo: int,
    max_ply: int,
    clocks: list[TimeControl],
) -> Iterator[GameRecord]:
    import chess.engine

    engine = chess.engine.SimpleEngine.popen_uci(sf_path, timeout=2.0)
    try:
        _configure_stockfish(engine, elo)
        for i in range(games):
            record = _record_from_play(i, engine, max_ply, clocks)
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
    clocks: list[TimeControl],
) -> Iterator[GameRecord]:
    ctx = multiprocessing.get_context("spawn")
    pool = ProcessPoolExecutor(
        max_workers=concurrency,
        mp_context=ctx,
        initializer=_init_worker,
        initargs=(sf_path, elo, clocks),
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
        f"game clocks: {clocks_label(_args_clocks(args))}",
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
                f"- Clocks: {clocks_label(_args_clocks(args))}",
                f"- Elo diff (logistic): {diff_s}",
                f"- Crashes: {score.crashes}, illegal moves: {score.illegal}",
                "",
            ]
        )
        with open(summary, "a", encoding="utf-8") as fh:
            fh.write(body)


def _parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "time controls (bare integers are seconds, not minutes):\n"
            "  30              30s sudden death\n"
            "  30+0            same as 30\n"
            "  60+1            60s + 1s increment (Fischer)\n"
            "  300             5-minute sudden death\n"
            "  5m+0            5 minutes (use m / min suffix for minutes)\n"
            "  3+2             3 seconds + 2s increment, not 3 minutes\n"
            "  1m30s+2         90s + 2s increment\n"
            "  30+0,60+0       cycle by game index (default; even=30+0, odd=60+0)\n"
            "\n"
            "concurrency: 1–4 games at once. Strength gate defaults to 2;\n"
            "--calibrate defaults to 4. Binary-search probes are sequential;\n"
            "only games inside one probe run in parallel.\n"
        ),
    )
    p.add_argument("--games", type=int, default=DEFAULT_GAMES)
    p.add_argument("--elo", type=int, default=DEFAULT_ELO, help="Stockfish UCI_Elo")
    p.add_argument(
        "--min-points",
        type=float,
        default=DEFAULT_MIN_POINTS,
        help="Minimum points (win=1, draw=0.5, loss=0). Calibrate uses this as even.",
    )
    p.add_argument(
        "--concurrency",
        type=int,
        default=None,
        help=(
            f"Parallel games in one match/probe (1–{MAX_CONCURRENCY}). "
            f"Default {DEFAULT_CONCURRENCY} for the strength gate, "
            f"{DEFAULT_CALIBRATE_CONCURRENCY} with --calibrate."
        ),
    )
    p.add_argument(
        "--clocks",
        type=_clocks_arg,
        default=DEFAULT_CLOCKS_SPEC,
        help=(
            "Comma-separated time controls cycled by game index. "
            f"Default: {DEFAULT_CLOCKS_SPEC}. Bare numbers are seconds."
        ),
    )
    p.add_argument("--max-ply", type=int, default=DEFAULT_MAX_PLY)
    p.add_argument("--stockfish", default=None, help="Path to Stockfish binary")
    p.add_argument("--pgn", default=None, help="Write games to this PGN path")
    p.add_argument(
        "--calibrate",
        "--binary-search",
        dest="calibrate",
        action="store_true",
        help="Binary-search UCI_Elo until the window is ±tolerance",
    )
    p.add_argument(
        "--elo-min",
        type=int,
        default=None,
        help=f"Search lower bound (default: Stockfish UCI_Elo min, {SF_UCI_ELO_MIN})",
    )
    p.add_argument(
        "--elo-max",
        type=int,
        default=None,
        help=f"Search upper bound (default: Stockfish UCI_Elo max, {SF_UCI_ELO_MAX})",
    )
    p.add_argument(
        "--tolerance",
        type=int,
        default=DEFAULT_TOLERANCE,
        help="Half-window in Elo (stop when hi-lo <= 2*tolerance). Default 50.",
    )
    p.add_argument(
        "--update-readme",
        default=None,
        help="Replace the <!-- elo-estimate --> block in this README path",
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the plan and exit without starting Stockfish or games",
    )
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
    args = p.parse_args(argv)
    if args.concurrency is None:
        args.concurrency = (
            DEFAULT_CALIBRATE_CONCURRENCY if args.calibrate else DEFAULT_CONCURRENCY
        )
    return args


def validate_concurrency(n: int) -> None:
    if n < 1 or n > MAX_CONCURRENCY:
        raise ValueError(
            f"concurrency must be 1–{MAX_CONCURRENCY} (got {n}). "
            f"A GitHub ubuntu-latest runner has ~4 vCPU; each game is ChessEngine "
            f"+ 1-thread Stockfish. {MAX_CONCURRENCY} pairs is about 8 threads and "
            f"stays near the ~100 Elo contention budget at 35–100ms/move."
        )


def _run_dry_run(args: argparse.Namespace) -> int:
    clocks = _args_clocks(args)
    elo_min, elo_max = resolve_elo_bounds(args.elo_min, args.elo_max)
    mode = "calibrate" if args.calibrate else "gauntlet"
    print(f"{mode} plan (dry-run; no games)")
    print(f"  games per match: {args.games}")
    print(f"  clocks: {clocks_label(clocks)}  (cycle by game index)")
    print(
        f"  concurrency: {args.concurrency}  "
        f"(max {MAX_CONCURRENCY}; games in a probe only; probes sequential)"
    )
    print(f"  min points: {args.min_points:g}")
    if args.calibrate:
        print(f"  UCI_Elo search: {elo_min}–{elo_max}  tolerance ±{args.tolerance}")
        print("  decision: points >= min-points → lo = mid, else hi = mid")
        print(f"  README: {args.update_readme or '(not writing)'}")
        print(
            format_strength_block(
                None,
                tolerance=args.tolerance,
                games=args.games,
                clocks=clocks_label(clocks),
            )
        )
    else:
        print(f"  opponent UCI_Elo: {args.elo}")
    return 0


def _write_calibrate_summary(estimate: EloEstimate, args: argparse.Namespace) -> None:
    clocks = clocks_label(_args_clocks(args))
    lines = [
        f"Estimated strength: {estimate.display_elo()} Elo",
        f"Search window: [{estimate.lo}, {estimate.hi}] "
        f"(tolerance ±{args.tolerance})",
        f"UCI_Elo range: [{estimate.elo_min}, {estimate.elo_max}]",
        f"Bound: {estimate.bound}",
        f"Clocks: {clocks}  games: {args.games}  "
        f"concurrency: {args.concurrency}",
    ]
    print("\n".join(lines), flush=True)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary:
        return
    body = "\n".join(
        [
            "## Elo estimate",
            "",
            f"- Estimated strength: **{estimate.display_elo()} Elo** "
            f"(±{args.tolerance})",
            f"- Window: [{estimate.lo}, {estimate.hi}]  bound: {estimate.bound}",
            f"- Stockfish UCI_Elo range: {estimate.elo_min}–{estimate.elo_max}",
            f"- Match: {args.games} games at {clocks}, "
            f"concurrency {args.concurrency} (probes sequential)",
            f"- Decision: ≥ {args.min_points:g} points means engine ≥ mid",
            "",
        ]
    )
    with open(summary, "a", encoding="utf-8") as fh:
        fh.write(body)


def _run_calibrate(args: argparse.Namespace) -> int:
    sf_path = _find_stockfish(args.stockfish)
    try:
        uci_min, uci_max = probe_uci_elo_range(sf_path)
    except Exception as exc:
        print(
            f"could not probe UCI_Elo range ({exc}); "
            f"using {SF_UCI_ELO_MIN}–{SF_UCI_ELO_MAX}",
            flush=True,
        )
        uci_min, uci_max = SF_UCI_ELO_MIN, SF_UCI_ELO_MAX
    elo_min, elo_max = resolve_elo_bounds(args.elo_min, args.elo_max, uci_min, uci_max)
    print(
        f"Calibrating UCI_Elo in [{elo_min}, {elo_max}]  "
        f"tolerance ±{args.tolerance}  "
        f"(games in a probe are parallel; probes are sequential)",
        flush=True,
    )

    def play_match(elo: int) -> MatchScore:
        args.elo = elo
        print(f"=== probe UCI_Elo={elo} ===", flush=True)
        score = run_gauntlet(args)
        if score.crashes or score.illegal:
            raise RuntimeError(
                f"probe at UCI_Elo={elo} aborted: "
                f"crashes={score.crashes} illegal={score.illegal}"
            )
        return score

    estimate = binary_search_elo(
        play_match,
        elo_min,
        elo_max,
        tolerance=args.tolerance,
        min_points=args.min_points,
        log=print,
    )
    _write_calibrate_summary(estimate, args)
    if args.update_readme:
        block = format_strength_block(
            estimate,
            tolerance=args.tolerance,
            games=args.games,
            clocks=clocks_label(_args_clocks(args)),
        )
        path = Path(args.update_readme)
        changed = update_readme_estimate(path, block)
        print(f"README {path}: {'updated' if changed else 'unchanged'}", flush=True)
    return 0


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    if args.games < 1:
        print("games must be >= 1", file=sys.stderr)
        return 2
    try:
        validate_concurrency(args.concurrency)
    except ValueError as exc:
        print(exc, file=sys.stderr)
        return 2
    if args.tolerance < 1:
        print("tolerance must be >= 1", file=sys.stderr)
        return 2
    if args.dry_run:
        return _run_dry_run(args)
    try:
        if args.calibrate:
            return _run_calibrate(args)
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
