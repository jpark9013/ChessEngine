#!/usr/bin/env python3
"""Gauntlet: our engine vs strength-limited Stockfish.

Used as the CI publish floor. Both sides get the same movetime. Openings are
short fixed lines so games are not identical. Requires python-chess, a
Stockfish binary, and the chessengine module on PYTHONPATH.
"""

from __future__ import annotations

import argparse
import math
import os
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path

DEFAULT_GAMES = 40
DEFAULT_ELO = 1600
DEFAULT_MIN_SCORE = 0.35
DEFAULT_MOVETIME_MS = 200
DEFAULT_MAX_PLY = 180
SF_THREADS = 1
SF_HASH_MB = 16

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


def _our_move(fen: str, movetime_s: float) -> str:
    from engine import search_position

    result = search_position(
        fen,
        max_seconds=movetime_s,
        target_seconds=max(0.04, movetime_s * 0.85),
        depth=12,
    )
    return str(result["uci"])


def _play_game(
    *,
    stockfish: object,
    opening: list[str],
    we_are_white: bool,
    movetime_s: float,
    max_ply: int,
) -> tuple[str, object]:
    """Return (result_from_our_side, board). result is 1-0, 0-1, 1/2-1/2, illegal, crash."""
    import chess
    import chess.engine

    board = chess.Board()
    for uci in opening:
        board.push_uci(uci)

    while not board.is_game_over(claim_draw=True) and board.ply() < max_ply:
        ours = (board.turn == chess.WHITE) == we_are_white
        if ours:
            try:
                uci = _our_move(board.fen(), movetime_s)
                move = chess.Move.from_uci(uci)
            except Exception:
                return "crash", board
            if move not in board.legal_moves:
                return "illegal", board
            board.push(move)
        else:
            played = stockfish.play(  # type: ignore[attr-defined]
                board, chess.engine.Limit(time=movetime_s)
            )
            if played.move is None:
                return "crash", board
            board.push(played.move)

    outcome = board.outcome(claim_draw=True)
    if outcome is None:
        return "1/2-1/2", board
    if outcome.winner is None:
        return "1/2-1/2", board
    we_won = (outcome.winner == chess.WHITE) == we_are_white
    return ("1-0" if we_won else "0-1"), board


def run_gauntlet(args: argparse.Namespace) -> MatchScore:
    import chess.engine
    import chess.pgn

    sf_path = _find_stockfish(args.stockfish)
    movetime_s = args.movetime_ms / 1000.0
    pgn_path = Path(args.pgn) if args.pgn else None

    wins = draws = losses = crashes = illegal = 0
    engine = chess.engine.SimpleEngine.popen_uci(str(sf_path))
    try:
        strength = _configure_stockfish(engine, args.elo)
        print(f"Stockfish: {sf_path}")
        print(f"Strength: {strength}")
        print(
            f"Games: {args.games}  movetime: {args.movetime_ms}ms  "
            f"min score: {args.min_score:.0%}"
        )

        pgn_file = pgn_path.open("w", encoding="utf-8") if pgn_path else None
        try:
            for i in range(args.games):
                opening = OPENINGS[i % len(OPENINGS)]
                we_are_white = i % 2 == 0
                result, board = _play_game(
                    stockfish=engine,
                    opening=opening,
                    we_are_white=we_are_white,
                    movetime_s=movetime_s,
                    max_ply=args.max_ply,
                )
                if result == "crash":
                    crashes += 1
                    losses += 1
                    tag = "crash-loss"
                elif result == "illegal":
                    illegal += 1
                    losses += 1
                    tag = "illegal-loss"
                elif result == "1-0":
                    wins += 1
                    tag = "win"
                elif result == "0-1":
                    losses += 1
                    tag = "loss"
                else:
                    draws += 1
                    tag = "draw"

                color = "white" if we_are_white else "black"
                print(
                    f"  game {i + 1}/{args.games}  us={color}  {tag}  "
                    f"ply={board.ply()}  W-D-L {wins}-{draws}-{losses}",
                    flush=True,
                )

                if pgn_file is not None:
                    game = chess.pgn.Game.from_board(board)
                    game.headers["Event"] = "ChessEngine gauntlet"
                    game.headers["White"] = "ChessEngine" if we_are_white else "Stockfish"
                    game.headers["Black"] = "Stockfish" if we_are_white else "ChessEngine"
                    game.headers["Result"] = (
                        board.outcome(claim_draw=True).result()
                        if board.outcome(claim_draw=True)
                        else "1/2-1/2"
                    )
                    print(game, file=pgn_file, end="\n\n")

                if result in {"crash", "illegal"}:
                    print(f"aborting gauntlet after {result}", flush=True)
                    break
        finally:
            if pgn_file is not None:
                pgn_file.close()
    finally:
        engine.quit()

    return MatchScore(wins=wins, draws=draws, losses=losses, crashes=crashes, illegal=illegal)


def _write_summary(score: MatchScore, args: argparse.Namespace) -> None:
    diff = elo_from_score(score.fraction)
    diff_s = f"{diff:+.0f} vs opponent" if diff is not None else "n/a (0% or 100%)"
    lines = [
        f"W-D-L {score.wins}-{score.draws}-{score.losses}  "
        f"score {score.points:.1f}/{score.games} ({score.fraction:.1%})",
        f"estimated Elo difference: {diff_s}",
        f"crashes: {score.crashes}  illegal: {score.illegal}",
        f"opponent Elo setting: {args.elo}  movetime: {args.movetime_ms}ms",
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
                f"- Floor: {args.min_score:.0%} vs UCI_Elo {args.elo}",
                f"- Movetime: {args.movetime_ms} ms",
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
    p.add_argument("--min-score", type=float, default=DEFAULT_MIN_SCORE)
    p.add_argument("--movetime-ms", type=int, default=DEFAULT_MOVETIME_MS)
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
    if score.fraction + 1e-9 < args.min_score:
        print(
            f"FAIL: score {score.fraction:.1%} < floor {args.min_score:.0%}",
            file=sys.stderr,
        )
        return 1
    print("PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
