"""FEN search contract used by the lichess-bot homemade adapter."""

from __future__ import annotations

import chessengine as ce

START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


def allocate_time(
    our: float | None,
    inc: float | None,
    sudden: float | None,
) -> float:
    if sudden is not None and sudden > 0:
        return max(0.05, sudden * 0.9 - 0.1)
    clock = our if our is not None else 5.0
    increment = inc if inc is not None else 0.0
    return max(0.05, min(8.0, clock / 25.0 + increment * 0.5) - 0.1)


def choose_depth(our: float | None, requested: int | None) -> int:
    depth = requested if requested else 4
    if our is not None and our < 10:
        depth = min(depth, 3)
    return max(1, depth)


def search_position(
    fen: str,
    *,
    max_seconds: float = 0.0,
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
