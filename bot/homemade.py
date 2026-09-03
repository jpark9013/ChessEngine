"""lichess-bot homemade engine — class name must match config.yml engine.name."""

from __future__ import annotations

import chess
from chess.engine import Limit, PlayResult
from lib.engine_wrapper import MinimalEngine
from lib.lichess_types import MOVE

from engine import allocate_time, choose_depth, search_position


class ExampleEngine(MinimalEngine):
    """lichess-bot imports this name from homemade.py before loading ChessEngine."""


class ChessEngine(ExampleEngine):
    def search(
        self,
        board: chess.Board,
        time_limit: Limit,
        ponder: bool,  # noqa: ARG002
        draw_offered: bool,
        root_moves: MOVE,
    ) -> PlayResult:
        if board.turn == chess.WHITE:
            our, inc = time_limit.white_clock, time_limit.white_inc
        else:
            our, inc = time_limit.black_clock, time_limit.black_inc

        budget = allocate_time(our, inc, time_limit.time, ply=board.ply())
        depth = min(choose_depth(our, time_limit.depth), budget.max_depth)
        roots = None
        if isinstance(root_moves, list):
            roots = [move.uci() for move in root_moves]

        result = search_position(
            board.fen(),
            max_seconds=budget.hard,
            target_seconds=budget.target,
            depth=depth,
            root_moves=roots,
        )

        try:
            move = chess.Move.from_uci(result["uci"])
            if move not in board.legal_moves:
                raise ValueError("illegal")
        except Exception:
            move = next(iter(board.legal_moves))

        info = {
            "score": chess.engine.PovScore(chess.engine.Cp(result["score_cp"]), board.turn),
            "depth": result["depth"],
            "nodes": result["nodes"],
        }
        return PlayResult(
            move,
            None,
            info=info,
            draw_offered=bool(draw_offered and result["draw_offered"]),
        )
