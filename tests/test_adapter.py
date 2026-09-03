"""Adapter contract tests. No live Lichess."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "bot"))

import chessengine as ce

from engine import START_FEN, allocate_time, choose_depth, search_position


class TestAdapter(unittest.TestCase):
    def test_search_returns_legal_opening(self) -> None:
        legal = {move.uci() for move in ce.Board.from_fen(START_FEN).legal_moves()}
        result = search_position(START_FEN, depth=2, max_seconds=1.0)
        self.assertIn(result["uci"], legal)
        self.assertIn("score_cp", result)
        self.assertGreaterEqual(result["depth"], 1)
        self.assertGreaterEqual(result["nodes"], 1)

    def test_root_moves_restriction(self) -> None:
        result = search_position(START_FEN, depth=2, max_seconds=1.0, root_moves=["a2a3"])
        self.assertEqual(result["uci"], "a2a3")

    def test_back_rank_mate(self) -> None:
        result = search_position(
            "6k1/5ppp/8/8/8/8/5PPP/4R1K1 w - - 0 1",
            depth=2,
            max_seconds=2.0,
        )
        self.assertEqual(result["uci"], "e1e8")

    def test_kr_vs_k_has_move(self) -> None:
        result = search_position("7k/8/8/8/8/8/8/6RK w - - 0 1", depth=1)
        self.assertNotEqual(result["uci"], "0000")

    def test_time_allocator(self) -> None:
        sudden = allocate_time(None, None, 1.0)
        self.assertGreaterEqual(sudden.hard, 0.04)
        self.assertLessEqual(sudden.hard, 0.95)
        self.assertLessEqual(sudden.target, sudden.hard)

        low = allocate_time(5.0, 0.0, None)
        self.assertLessEqual(low.hard, 0.6)
        self.assertLessEqual(low.target, low.hard)

        full_bullet = allocate_time(60.0, 0.0, None)
        self.assertLessEqual(full_bullet.target, 2.6)
        self.assertLessEqual(full_bullet.hard, 4.1)
        self.assertGreater(full_bullet.max_depth, 8)

        panic = allocate_time(0.3, 0.0, None)
        self.assertLessEqual(panic.hard, 0.12)
        self.assertEqual(panic.max_depth, 1)

    def test_depth_allocator(self) -> None:
        self.assertEqual(choose_depth(0.3, None), 1)
        self.assertEqual(choose_depth(5.0, 4), 4)
        self.assertEqual(choose_depth(60.0, None), 24)
        self.assertEqual(choose_depth(60.0, 6), 6)


if __name__ == "__main__":
    unittest.main()
