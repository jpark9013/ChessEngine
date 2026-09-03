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

    def test_time_allocator_live(self) -> None:
        overhead = 0.35
        sudden = allocate_time(None, None, 1.0, mode="live")
        self.assertGreaterEqual(sudden.hard, 0.04)
        self.assertLessEqual(sudden.hard, 0.95)
        self.assertLessEqual(sudden.target, sudden.hard)

        low = allocate_time(5.0, 0.0, None, mode="live")
        self.assertLessEqual(low.hard, 5.0 - overhead)
        self.assertLessEqual(low.target, low.hard)
        self.assertGreater(low.target, 0.05)

        # 1+0 opening: hundreds of ms to ~2s, not the old 35ms handicap.
        full_bullet = allocate_time(60.0, 0.0, None, mode="live")
        self.assertGreaterEqual(full_bullet.target, 0.30)
        self.assertLessEqual(full_bullet.target, 2.0)
        self.assertLess(full_bullet.target, full_bullet.hard)
        self.assertLessEqual(full_bullet.hard, 60.0 - overhead)
        self.assertGreater(full_bullet.max_depth, 16)

        with_inc = allocate_time(10.0, 1.0, None, mode="live")
        no_inc = allocate_time(10.0, 0.0, None, mode="live")
        self.assertGreater(with_inc.target, no_inc.target)
        self.assertGreaterEqual(with_inc.hard, no_inc.hard - 1e-9)

        mid_bullet = allocate_time(30.0, 0.0, None, ply=20, mode="live")
        self.assertGreaterEqual(mid_bullet.target, 0.20)
        self.assertLessEqual(mid_bullet.hard, 30.0 - overhead)
        self.assertLessEqual(mid_bullet.target, mid_bullet.hard)

        late = allocate_time(1.0, 0.0, None, ply=80, mode="live")
        self.assertLessEqual(late.hard, 0.30)
        self.assertLessEqual(late.target, late.hard)

        panic = allocate_time(0.3, 0.0, None, mode="live")
        self.assertLessEqual(panic.hard, 0.12)
        self.assertLessEqual(panic.max_depth, 4)
        self.assertLessEqual(panic.hard, 0.3)

    def test_time_allocator_never_exceeds_usable(self) -> None:
        for clock, inc, ply in (
            (60.0, 0.0, 0),
            (30.0, 0.0, 20),
            (10.0, 1.0, 8),
            (2.0, 0.0, 40),
            (1.0, 0.0, 80),
        ):
            budget = allocate_time(clock, inc, None, ply=ply, mode="live")
            usable = max(0.05, clock - 0.35)
            self.assertLessEqual(budget.hard, usable + 1e-9)
            self.assertLessEqual(budget.target, budget.hard + 1e-9)
            self.assertLessEqual(budget.hard, clock - 0.05)

    def test_gauntlet_keeps_match_fairness(self) -> None:
        g = allocate_time(60.0, 0.0, None, mode="gauntlet")
        self.assertAlmostEqual(g.target, 0.035)
        self.assertAlmostEqual(g.hard, 0.10)
        self.assertEqual(g.max_depth, 12)
        mid = allocate_time(30.0, 0.0, None, ply=20, mode="gauntlet")
        self.assertAlmostEqual(mid.target, 0.035)
        self.assertAlmostEqual(mid.hard, 0.10)

    def test_gauntlet_env_flag(self) -> None:
        import os
        from unittest.mock import patch

        with patch.dict(os.environ, {"GAUNTLET": "1"}):
            env = allocate_time(60.0, 0.0, None)
            self.assertAlmostEqual(env.target, 0.035)
            self.assertAlmostEqual(env.hard, 0.10)

    def test_depth_allocator(self) -> None:
        self.assertEqual(choose_depth(0.3, None), 1)
        self.assertEqual(choose_depth(5.0, 4), 4)
        self.assertEqual(choose_depth(60.0, None), 64)
        self.assertEqual(choose_depth(60.0, 6), 6)


if __name__ == "__main__":
    unittest.main()
