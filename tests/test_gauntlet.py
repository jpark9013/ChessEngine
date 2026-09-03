"""Gauntlet scoring helpers. Does not run Stockfish."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))

from gauntlet import elo_from_score, score_fraction


class TestGauntletScore(unittest.TestCase):
    def test_even_score_is_half(self) -> None:
        self.assertEqual(score_fraction(20, 0, 20), 0.5)
        self.assertEqual(score_fraction(10, 20, 10), 0.5)

    def test_all_wins(self) -> None:
        self.assertEqual(score_fraction(10, 0, 0), 1.0)

    def test_empty_is_zero(self) -> None:
        self.assertEqual(score_fraction(0, 0, 0), 0.0)

    def test_floor_example(self) -> None:
        # 40 games, 35% floor: 8 wins + 12 draws + 20 losses = 14/40 = 35%
        self.assertAlmostEqual(score_fraction(8, 12, 20), 0.35)

    def test_elo_even_is_zero(self) -> None:
        diff = elo_from_score(0.5)
        assert diff is not None
        self.assertAlmostEqual(diff, 0.0, places=6)

    def test_elo_ends_are_undefined(self) -> None:
        self.assertIsNone(elo_from_score(0.0))
        self.assertIsNone(elo_from_score(1.0))

    def test_elo_weaker_is_negative(self) -> None:
        diff = elo_from_score(0.35)
        assert diff is not None
        self.assertLess(diff, -100)
        self.assertGreater(diff, -150)


if __name__ == "__main__":
    unittest.main()
