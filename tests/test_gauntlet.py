"""Gauntlet scoring helpers. Does not run Stockfish."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))

from gauntlet import (
    MatchScore,
    _clock_for_game,
    _parse_args,
    early_stop_decision,
    elo_from_score,
    meets_floor,
    score_fraction,
)


class TestGauntletScore(unittest.TestCase):
    def test_even_score_is_half(self) -> None:
        self.assertEqual(score_fraction(20, 0, 20), 0.5)
        self.assertEqual(score_fraction(10, 20, 10), 0.5)

    def test_all_wins(self) -> None:
        self.assertEqual(score_fraction(10, 0, 0), 1.0)

    def test_empty_is_zero(self) -> None:
        self.assertEqual(score_fraction(0, 0, 0), 0.0)

    def test_exactly_four_points_meets_floor(self) -> None:
        # 8 games, 4-point floor: 3 wins + 2 draws + 3 losses = 4.0 must PASS
        score = MatchScore(3, 2, 3)
        self.assertEqual(score.points, 4.0)
        self.assertTrue(meets_floor(score.points, 4.0))
        self.assertAlmostEqual(score_fraction(3, 2, 3), 0.5)

    def test_three_five_points_fails_floor(self) -> None:
        score = MatchScore(3, 1, 4)
        self.assertEqual(score.points, 3.5)
        self.assertFalse(meets_floor(score.points, 4.0))

    def test_meets_floor_is_greater_or_equal(self) -> None:
        self.assertTrue(meets_floor(4.0, 4.0))
        self.assertTrue(meets_floor(4.5, 4.0))
        self.assertFalse(meets_floor(3.5, 4.0))
        self.assertFalse(meets_floor(0.0, 4.0))

    def test_early_pass_when_points_meet_floor(self) -> None:
        self.assertEqual(early_stop_decision(4.0, 4, 4.0), "pass")
        self.assertEqual(early_stop_decision(4.5, 2, 4.0), "pass")
        self.assertEqual(early_stop_decision(4.0, 0, 4.0), "pass")

    def test_early_fail_when_remaining_cannot_reach_floor(self) -> None:
        self.assertEqual(early_stop_decision(1.5, 2, 4.0), "fail")
        self.assertEqual(early_stop_decision(3.0, 0, 4.0), "fail")
        self.assertEqual(early_stop_decision(3.5, 0, 4.0), "fail")

    def test_continue_when_remaining_can_still_decide(self) -> None:
        # 3.0 + 1 remaining can still reach exactly 4.0
        self.assertIsNone(early_stop_decision(3.0, 1, 4.0))
        self.assertIsNone(early_stop_decision(2.0, 2, 4.0))
        self.assertIsNone(early_stop_decision(3.5, 1, 4.0))
        self.assertIsNone(early_stop_decision(0.0, 8, 4.0))

    def test_cli_defaults(self) -> None:
        args = _parse_args([])
        self.assertEqual(args.games, 8)
        self.assertEqual(args.min_points, 4.0)
        self.assertEqual(args.concurrency, 2)
        self.assertEqual(args.elo, 2200)

    def test_clocks_split_eight_games(self) -> None:
        clocks = [_clock_for_game(i) for i in range(8)]
        self.assertEqual(clocks.count(30.0), 4)
        self.assertEqual(clocks.count(60.0), 4)

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
