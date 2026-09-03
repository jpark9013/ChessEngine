"""Gauntlet scoring, clocks, and Elo binary search. Does not run Stockfish."""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))

from gauntlet import (
    DEFAULT_TOLERANCE,
    ELO_MARK_END,
    ELO_MARK_START,
    MAX_CONCURRENCY,
    SF_UCI_ELO_MAX,
    SF_UCI_ELO_MIN,
    EloEstimate,
    MatchScore,
    _clock_for_game,
    _parse_args,
    binary_search_elo,
    clock_for_game,
    clocks_label,
    early_stop_decision,
    elo_from_score,
    format_strength_block,
    main,
    meets_floor,
    parse_clocks,
    parse_time_control,
    replace_readme_estimate,
    resolve_elo_bounds,
    score_fraction,
    uci_elo_limits_from_options,
    update_readme_estimate,
    validate_concurrency,
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
        self.assertFalse(args.calibrate)
        self.assertEqual(args.tolerance, 50)
        self.assertEqual([tc.display() for tc in args.clocks], ["30+0", "60+0"])

    def test_cli_calibrate_defaults_concurrency_four(self) -> None:
        args = _parse_args(["--calibrate"])
        self.assertTrue(args.calibrate)
        self.assertEqual(args.concurrency, 4)
        self.assertEqual(args.tolerance, 50)

    def test_cli_binary_search_alias(self) -> None:
        args = _parse_args(["--binary-search", "--concurrency", "3"])
        self.assertTrue(args.calibrate)
        self.assertEqual(args.concurrency, 3)

    def test_help_exits_cleanly(self) -> None:
        with self.assertRaises(SystemExit) as ctx:
            _parse_args(["--help"])
        self.assertEqual(ctx.exception.code, 0)

    def test_clocks_split_eight_games(self) -> None:
        clocks = [_clock_for_game(i) for i in range(8)]
        self.assertEqual(sum(1 for c in clocks if c.base_s == 30.0), 4)
        self.assertEqual(sum(1 for c in clocks if c.base_s == 60.0), 4)
        self.assertTrue(all(c.increment_s == 0.0 for c in clocks))

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


class TestTimeControlParse(unittest.TestCase):
    def test_bare_seconds(self) -> None:
        self.assertEqual(parse_time_control("30").base_s, 30.0)
        self.assertEqual(parse_time_control("30").increment_s, 0.0)
        self.assertEqual(parse_time_control("60").display(), "60+0")
        self.assertEqual(parse_time_control("300").base_s, 300.0)

    def test_base_plus_increment(self) -> None:
        tc = parse_time_control("30+0")
        self.assertEqual(tc.base_s, 30.0)
        self.assertEqual(tc.increment_s, 0.0)
        self.assertEqual(tc.display(), "30+0")
        plus = parse_time_control("60+1")
        self.assertEqual((plus.base_s, plus.increment_s), (60.0, 1.0))
        self.assertEqual(parse_time_control("3+2").display(), "3+2")
        self.assertEqual(parse_time_control("15+10").display(), "15+10")

    def test_three_plus_two_is_seconds_not_minutes(self) -> None:
        tc = parse_time_control("3+2")
        self.assertEqual(tc.base_s, 3.0)
        self.assertNotEqual(tc.base_s, 180.0)

    def test_minute_suffix(self) -> None:
        self.assertEqual(parse_time_control("5m+0").base_s, 300.0)
        self.assertEqual(parse_time_control("5min+0").base_s, 300.0)
        self.assertEqual(parse_time_control("1m30s+2").base_s, 90.0)
        self.assertEqual(parse_time_control("1m30s+2").increment_s, 2.0)
        self.assertEqual(parse_time_control("2:00").base_s, 120.0)
        self.assertEqual(parse_time_control("2:00+1").increment_s, 1.0)

    def test_clocks_cycle_by_index(self) -> None:
        clocks = parse_clocks("30+0,60+0")
        self.assertEqual(clock_for_game(0, clocks).display(), "30+0")
        self.assertEqual(clock_for_game(1, clocks).display(), "60+0")
        self.assertEqual(clock_for_game(2, clocks).display(), "30+0")
        self.assertEqual(clocks_label(clocks), "30+0 / 60+0")

    def test_single_clock_repeats(self) -> None:
        clocks = parse_clocks("180+2")
        self.assertEqual(clock_for_game(0, clocks).display(), "180+2")
        self.assertEqual(clock_for_game(7, clocks).display(), "180+2")

    def test_cli_clocks(self) -> None:
        args = _parse_args(["--clocks", "5m+0,60+1"])
        self.assertEqual(args.clocks[0].base_s, 300.0)
        self.assertEqual(args.clocks[1].display(), "60+1")

    def test_reject_garbage(self) -> None:
        for spec in ("", "foo", "++", "30+", "+1", "30++0", "abc+1", "-10", "0", "0+1"):
            with self.subTest(spec=spec):
                with self.assertRaises(ValueError):
                    if "," in spec or spec == "":
                        parse_clocks(spec or ",")
                    else:
                        parse_time_control(spec)

    def test_reject_empty_clocks_list(self) -> None:
        with self.assertRaises(ValueError):
            parse_clocks("")
        with self.assertRaises(ValueError):
            parse_clocks("30+0,")
        with self.assertRaises(ValueError):
            parse_clocks(",60+0")

    def test_cli_rejects_garbage_clocks(self) -> None:
        with self.assertRaises(SystemExit):
            _parse_args(["--clocks", "not-a-clock"])


class TestConcurrency(unittest.TestCase):
    def test_cap_is_four(self) -> None:
        self.assertEqual(MAX_CONCURRENCY, 4)
        validate_concurrency(1)
        validate_concurrency(4)
        with self.assertRaises(ValueError) as ctx:
            validate_concurrency(5)
        self.assertIn("1–4", str(ctx.exception))
        with self.assertRaises(ValueError):
            validate_concurrency(0)

    def test_main_rejects_over_cap(self) -> None:
        self.assertEqual(main(["--concurrency", "8", "--dry-run"]), 2)

    def test_dry_run_ok(self) -> None:
        self.assertEqual(main(["--calibrate", "--dry-run"]), 0)
        self.assertEqual(main(["--dry-run", "--clocks", "15+10"]), 0)


class TestBinarySearchElo(unittest.TestCase):
    def test_converges_near_1800(self) -> None:
        probes: list[int] = []

        def play_match(elo: int) -> float:
            probes.append(elo)
            return 4.0 if elo <= 1800 else 3.0

        estimate = binary_search_elo(play_match, 1320, 3190, tolerance=50)
        self.assertEqual(estimate.bound, "range")
        self.assertLessEqual(estimate.window, 100)
        self.assertLessEqual(abs(estimate.midpoint - 1800), 50)
        self.assertTrue(probes)
        self.assertTrue(all(1320 <= elo <= 3190 for elo in probes))

    def test_tolerance_fifty_window_at_most_100(self) -> None:
        def play_match(elo: int) -> float:
            return 4.0 if elo <= 2000 else 3.5

        estimate = binary_search_elo(
            play_match, SF_UCI_ELO_MIN, SF_UCI_ELO_MAX, tolerance=DEFAULT_TOLERANCE
        )
        self.assertLessEqual(estimate.hi - estimate.lo, 100)

    def test_loses_to_minimum(self) -> None:
        def play_match(_elo: int) -> float:
            return 3.0

        estimate = binary_search_elo(play_match, 1320, 3190, tolerance=50)
        self.assertEqual(estimate.bound, "at_most")
        self.assertEqual(estimate.display_elo(), "<= 1320")
        self.assertEqual(estimate.value, 1320)

    def test_beats_maximum(self) -> None:
        def play_match(_elo: int) -> float:
            return 4.0

        estimate = binary_search_elo(play_match, 1320, 3190, tolerance=50)
        self.assertEqual(estimate.bound, "at_least")
        self.assertEqual(estimate.display_elo(), ">= 3190")
        self.assertEqual(estimate.value, 3190)

    def test_match_score_return_is_accepted(self) -> None:
        def play_match(elo: int) -> MatchScore:
            if elo <= 1600:
                return MatchScore(4, 0, 4)
            return MatchScore(2, 2, 4)

        estimate = binary_search_elo(play_match, 1320, 3190, tolerance=50)
        self.assertLessEqual(estimate.window, 100)
        self.assertLessEqual(abs(estimate.midpoint - 1600), 50)

    def test_logs_probes(self) -> None:
        lines: list[str] = []

        def play_match(elo: int) -> float:
            return 4.0 if elo <= 1800 else 3.0

        binary_search_elo(play_match, 1320, 3190, tolerance=50, log=lines.append)
        self.assertTrue(any("probe UCI_Elo=" in line for line in lines))
        self.assertTrue(any("lo=" in line and "hi=" in line for line in lines))

    def test_resolve_elo_bounds_clamps(self) -> None:
        self.assertEqual(resolve_elo_bounds(None, None), (1320, 3190))
        self.assertEqual(resolve_elo_bounds(1000, 4000), (1320, 3190))
        self.assertEqual(resolve_elo_bounds(2000, 1800), (1800, 2000))

    def test_uci_elo_limits_from_fake_options(self) -> None:
        class _Opt:
            def __init__(self, minimum: int, maximum: int) -> None:
                self.min = minimum
                self.max = maximum

        limits = uci_elo_limits_from_options({"UCI_Elo": _Opt(1400, 3000)})
        self.assertEqual(limits, (1400, 3000))
        self.assertEqual(uci_elo_limits_from_options({}), (1320, 3190))


class TestReadmeEstimate(unittest.TestCase):
    def test_format_unmeasured_and_number(self) -> None:
        placeholder = format_strength_block(None)
        self.assertIn(ELO_MARK_START, placeholder)
        self.assertIn(ELO_MARK_END, placeholder)
        self.assertIn("**unmeasured**", placeholder)
        estimate = EloEstimate(1750, 1850, 1320, 3190, "range")
        block = format_strength_block(estimate)
        self.assertIn("**1800 Elo**", block)
        self.assertIn("30+0 / 60+0", block)

    def test_format_edges(self) -> None:
        low = EloEstimate(1320, 1320, 1320, 3190, "at_most")
        self.assertIn("**<= 1320 Elo**", format_strength_block(low))
        high = EloEstimate(3190, 3190, 1320, 3190, "at_least")
        self.assertIn("**>= 3190 Elo**", format_strength_block(high))

    def test_replace_existing_markers(self) -> None:
        text = (
            "# Title\n\nintro\n\n"
            f"{ELO_MARK_START}\nEstimated strength: **unmeasured**\n"
            f"{ELO_MARK_END}\n\nrest\n"
        )
        block = format_strength_block(EloEstimate(1800, 1900, 1320, 3190))
        out = replace_readme_estimate(text, block)
        self.assertIn("**1850 Elo**", out)
        self.assertEqual(out.count(ELO_MARK_START), 1)

    def test_update_readme_file(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "README.md"
            path.write_text("# ChessEngine\n\nHello.\n\nMore.\n", encoding="utf-8")
            block = format_strength_block(None)
            self.assertTrue(update_readme_estimate(path, block))
            self.assertFalse(update_readme_estimate(path, block))
            text = path.read_text(encoding="utf-8")
            self.assertIn(ELO_MARK_START, text)
            self.assertIn("unmeasured", text)


if __name__ == "__main__":
    unittest.main()
