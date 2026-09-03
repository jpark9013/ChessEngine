"""Matchmaking policy tests. No live Lichess."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from matchmaking import (
    BUSY_COOLDOWN_SECONDS,
    DECLINE_COOLDOWN_SECONDS,
    OUTGOING_TC,
    CooldownStore,
    Opponent,
    cooldown_seconds_for,
    filter_candidates,
    in_window,
    opponent_weight,
    outgoing_challenge_params,
    rating_window,
    select_opponent,
)


class TestTimeControl(unittest.TestCase):
    def test_outgoing_is_1_plus_0_rated(self) -> None:
        self.assertEqual(OUTGOING_TC.initial, 60)
        self.assertEqual(OUTGOING_TC.increment, 0)
        self.assertTrue(OUTGOING_TC.is_bullet_1_plus_0())
        params = outgoing_challenge_params()
        self.assertEqual(params["initial"], 60)
        self.assertEqual(params["increment"], 0)
        self.assertEqual(params["days"], 0)
        self.assertEqual(params["variant"], "standard")
        self.assertEqual(params["mode"], "rated")


class TestRatingWindow(unittest.TestCase):
    def test_provisional_or_missing_is_wide(self) -> None:
        wide = rating_window(None)
        self.assertFalse(wide.established)
        self.assertTrue(in_window(800, wide))
        self.assertTrue(in_window(2400, wide))

        provisional = rating_window({"rating": 1500, "games": 3, "prov": True})
        self.assertFalse(provisional.established)
        self.assertLessEqual(provisional.lo, 800)

        few_games = rating_window({"rating": 1500, "games": 2})
        self.assertFalse(few_games.established)

    def test_established_prefers_equal_or_stronger(self) -> None:
        window = rating_window({"rating": 1500, "games": 20})
        self.assertTrue(window.established)
        self.assertEqual(window.lo, 1450)
        self.assertEqual(window.hi, 1700)
        self.assertTrue(in_window(1450, window))
        self.assertTrue(in_window(1500, window))
        self.assertTrue(in_window(1700, window))
        self.assertFalse(in_window(1299, window))
        self.assertFalse(in_window(1701, window))

        weaker = opponent_weight(1450, 1500, True)
        equal = opponent_weight(1500, 1500, True)
        stronger = opponent_weight(1700, 1500, True)
        self.assertGreater(equal, weaker)
        self.assertGreater(stronger, equal)

    def test_filter_skips_humans_self_and_cooldown(self) -> None:
        window = rating_window({"rating": 1500, "games": 20})
        def bullet(rating: int) -> dict:
            return {"bullet": {"rating": rating, "games": 10}}

        bots = [
            {"username": "MeBot", "title": "BOT", "perfs": bullet(1500)},
            {"username": "Human", "title": "", "perfs": bullet(1500)},
            {"username": "CoolBot", "title": "BOT", "perfs": bullet(1520)},
            {"username": "WeakBot", "title": "BOT", "perfs": bullet(1200)},
            {"username": "PeerBot", "title": "BOT", "perfs": bullet(1510)},
        ]
        cooling = {"coolbot"}
        picked = filter_candidates(
            bots,
            me="MeBot",
            window=window,
            cooling=lambda name: name.lower() in cooling,
        )
        names = {bot.username for bot in picked}
        self.assertEqual(names, {"PeerBot"})

    def test_select_never_returns_empty(self) -> None:
        self.assertIsNone(select_opponent([], our_rating=1500, established=True))
        only = Opponent("PeerBot", 1510)
        self.assertEqual(select_opponent([only], our_rating=1500, established=True), only)


class TestCooldownStore(unittest.TestCase):
    def test_persist_and_expiry(self) -> None:
        clock = {"now": 1_000.0}
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "challenge_cooldown.json"
            store = CooldownStore(path, now=lambda: clock["now"])
            store.record("FooBot", "generic")
            self.assertTrue(store.is_cooling("foobot"))
            self.assertTrue(path.is_file())

            reloaded = CooldownStore(path, now=lambda: clock["now"])
            self.assertTrue(reloaded.is_cooling("FooBot"))

            clock["now"] += DECLINE_COOLDOWN_SECONDS - 1
            self.assertTrue(reloaded.is_cooling("FooBot"))
            clock["now"] += 2
            self.assertFalse(reloaded.is_cooling("FooBot"))

    def test_busy_is_shorter_than_decline(self) -> None:
        self.assertEqual(cooldown_seconds_for("later"), BUSY_COOLDOWN_SECONDS)
        self.assertEqual(cooldown_seconds_for("generic"), DECLINE_COOLDOWN_SECONDS)
        self.assertLess(BUSY_COOLDOWN_SECONDS, DECLINE_COOLDOWN_SECONDS)

        clock = {"now": 50.0}
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "cd.json"
            store = CooldownStore(path, now=lambda: clock["now"])
            store.record("BusyBot", "later")
            clock["now"] += BUSY_COOLDOWN_SECONDS + 1
            self.assertFalse(store.is_cooling("BusyBot"))


if __name__ == "__main__":
    unittest.main()
