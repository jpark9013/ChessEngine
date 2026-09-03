"""Outgoing Lichess matchmaking policy used by the lichess-bot hook.

lichess-bot still plays games and creates challenges. This module decides
*whom* to challenge (1+0 rated, prefer equal/stronger) and how long to wait
after a decline/timeout before trying the same bot again.
"""

from __future__ import annotations

import json
import random
import time
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

OUTGOING_INITIAL_SECONDS = 60
OUTGOING_INCREMENT_SECONDS = 0
OUTGOING_VARIANT = "standard"
OUTGOING_MODE = "rated"

DECLINE_COOLDOWN_SECONDS = 7 * 24 * 60 * 60
BUSY_COOLDOWN_SECONDS = 2 * 60 * 60

RATING_BELOW = 50
RATING_ABOVE = 200
PROVISIONAL_MIN_GAMES = 8
WIDE_MIN_RATING = 600
WIDE_MAX_RATING = 4000

DEFAULT_COOLDOWN_PATH = Path(__file__).resolve().parent / "challenge_cooldown.json"

BUSY_REASONS = frozenset({"later", "busy", "opponent_rate_limited"})


@dataclass(frozen=True)
class TimeControl:
    initial: int
    increment: int

    def is_bullet_1_plus_0(self) -> bool:
        return (
            self.initial == OUTGOING_INITIAL_SECONDS
            and self.increment == OUTGOING_INCREMENT_SECONDS
        )


OUTGOING_TC = TimeControl(OUTGOING_INITIAL_SECONDS, OUTGOING_INCREMENT_SECONDS)


@dataclass(frozen=True)
class RatingWindow:
    lo: int
    hi: int
    established: bool


@dataclass(frozen=True)
class Opponent:
    username: str
    rating: int
    games: int = 0


def outgoing_challenge_params() -> dict[str, int | str]:
    """Clock and mode for every outgoing challenge."""
    return {
        "initial": OUTGOING_TC.initial,
        "increment": OUTGOING_TC.increment,
        "days": 0,
        "variant": OUTGOING_VARIANT,
        "mode": OUTGOING_MODE,
    }


def rating_is_established(perf: Mapping[str, Any] | None) -> bool:
    if not perf:
        return False
    if perf.get("prov"):
        return False
    rating = int(perf.get("rating") or 0)
    games = int(perf.get("games") or 0)
    return rating > 0 and games >= PROVISIONAL_MIN_GAMES


def rating_window(perf: Mapping[str, Any] | None) -> RatingWindow:
    """Wide hunt while provisional; then [our - 50, our + 200]."""
    perf = perf or {}
    rating = int(perf.get("rating") or 0)
    if not rating_is_established(perf):
        return RatingWindow(WIDE_MIN_RATING, WIDE_MAX_RATING, False)
    return RatingWindow(rating - RATING_BELOW, rating + RATING_ABOVE, True)


def in_window(opp_rating: int, window: RatingWindow) -> bool:
    return window.lo <= int(opp_rating) <= window.hi


def opponent_weight(opp_rating: int, our_rating: int, established: bool) -> float:
    """Equal/stronger bots are more likely once our rating is established."""
    if not established:
        return 1.0
    return max(0.05, 1.0 + (int(opp_rating) - int(our_rating)) / 100.0)


def cooldown_seconds_for(reason: str) -> int:
    key = (reason or "generic").strip().lower()
    if key in BUSY_REASONS:
        return BUSY_COOLDOWN_SECONDS
    return DECLINE_COOLDOWN_SECONDS


def _norm_name(username: str) -> str:
    return username.strip().lower()


def filter_candidates(
    bots: Sequence[Mapping[str, Any]],
    *,
    me: str,
    window: RatingWindow,
    cooling: Callable[[str], bool],
    speed: str = "bullet",
) -> list[Opponent]:
    """Keep online bots that play this speed, are in-window, and not on cooldown."""
    ours = _norm_name(me)
    chosen: list[Opponent] = []
    for bot in bots:
        name = str(bot.get("username") or "")
        if not name or _norm_name(name) == ours:
            continue
        if cooling(name):
            continue
        title = str(bot.get("title") or "").upper()
        if title != "BOT":
            continue
        perf = (bot.get("perfs") or {}).get(speed) or {}
        games = int(perf.get("games") or 0)
        rating = int(perf.get("rating") or 0)
        if games <= 0 or rating <= 0:
            continue
        if not in_window(rating, window):
            continue
        chosen.append(Opponent(name, rating, games))
    return chosen


def select_opponent(
    candidates: Sequence[Opponent],
    *,
    our_rating: int,
    established: bool,
    rng: random.Random | None = None,
) -> Opponent | None:
    if not candidates:
        return None
    picker = rng or random.Random()
    weights = [opponent_weight(bot.rating, our_rating, established) for bot in candidates]
    return picker.choices(list(candidates), weights=weights, k=1)[0]


class CooldownStore:
    """Persisted per-opponent challenge cooldown (survives restarts)."""

    def __init__(
        self,
        path: Path | None = None,
        now: Callable[[], float] | None = None,
    ) -> None:
        self.path = Path(path) if path is not None else DEFAULT_COOLDOWN_PATH
        self._now = now or time.time
        self._entries: dict[str, dict[str, Any]] = {}
        self.load()

    def load(self) -> None:
        self._entries = {}
        if not self.path.is_file():
            return
        try:
            raw = json.loads(self.path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            return
        opponents = raw.get("opponents", raw) if isinstance(raw, dict) else {}
        if not isinstance(opponents, dict):
            return
        for name, payload in opponents.items():
            if isinstance(payload, dict) and "until" in payload:
                self._entries[_norm_name(str(name))] = payload

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        now = self._now()
        live = {
            name: data
            for name, data in self._entries.items()
            if float(data.get("until", 0)) > now
        }
        self._entries = live
        payload = {"opponents": live}
        self.path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")

    def is_cooling(self, username: str) -> bool:
        key = _norm_name(username)
        data = self._entries.get(key)
        if not data:
            return False
        if float(data.get("until", 0)) <= self._now():
            del self._entries[key]
            return False
        return True

    def record(self, username: str, reason: str, seconds: int | None = None) -> float:
        key = _norm_name(username)
        duration = cooldown_seconds_for(reason) if seconds is None else int(seconds)
        until = self._now() + duration
        self._entries[key] = {
            "until": until,
            "reason": (reason or "generic").strip().lower(),
            "recorded_at": self._now(),
        }
        self.save()
        return until
