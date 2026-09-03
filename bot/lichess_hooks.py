"""Patch lichess-bot matchmaking to use our 1+0 / cooldown policy."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

from matchmaking import (
    DEFAULT_COOLDOWN_PATH,
    OUTGOING_INCREMENT_SECONDS,
    OUTGOING_INITIAL_SECONDS,
    OUTGOING_MODE,
    OUTGOING_VARIANT,
    CooldownStore,
    filter_candidates,
    outgoing_challenge_params,
    rating_window,
    select_opponent,
)

logger = logging.getLogger(__name__)


def _opponent_from_event(event: dict[str, Any], our_username: str) -> tuple[str | None, bool]:
    challenge = event.get("challenge") or {}
    challenger = (challenge.get("challenger") or {}).get("name") or ""
    dest = (challenge.get("destUser") or {}).get("name") or ""
    ours = our_username.strip().lower()
    from_self = challenger.strip().lower() == ours
    opponent = dest if from_self else challenger
    return (opponent or None, from_self)


def install_hooks(store: CooldownStore | None = None, matchmaking_mod: Any = None) -> CooldownStore:
    """Replace opponent choice and decline handling on lichess-bot's Matchmaking."""
    if matchmaking_mod is None:
        from lib import matchmaking as matchmaking_mod  # type: ignore[import-not-found]

    cooldown = store or CooldownStore(Path(DEFAULT_COOLDOWN_PATH))
    Matchmaking = matchmaking_mod.Matchmaking
    original_should = Matchmaking.should_create_challenge
    original_handle_error = Matchmaking.handle_challenge_error_response
    empty = outgoing_challenge_params()

    def no_opponent() -> tuple[str | None, int, int, int, str, str]:
        return (
            None,
            int(empty["initial"]),
            int(empty["increment"]),
            0,
            str(empty["variant"]),
            str(empty["mode"]),
        )

    def choose_opponent(self: Any) -> tuple[str | None, int, int, int, str, str]:
        self.update_user_profile()
        perf = self.perf().get("bullet", {}) or {}
        window = rating_window(perf)
        our_rating = int(perf.get("rating") or 0)
        logger.info(
            "Seeking 1+0 rated standard; bullet rating %s%s window [%s, %s]",
            our_rating or "none",
            " (provisional)" if not window.established else "",
            window.lo,
            window.hi,
        )
        try:
            online = self.li.get_online_bots()
        except Exception:
            logger.exception("Could not list online bots")
            return no_opponent()

        candidates = filter_candidates(
            online,
            me=self.username(),
            window=window,
            cooling=cooldown.is_cooling,
        )
        pick = select_opponent(
            candidates,
            our_rating=our_rating,
            established=window.established,
        )
        if pick is None:
            logger.info(
                "No eligible 1+0 bots in-window and off cooldown (%s online).",
                len(online),
            )
            self._mm_last_opponent = None
            return no_opponent()

        self._mm_last_opponent = pick.username
        logger.info(
            "Will challenge %s (bullet %s) for 1+0 rated.",
            pick.username,
            pick.rating,
        )
        return (
            pick.username,
            OUTGOING_INITIAL_SECONDS,
            OUTGOING_INCREMENT_SECONDS,
            0,
            OUTGOING_VARIANT,
            OUTGOING_MODE,
        )

    def declined_challenge(self: Any, event: dict[str, Any]) -> None:
        opponent, from_self = _opponent_from_event(event, self.username())
        challenge = event.get("challenge") or {}
        reason_key = challenge.get("declineReasonKey") or challenge.get("declineReason")
        reason = str(reason_key or "generic")
        logger.info("%s declined our challenge: %s", opponent or "opponent", reason)
        challenge_id = challenge.get("id") or ""
        if challenge_id:
            self.discard_challenge(challenge_id)
        if from_self and opponent:
            cooldown.record(opponent, reason)
            logger.info("Cooldown for %s after decline (%s).", opponent, reason)
        self.show_earliest_challenge_time()

    def handle_challenge_error_response(self: Any, response: dict[str, Any], username: str) -> None:
        if response.get("bot_is_rate_limited"):
            original_handle_error(self, response, username)
            return
        if response.get("opponent_is_rate_limited"):
            cooldown.record(username, "busy")
            logger.info("Cooldown for %s (busy / opponent rate limit).", username)
            original_handle_error(self, response, username)
            return
        cooldown.record(username, "not_open")
        logger.info("Cooldown for %s (not open to challenges / rejected).", username)
        original_handle_error(self, response, username)

    def should_create_challenge(self: Any) -> bool:
        expired = self.last_challenge_created_delay.is_expired() and self.challenge_id
        if expired:
            opponent = getattr(self, "_mm_last_opponent", None)
            if opponent:
                cooldown.record(opponent, "timeout")
                logger.info("Cooldown for %s (challenge timed out).", opponent)
                self._mm_last_opponent = None
        return bool(original_should(self))

    Matchmaking.choose_opponent = choose_opponent
    Matchmaking.declined_challenge = declined_challenge
    Matchmaking.handle_challenge_error_response = handle_challenge_error_response
    Matchmaking.should_create_challenge = should_create_challenge
    logger.info(
        "Installed 1+0 matchmaking hooks (decline cooldown %ss, store %s).",
        7 * 24 * 60 * 60,
        cooldown.path,
    )
    return cooldown
