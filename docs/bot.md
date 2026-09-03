# Lichess bot

The worker is [lichess-bot](https://github.com/lichess-bot-devs/lichess-bot) plus our homemade engine. Game play still goes through lichess-bot. Outgoing matchmaking policy (who to challenge, 1+0, reject cooldown) lives in `bot/matchmaking.py` and is installed as a hook (`bot/lichess_hooks.py`) before `lichess-bot` starts.

The bot only accepts **bullet with base ≤ 120s** (1+0, 2+1, 1+1, 30+0, ½+0). Lichess does not allow bots to play ultraBullet. Blitz/rapid/classical are refused.

Search is **classical** (tapered HCE, no NNUE). Live clocks use a Stockfish-style two-layer allocator in `bot/engine.py`: subtract `move_overhead` (350ms), then `optimum ≈ usable / horizon` (horizon 20–40 remaining moves, shrinking with ply) plus ~0.65× increment. `maximum` is about 5.5–8× optimum, never more than 75% of remaining after overhead, and always leaves a flag buffer. A 1+0 opening move therefore thinks **~1.5s** (hard cap ~8s if the PV is unstable), not 35ms. Panic remaining (<0.4s) uses a tiny fixed think. C++ iterative deepening stops at the optimum when the PV is stable and spends toward the maximum only when the best move keeps changing. Depth is allowed up to 64; time is the real limit. Concurrency is 1 so one game owns the CPU.

The Stockfish **gauntlet / Elo CI job** uses the same live allocator against a **60+0** (1+0) game clock. Stockfish gets remaining `wtime`/`btime` so its own time manager runs; we still have the heuristic (~1.5s opening / ~8s hard on a full 60s clock). A side that hits 0 loses on time.

`bot/engine.py` is the contract: FEN in, UCI out. `bot/homemade.py` is the class lichess-bot loads (`engine.name: ChessEngine`).

## Matchmaking

Outgoing challenges are **rated 1+0 standard** against other Lichess **bots** only. Incoming still follows `bot/config.yml` (bullet, base ≤ 120s, bots and humans).

- **No / provisional bullet rating:** any eligible online bot (has a bullet rating and is not on cooldown).
- **Established rating** (non-provisional and at least 8 rated bullet games): opponents in `[our_rating - 50, our_rating + 200]`, sampled with a stronger-side skew. We do not challenge bots 200+ points below us.
- Our bullet rating is re-read from the Lichess profile about every five minutes (lichess-bot's `update_user_profile`).
- **Reject cooldown:** if a bot declines, times out, or is not open to challenges, we do not challenge them again for **7 days**. `"later"` / busy / opponent rate-limit uses a **2 hour** cooldown instead. Cooldowns persist in `bot/challenge_cooldown.json` (gitignored) so restarts remember.

Lichess limits bots to about **100 games/day** against other bots. Challenges expire in ~20 seconds; the worker waits at least a minute between outgoing challenges. The challenge API can also rate-limit us.

## Token

Put `LICHESS_TOKEN=` in `.env` (see `.env.example`). Never commit `.env`. The runner copies that value to `LICHESS_BOT_TOKEN`, which lichess-bot reads instead of writing the secret into `config.yml`.

## Local

1. Build the Python module (`./scripts/run_tests.sh` or CMake).
2. `uv sync --group dev` and put `LICHESS_TOKEN` in `.env`.
3. `uv run python scripts/run_lichess_bot.py`

That clones [lichess-bot](https://github.com/lichess-bot-devs/lichess-bot) into `.lichess-bot/` (gitignored) if needed, copies the homemade adapter, installs hooks, and starts one worker. A second invocation reuses the existing pid instead of opening another Lichess stream.

You can still run lichess-bot yourself: copy `bot/homemade.py` and `bot/engine.py` into a clone, keep `LICHESS_TOKEN_PLACEHOLDER` in `config.yml`, export `LICHESS_BOT_TOKEN`, and start via `bot/run.py` so the hooks load.

## Fly.io

Fly machines are **Linux x86_64**. Your Mac is not. `fly deploy` builds the image in Linux Docker from this repo — it does not ship the macOS `chessengine*.so` / `.dylib`. Develop and test here as usual (`./scripts/run_tests.sh`). The GitHub `python-test` job is the same Linux link that Fly uses.

Create the app once:

```bash
fly auth login
fly apps create little-complex-bot
fly secrets set LICHESS_TOKEN=lip_...
```

GitHub secrets: `LICHESS_TOKEN` is only needed on the machine; `FLY_API_TOKEN` is for Actions `fly deploy` on `master`.

`fly.toml` is a 2 GB `shared-cpu-1x` worker with no HTTP service. After the first deploy, keep it from sleeping:

```bash
fly machine update --autostop=off
fly scale count 1
```
