# Lichess bot

The worker is [lichess-bot](https://github.com/lichess-bot-devs/lichess-bot) plus our homemade engine. We do not call berserk or the Lichess HTTP API ourselves.

The bot only accepts **bullet with base ≤ 120s** (1+0, 2+1, 1+1, 30+0, ½+0). Lichess does not allow bots to play ultraBullet. Blitz/rapid/classical are refused. Search is iterative deepening with a bullet clock: a soft target (~clock/30, capped at 2.5s) and a hard abort (≤ 4s, much less when flagging). Concurrency is 1 so one game owns the CPU. `move_overhead` is 350ms.

`bot/engine.py` is the contract: FEN in, UCI out. `bot/homemade.py` is the class lichess-bot loads (`engine.name: ChessEngine`).

## Local

1. Build the Python module (`./scripts/run_tests.sh` or CMake).
2. Clone lichess-bot next to this repo (or anywhere).
3. Copy `bot/homemade.py` and `bot/engine.py` into that clone.
4. Copy `bot/config.yml`, replace `LICHESS_TOKEN_PLACEHOLDER` with the token from `.env` (never commit it).
5. `PYTHONPATH=/path/to/ChessEngine/build python lichess-bot.py`

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
