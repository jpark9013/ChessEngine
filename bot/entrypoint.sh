#!/bin/sh
set -eu

if [ -z "${LICHESS_TOKEN:-}" ] && [ -z "${LICHESS_BOT_TOKEN:-}" ]; then
  echo "LICHESS_TOKEN is required" >&2
  exit 1
fi

# lichess-bot reads LICHESS_BOT_TOKEN and never needs the raw secret in config.yml.
export LICHESS_BOT_TOKEN="${LICHESS_BOT_TOKEN:-$LICHESS_TOKEN}"

CONFIG="${LICHESS_BOT_CONFIG:-/app/lichess-bot/config.yml}"
cd /app/lichess-bot
# /app has chessengine*.so and our adapter; /app/lichess-bot has upstream lib/.
# Running /app/run.py by absolute path puts only /app on sys.path[0], so lib
# must be on PYTHONPATH (local scripts/run_lichess_bot.py does the same).
export PYTHONPATH="/app/lichess-bot:/app:${PYTHONPATH:-}"
exec python3 run.py --config "$CONFIG"
