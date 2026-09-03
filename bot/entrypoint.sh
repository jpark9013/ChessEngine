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
export PYTHONPATH="/app:${PYTHONPATH:-}"
exec python3 /app/run.py --config "$CONFIG"
