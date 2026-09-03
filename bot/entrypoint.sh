#!/bin/sh
set -eu

if [ -z "${LICHESS_TOKEN:-}" ]; then
  echo "LICHESS_TOKEN is required" >&2
  exit 1
fi

CONFIG="${LICHESS_BOT_CONFIG:-/app/lichess-bot/config.yml}"
# Replace the placeholder without treating the token as a sed delimiter.
python3 - "$CONFIG" <<'PY'
import os
import sys
from pathlib import Path

path = Path(sys.argv[1])
text = path.read_text()
path.write_text(text.replace("LICHESS_TOKEN_PLACEHOLDER", os.environ["LICHESS_TOKEN"]))
PY

cd /app/lichess-bot
exec python3 lichess-bot.py --config "$CONFIG"
