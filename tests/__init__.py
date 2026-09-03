"""Make `bot/` importable when unittest discovers tests."""

import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_BOT = _ROOT / "bot"
if str(_BOT) not in sys.path:
    sys.path.insert(0, str(_BOT))
