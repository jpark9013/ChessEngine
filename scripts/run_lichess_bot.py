#!/usr/bin/env python3
"""Clone lichess-bot if needed, load .env, install hooks, and start the worker."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LICHESS_BOT_DIR = Path(os.environ.get("LICHESS_BOT_DIR", ROOT / ".lichess-bot"))
PID_PATH = LICHESS_BOT_DIR / "lichess-bot.pid"
ADAPTER_FILES = ("homemade.py", "engine.py", "matchmaking.py", "lichess_hooks.py", "run.py")


def load_dotenv(path: Path) -> None:
    if not path.is_file():
        return
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        key = key.strip()
        value = value.strip().strip("'\"")
        if key and key not in os.environ:
            os.environ[key] = value


def existing_bot_pid() -> int | None:
    if PID_PATH.is_file():
        try:
            pid = int(PID_PATH.read_text(encoding="utf-8").strip())
        except ValueError:
            pid = 0
        if pid > 0 and _pid_alive(pid):
            return pid
    return None


def _pid_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    return True


def ensure_lichess_bot(dest: Path) -> None:
    marker = dest / "lichess-bot.py"
    if marker.is_file():
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.exists():
        shutil.rmtree(dest)
    repo = "https://github.com/lichess-bot-devs/lichess-bot.git"
    subprocess.run(["git", "clone", "--depth", "1", repo, str(dest)], check=True)


def copy_adapter(dest: Path) -> None:
    bot_dir = ROOT / "bot"
    for name in ADAPTER_FILES:
        shutil.copy2(bot_dir / name, dest / name)


def ensure_deps(dest: Path) -> None:
    requirements = dest / "requirements.txt"
    if not requirements.is_file():
        return
    if shutil.which("uv"):
        subprocess.run(["uv", "pip", "install", "-r", str(requirements)], check=True)
        return
    subprocess.run([sys.executable, "-m", "pip", "install", "-r", str(requirements)], check=True)


def wire_token() -> None:
    token = os.environ.get("LICHESS_TOKEN") or os.environ.get("LICHESS_BOT_TOKEN")
    if not token:
        print("LICHESS_TOKEN is required (set it in .env or the environment).", file=sys.stderr)
        sys.exit(1)
    os.environ["LICHESS_BOT_TOKEN"] = token
    os.environ.pop("LICHESS_TOKEN", None)


def main() -> None:
    load_dotenv(ROOT / ".env")
    wire_token()

    running = existing_bot_pid()
    if running is not None:
        print(f"lichess-bot already running as pid {running}; not starting a second instance.")
        return

    ensure_lichess_bot(LICHESS_BOT_DIR)
    copy_adapter(LICHESS_BOT_DIR)
    ensure_deps(LICHESS_BOT_DIR)

    build = ROOT / "build"
    sys.path.insert(0, str(LICHESS_BOT_DIR))
    sys.path.insert(0, str(ROOT / "bot"))
    if build.is_dir():
        sys.path.insert(0, str(build))

    os.environ.setdefault("PYTHONPATH", "")
    os.environ["PYTHONPATH"] = os.pathsep.join(
        [str(build), str(ROOT / "bot"), str(LICHESS_BOT_DIR), os.environ["PYTHONPATH"]]
    )

    os.chdir(LICHESS_BOT_DIR)
    PID_PATH.write_text(f"{os.getpid()}\n", encoding="utf-8")
    config = ROOT / "bot" / "config.yml"
    log_file = LICHESS_BOT_DIR / "lichess-bot.log"
    sys.argv = [
        "lichess-bot",
        "--config",
        str(config),
        "-l",
        str(log_file),
        "--disable_auto_logging",
    ]
    from run import main as run_main

    run_main()


if __name__ == "__main__":
    main()
