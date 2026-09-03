"""Start lichess-bot after installing our matchmaking hooks."""

from __future__ import annotations

from lichess_hooks import install_hooks


def main() -> None:
    install_hooks()
    from lib.lichess_bot import start_program  # type: ignore[import-not-found]

    start_program()


if __name__ == "__main__":
    main()
