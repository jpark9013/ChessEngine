# Architecture

The library is a hybrid bitboard engine: 12 piece bitboards plus occupancy for generation and attacks, with an 8×8 mailbox kept in sync for `piece_at`, FEN, and eval. Headers live next to their `.cpp` files under `src/`.

## Modules

| File | Role |
|---|---|
| `src/types.hpp` | `Color`, `Piece`, `Square`, `Move`, `MoveList`, game result enums |
| `src/bitboard.hpp` | `Bitboard` helpers (`pop_lsb`, file/rank masks, shifts) |
| `src/attacks.cpp` | Pawn/knight/king tables; occupancy-masked slider rays |
| `src/zobrist.cpp` | Deterministic 64-bit keys for pieces, side, castling, en passant file |
| `src/board.cpp` | Position, FEN, attacks, move gen, make/unmake, SAN/UCI, eval, game status |
| `src/search.cpp` | Iterative deepening, TT, null-move, LMR, PVS, killers/history |
| `src/perft.cpp` | Recursive legal-move node counts |
| `src/python_module.cpp` | pybind11 module `chessengine` |

`Board` is the only mutable object that matters. Search, perft, and notation all go through `make` / `unmake` / `legal_moves`.

## Coordinates

Squares are `rank * 8 + file` with `a1 = 0` and `h8 = 63`.

- Rank 0 is White’s first rank (`1` in algebraic).
- File 0 is the a-file.
- White pawns increase rank; Black pawns decrease rank.
- FEN’s first rank token is rank 8 (`a8`–`h8`), loaded onto index 56–63.

`Move` is an explicit struct: from, to, promotion piece, and a flag (`Normal`, `DoublePawn`, `EnPassant`, `CastleKingside`, `CastleQueenside`, `Promotion`). Flags are not packed into the square indexes.

## Make / unmake

`Board::make` pushes an `Undo` record with captured piece, castling rights, en passant square, clocks, hash, king squares, and whose turn it was. `unmake` restores that snapshot and reverses the piece movement, including rook placement for castling and the captured pawn for en passant.

Search and perft **always** unmake, including after a beta cutoff. That was a real bug in the previous engine.

## Legal moves

1. Generate pseudo-legal moves (piece geometry, including specials).
2. Make each move, reject it if the moving side’s king is attacked, unmake.

Attacks include **pawns**. Castling extra rules are applied in generation:

- The relevant right must still be set, and the rook/king must still be on their original squares.
- Intermediate squares empty (queenside includes the b-file).
- The king is not in check, and does not pass through or land on an attacked square.

En passant is generated when `ep_square` is set. The legal-move filter drops EP captures that would leave the king in check (the classic rank pin).

Promotions emit four moves (Q, R, B, N), including capturing promotions.

## Game end

`Board::status()` checks, in order:

1. Threefold repetition (Zobrist history)
2. Fifty-move rule (`halfmove_clock >= 100`)
3. Insufficient material (K vs K, K+N vs K, K+B vs K, same-colored bishops only)
4. No legal moves: checkmate if in check, otherwise stalemate

## Evaluation

White-centric centipawns: material + piece-square tables from the [simplified evaluation function](https://www.chessprogramming.org/Simplified_Evaluation_Function). King tables switch to the endgame table when there are no queens. `put` / `remove` keep this score incrementally so search does not walk the board at every leaf. `evaluate()` returns that score from the side to move (so negamax can negate).

## Search

Root search is **iterative deepening**: it completes depth 1, then 2, … up to `limits.depth`. The last fully finished iteration is the result. A single legal move is returned without searching.

Each iteration tries every legal move with PVS, then calls:

- **Minimax** — full tree, no pruning (debug / CLI)
- **Alpha-beta** — fail-soft negamax with a transposition table, hash-move / MVV-LVA / killer / history ordering, null-move pruning, LMR, reverse futility, and quiet futility
- **Alpha-beta + quiescence** — at depth 0, stand pat and search captures (all moves if in check), capped at ply 18

Interior nodes generate **captures first**, then quiets only if there is no cutoff. Moves are tried with make/unmake; those that leave the king in check are skipped.

`max_seconds` is a hard abort (checked every 256 nodes). `target_seconds` is a soft stop: do not start the next iteration if the target is gone or the last iteration would not fit (~1.5×). If `target_seconds` is 0 and a hard cap is set, the soft bound is 70% of the hard cap.

Mate is `100000 - ply` so shorter mates score higher. Draw by 50-move or repetition returns 0 inside the tree. The search always unmakes before returning.

The transposition table is process-lifetime (~1M entries). Killers and history reset at the start of each root search.

## Hash

Zobrist keys are filled from `mt19937_64` with a fixed seed, so hashes are reproducible across runs. The hash includes pieces, side to move, all 16 castling-right combinations, and en passant file.

## Bitboards

`put` / `remove` update both the mailbox and the piece/color bitboards. `is_attacked` and `generate_pseudo` bitscan attack sets. Sliders use occupancy-masked rays (not magic bitboards). The public `Move` / `Board` API is unchanged.
