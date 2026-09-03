#pragma once

#include "bitboard.hpp"
#include "types.hpp"

namespace chess::attacks {

void init();

Bitboard pawn(Color c, int sq);
Bitboard knight(int sq);
Bitboard king(int sq);
Bitboard bishop(int sq, Bitboard occ);
Bitboard rook(int sq, Bitboard occ);
Bitboard queen(int sq, Bitboard occ);

}  // namespace chess::attacks
