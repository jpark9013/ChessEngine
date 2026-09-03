#pragma once

#include "bitboard.hpp"
#include "types.hpp"

namespace chess::attacks {

void init();

extern Bitboard pawn_att[2][64];
extern Bitboard knight_att[64];
extern Bitboard king_att[64];

inline Bitboard pawn(Color c, int sq) { return pawn_att[static_cast<int>(c)][sq]; }
inline Bitboard knight(int sq) { return knight_att[sq]; }
inline Bitboard king(int sq) { return king_att[sq]; }

Bitboard bishop(int sq, Bitboard occ);
Bitboard rook(int sq, Bitboard occ);
Bitboard queen(int sq, Bitboard occ);

}  // namespace chess::attacks
