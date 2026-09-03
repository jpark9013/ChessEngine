#include "attacks.hpp"

#include <array>
#include <mutex>

namespace chess::attacks {
namespace {

constexpr int kDr[8] = {1, -1, 0, 0, 1, 1, -1, -1};
constexpr int kDf[8] = {0, 0, 1, -1, 1, -1, 1, -1};
constexpr int kKnightR[8] = {2, 2, -2, -2, 1, 1, -1, -1};
constexpr int kKnightF[8] = {1, -1, 1, -1, 2, -2, 2, -2};
constexpr int kKingR[8] = {1, 1, 1, 0, 0, -1, -1, -1};
constexpr int kKingF[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

alignas(64) std::array<std::array<Bitboard, 64>, 8> rays{};
std::once_flag once;

bool positive_dir(int d) { return kDr[d] > 0 || (kDr[d] == 0 && kDf[d] > 0); }

void fill() {
  for (int sq = 0; sq < 64; ++sq) {
    const int r = sq >> 3;
    const int f = sq & 7;

    for (int d = 0; d < 8; ++d) {
      Bitboard bb = 0;
      int nr = r + kDr[d];
      int nf = f + kDf[d];
      while (on_board(nr, nf)) {
        bb |= bit(nr * 8 + nf);
        nr += kDr[d];
        nf += kDf[d];
      }
      rays[d][sq] = bb;
    }

    Bitboard kn = 0;
    for (int i = 0; i < 8; ++i) {
      const int nr = r + kKnightR[i];
      const int nf = f + kKnightF[i];
      if (on_board(nr, nf)) kn |= bit(nr * 8 + nf);
    }
    knight_att[sq] = kn;

    Bitboard kg = 0;
    for (int i = 0; i < 8; ++i) {
      const int nr = r + kKingR[i];
      const int nf = f + kKingF[i];
      if (on_board(nr, nf)) kg |= bit(nr * 8 + nf);
    }
    king_att[sq] = kg;

    const Bitboard b = bit(sq);
    pawn_att[0][sq] = shift_ne(b) | shift_nw(b);
    pawn_att[1][sq] = shift_se(b) | shift_sw(b);
  }
}

Bitboard ray_attacks(int sq, Bitboard occ, int d) {
  Bitboard attacks = rays[d][sq];
  const Bitboard blockers = attacks & occ;
  if (!blockers) return attacks;
  const int blk = positive_dir(d) ? lsb(blockers) : msb(blockers);
  return attacks ^ rays[d][blk];
}

}  // namespace

alignas(64) Bitboard pawn_att[2][64]{};
alignas(64) Bitboard knight_att[64]{};
alignas(64) Bitboard king_att[64]{};

void init() { std::call_once(once, fill); }

Bitboard bishop(int sq, Bitboard occ) {
  return ray_attacks(sq, occ, 4) | ray_attacks(sq, occ, 5) | ray_attacks(sq, occ, 6) |
         ray_attacks(sq, occ, 7);
}

Bitboard rook(int sq, Bitboard occ) {
  return ray_attacks(sq, occ, 0) | ray_attacks(sq, occ, 1) | ray_attacks(sq, occ, 2) |
         ray_attacks(sq, occ, 3);
}

Bitboard queen(int sq, Bitboard occ) { return bishop(sq, occ) | rook(sq, occ); }

}  // namespace chess::attacks
