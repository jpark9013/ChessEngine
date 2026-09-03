#pragma once

#include <bit>
#include <cstdint>

namespace chess {

using Bitboard = std::uint64_t;

constexpr Bitboard kFileA = 0x0101010101010101ULL;
constexpr Bitboard kFileH = 0x8080808080808080ULL;
constexpr Bitboard kRank1 = 0x00000000000000FFULL;
constexpr Bitboard kRank2 = 0x000000000000FF00ULL;
constexpr Bitboard kRank3 = 0x0000000000FF0000ULL;
constexpr Bitboard kRank6 = 0x0000FF0000000000ULL;
constexpr Bitboard kRank7 = 0x00FF000000000000ULL;
constexpr Bitboard kRank8 = 0xFF00000000000000ULL;

inline constexpr Bitboard bit(int sq) { return 1ULL << sq; }

inline int lsb(Bitboard b) { return std::countr_zero(b); }
inline int msb(Bitboard b) { return 63 - std::countl_zero(b); }
inline int popcount(Bitboard b) { return std::popcount(b); }

inline int pop_lsb(Bitboard& b) {
  const int s = lsb(b);
  b &= b - 1;
  return s;
}

inline constexpr Bitboard shift_n(Bitboard b) { return b << 8; }
inline constexpr Bitboard shift_s(Bitboard b) { return b >> 8; }
inline constexpr Bitboard shift_e(Bitboard b) { return (b & ~kFileH) << 1; }
inline constexpr Bitboard shift_w(Bitboard b) { return (b & ~kFileA) >> 1; }
inline constexpr Bitboard shift_ne(Bitboard b) { return (b & ~kFileH) << 9; }
inline constexpr Bitboard shift_nw(Bitboard b) { return (b & ~kFileA) << 7; }
inline constexpr Bitboard shift_se(Bitboard b) { return (b & ~kFileH) >> 7; }
inline constexpr Bitboard shift_sw(Bitboard b) { return (b & ~kFileA) >> 9; }

}  // namespace chess
