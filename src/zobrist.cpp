#include "zobrist.hpp"

#include <array>
#include <mutex>
#include <random>

namespace chess::zobrist {
namespace {

std::array<std::array<std::uint64_t, 64>, 12> piece_keys{};
std::uint64_t side_key = 0;
std::array<std::uint64_t, 16> castle_keys{};
std::array<std::uint64_t, 8> ep_keys{};
bool ready = false;
std::once_flag once;

void fill() {
  std::mt19937_64 rng(0xC0FFEEULL);
  for (int p = 0; p < 12; ++p) {
    for (int s = 0; s < 64; ++s) {
      piece_keys[p][s] = rng();
    }
  }
  side_key = rng();
  for (int i = 0; i < 16; ++i) castle_keys[i] = rng();
  for (int i = 0; i < 8; ++i) ep_keys[i] = rng();
  ready = true;
}

}  // namespace

void init() { std::call_once(once, fill); }

std::uint64_t piece(Piece p, Square sq) {
  if (p == Piece::None || !sq.valid()) return 0;
  return piece_keys[static_cast<int>(p) - 1][sq.index()];
}

std::uint64_t side_to_move() { return side_key; }

std::uint64_t castle(int rights) { return castle_keys[rights & 15]; }

std::uint64_t en_passant_file(int file) { return ep_keys[file & 7]; }

}  // namespace chess::zobrist
