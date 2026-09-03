#pragma once

#include "types.hpp"

#include <cstdint>

namespace chess::zobrist {

void init();

std::uint64_t piece(Piece p, Square sq);
std::uint64_t side_to_move();
std::uint64_t castle(int rights);
std::uint64_t en_passant_file(int file);

}  // namespace chess::zobrist
