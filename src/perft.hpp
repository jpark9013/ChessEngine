#pragma once

#include "board.hpp"

#include <cstdint>
#include <iosfwd>

namespace chess {

std::uint64_t perft(Board& board, int depth);
std::uint64_t perft_divide(Board& board, int depth, std::ostream* out = nullptr);

}  // namespace chess
