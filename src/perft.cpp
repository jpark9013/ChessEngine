#include "perft.hpp"

#include <iomanip>
#include <iostream>

namespace chess {

std::uint64_t perft(Board& board, int depth) {
  if (depth <= 0) return 1;
  MoveList moves = board.legal_moves();
  if (depth == 1) return static_cast<std::uint64_t>(moves.size());
  std::uint64_t nodes = 0;
  for (const Move& m : moves) {
    board.make(m);
    nodes += perft(board, depth - 1);
    board.unmake();
  }
  return nodes;
}

std::uint64_t perft_divide(Board& board, int depth, std::ostream* out) {
  MoveList moves = board.legal_moves();
  std::uint64_t total = 0;
  for (const Move& m : moves) {
    board.make(m);
    std::uint64_t n = (depth <= 1) ? 1 : perft(board, depth - 1);
    board.unmake();
    total += n;
    if (out) *out << m.uci() << ": " << n << "\n";
  }
  if (out) *out << "total: " << total << "\n";
  return total;
}

}  // namespace chess
