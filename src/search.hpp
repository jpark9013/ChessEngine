#pragma once

#include "board.hpp"
#include "types.hpp"

namespace chess {

enum class SearchMode {
  Minimax = 1,
  AlphaBeta = 2,
  AlphaBetaQuiescence = 3
};

struct SearchLimits {
  int depth = 4;
  double max_seconds = 0.0;  // 0 = unlimited
  SearchMode mode = SearchMode::AlphaBetaQuiescence;
};

struct SearchResult {
  Move best_move{};
  int score = 0;
  int depth = 0;
  std::uint64_t nodes = 0;
  double seconds = 0.0;
};

class Searcher {
 public:
  SearchResult search(Board& board, const SearchLimits& limits);

  std::uint64_t nodes() const { return nodes_; }

 private:
  int minimax(Board& board, int depth, int ply);
  int alphabeta(Board& board, int depth, int ply, int alpha, int beta);
  int quiescence(Board& board, int ply, int alpha, int beta);
  int terminal_score(Board& board, int ply);
  bool time_up() const;
  void order_moves(Board& board, MoveList& moves) const;

  SearchLimits limits_{};
  std::uint64_t nodes_ = 0;
  double deadline_ = 0.0;
  bool use_deadline_ = false;
};

SearchResult search(Board& board, const SearchLimits& limits);

}  // namespace chess
