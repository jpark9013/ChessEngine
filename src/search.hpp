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
  int depth = 4;             // max ply; iterative deepening runs 1..depth
  double max_seconds = 0.0;  // hard abort; 0 = unlimited
  SearchMode mode = SearchMode::AlphaBetaQuiescence;
  double target_seconds = 0.0;  // soft stop; 0 = 70% of max
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
  static constexpr int kMaxPly = 64;

  int minimax(Board& board, int depth, int ply);
  int alphabeta(Board& board, int depth, int ply, int alpha, int beta, bool pv);
  int quiescence(Board& board, int ply, int alpha, int beta);
  int terminal_score(Board& board, int ply);
  bool time_up() const;
  bool soft_stop() const;
  bool timed_out();
  void order_moves(Board& board, MoveList& moves, const Move& hash, int ply) const;
  void score_moves(const Board& board, const MoveList& moves, int* scores, const Move& hash,
                   int ply) const;
  static void pick_next(MoveList& moves, int* scores, int idx);
  int move_score(const Board& board, const Move& m, const Move& hash, int ply) const;
  bool is_capture(const Board& board, const Move& m) const;
  void on_quiet_cutoff(int ply, const Move& m, int depth);

  SearchLimits limits_{};
  std::uint64_t nodes_ = 0;
  double deadline_ = 0.0;
  double soft_deadline_ = 0.0;
  bool use_deadline_ = false;
  bool use_soft_ = false;
  bool abort_ = false;
  Move killers_[kMaxPly][2]{};
  int history_[64][64]{};
  int eval_stack_[kMaxPly]{};
};

SearchResult search(Board& board, const SearchLimits& limits);

}  // namespace chess
