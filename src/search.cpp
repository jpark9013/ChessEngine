#include "search.hpp"

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <utility>
#include <vector>

namespace chess {
namespace {

int piece_value(PieceType t) {
  switch (t) {
    case PieceType::Pawn: return 100;
    case PieceType::Knight: return 320;
    case PieceType::Bishop: return 330;
    case PieceType::Rook: return 500;
    case PieceType::Queen: return 900;
    default: return 0;
  }
}

double now_seconds() {
  using clock = std::chrono::steady_clock;
  return std::chrono::duration<double>(clock::now().time_since_epoch()).count();
}

enum class TTFlag : std::uint8_t { Exact, Lower, Upper };

struct TTEntry {
  std::uint64_t key = 0;
  Move move{};
  int score = 0;
  std::int8_t depth = -1;
  TTFlag flag = TTFlag::Exact;
};

constexpr int kTTSize = 1 << 20;

int to_tt(int s, int ply) {
  if (s >= kMateScore - 512) return s + ply;
  if (s <= -kMateScore + 512) return s - ply;
  return s;
}

int from_tt(int s, int ply) {
  if (s >= kMateScore - 512) return s - ply;
  if (s <= -kMateScore + 512) return s + ply;
  return s;
}

struct TranspositionTable {
  std::vector<TTEntry> t;
  TranspositionTable() : t(kTTSize) {}

  TTEntry& slot(std::uint64_t k) { return t[k & (kTTSize - 1)]; }
  const TTEntry& slot(std::uint64_t k) const { return t[k & (kTTSize - 1)]; }

  Move probe_move(std::uint64_t k) const {
    const TTEntry& e = slot(k);
    return e.key == k ? e.move : Move{};
  }

  bool cutoff(std::uint64_t k, int depth, int ply, int alpha, int beta, int& score) const {
    const TTEntry& e = slot(k);
    if (e.key != k || e.depth < depth) return false;
    const int s = from_tt(e.score, ply);
    if (e.flag == TTFlag::Exact) {
      score = s;
      return true;
    }
    if (e.flag == TTFlag::Lower && s >= beta) {
      score = s;
      return true;
    }
    if (e.flag == TTFlag::Upper && s <= alpha) {
      score = s;
      return true;
    }
    return false;
  }

  void store(std::uint64_t k, int depth, int ply, int score, TTFlag flag, Move m) {
    TTEntry& e = slot(k);
    if (e.key == k && e.depth > depth) return;
    e.key = k;
    e.depth = static_cast<std::int8_t>(std::clamp(depth, 0, 127));
    e.score = to_tt(score, ply);
    e.flag = flag;
    e.move = m;
  }
};

TranspositionTable& tt() {
  static TranspositionTable table;
  return table;
}

}  // namespace

bool Searcher::is_capture(const Board& board, const Move& m) const {
  if (m.flag == MoveFlag::EnPassant) return true;
  return board.piece_at(m.to) != Piece::None;
}

int Searcher::move_score(const Board& board, const Move& m, const Move& hash, int ply) const {
  if (!hash.is_null() && m == hash) return 1'000'000;
  if (m.flag == MoveFlag::Promotion) return 800'000 + piece_value(m.promotion);
  if (is_capture(board, m)) {
    Piece captured = m.flag == MoveFlag::EnPassant
                         ? make_piece(opposite(board.side_to_move()), PieceType::Pawn)
                         : board.piece_at(m.to);
    Piece attacker = board.piece_at(m.from);
    return 100'000 + 10 * piece_value(type_of(captured)) - piece_value(type_of(attacker));
  }
  if (ply >= 0 && ply < kMaxPly) {
    if (m == killers_[ply][0]) return 90'000;
    if (m == killers_[ply][1]) return 80'000;
  }
  if (m.flag == MoveFlag::CastleKingside || m.flag == MoveFlag::CastleQueenside) return 50;
  return history_[m.from.index()][m.to.index()];
}

void Searcher::order_moves(Board& board, MoveList& moves, const Move& hash, int ply) const {
  std::sort(moves.begin(), moves.end(), [&](const Move& a, const Move& b) {
    return move_score(board, a, hash, ply) > move_score(board, b, hash, ply);
  });
}

void Searcher::on_quiet_cutoff(int ply, const Move& m, int depth) {
  if (ply < 0 || ply >= kMaxPly) return;
  if (killers_[ply][0] != m) {
    killers_[ply][1] = killers_[ply][0];
    killers_[ply][0] = m;
  }
  int& h = history_[m.from.index()][m.to.index()];
  h += depth * depth;
  if (h > 20'000) h = 20'000;
}

bool Searcher::time_up() const {
  return use_deadline_ && now_seconds() >= deadline_;
}

bool Searcher::soft_stop() const {
  return use_soft_ && now_seconds() >= soft_deadline_;
}

bool Searcher::timed_out() {
  if (abort_) return true;
  if ((nodes_ & 255ULL) != 0) return false;
  if (time_up()) {
    abort_ = true;
    return true;
  }
  return false;
}

int Searcher::terminal_score(Board& board, int ply) {
  if (board.in_check()) return -kMateScore + ply;
  return 0;
}

int Searcher::quiescence(Board& board, int ply, int alpha, int beta) {
  ++nodes_;
  if (timed_out()) return board.evaluate();
  if (ply >= 18) return board.evaluate();
  if (board.halfmove_clock() >= 100 || board.repetition_count() >= 3) return 0;

  if (board.in_check()) {
    MoveList moves;
    board.generate_pseudo(moves, MoveGen::All);
    order_moves(board, moves, tt().probe_move(board.hash()), ply);
    int best = -kInfScore;
    int legal = 0;
    const Color us = board.side_to_move();
    for (const Move& m : moves) {
      board.make(m);
      if (board.in_check(us)) {
        board.unmake();
        continue;
      }
      ++legal;
      int score = -quiescence(board, ply + 1, -beta, -alpha);
      board.unmake();
      best = std::max(best, score);
      alpha = std::max(alpha, score);
      if (alpha >= beta) break;
    }
    if (legal == 0) return terminal_score(board, ply);
    return best;
  }

  const int stand = board.evaluate();
  if (stand >= beta) return stand;
  if (stand > alpha) alpha = stand;

  MoveList captures;
  board.generate_pseudo(captures, MoveGen::Captures);
  order_moves(board, captures, tt().probe_move(board.hash()), ply);
  const Color us = board.side_to_move();
  for (const Move& m : captures) {
    if (stand + 900 < alpha && m.flag != MoveFlag::Promotion) continue;
    board.make(m);
    if (board.in_check(us)) {
      board.unmake();
      continue;
    }
    int score = -quiescence(board, ply + 1, -beta, -alpha);
    board.unmake();
    if (score >= beta) return score;
    if (score > alpha) alpha = score;
  }
  return alpha;
}

int Searcher::minimax(Board& board, int depth, int ply) {
  ++nodes_;
  if (timed_out()) return board.evaluate();
  if (board.halfmove_clock() >= 100 || board.repetition_count() >= 3) return 0;

  MoveList moves = board.legal_moves();
  if (moves.empty()) return terminal_score(board, ply);
  if (depth <= 0) return board.evaluate();

  int best = -kInfScore;
  for (const Move& m : moves) {
    board.make(m);
    int score = -minimax(board, depth - 1, ply + 1);
    board.unmake();
    best = std::max(best, score);
  }
  return best;
}

int Searcher::alphabeta(Board& board, int depth, int ply, int alpha, int beta, bool pv) {
  ++nodes_;
  if (timed_out()) return board.evaluate();
  if (ply >= kMaxPly - 1) return board.evaluate();
  if (board.halfmove_clock() >= 100 || board.repetition_count() >= 3) return 0;

  const int orig_alpha = alpha;
  alpha = std::max(alpha, -kMateScore + ply);
  beta = std::min(beta, kMateScore - ply - 1);
  if (alpha >= beta) return alpha;

  const bool in_check = board.in_check();
  if (in_check) depth = std::max(depth, 1);

  if (depth <= 0) {
    if (limits_.mode == SearchMode::AlphaBetaQuiescence && !abort_) {
      return quiescence(board, ply, alpha, beta);
    }
    return board.evaluate();
  }

  const std::uint64_t key = board.hash();
  Move hash_move = tt().probe_move(key);
  int tt_score = 0;
  if (!pv && tt().cutoff(key, depth, ply, alpha, beta, tt_score)) return tt_score;

  const int eval = in_check ? -kInfScore : board.evaluate();
  if (!pv && !in_check && depth <= 3 && eval < kMateScore - 512 && eval > -kMateScore + 512 &&
      eval - 120 * depth >= beta) {
    return eval;
  }

  if (!pv && !in_check && depth >= 3 && eval >= beta &&
      board.has_non_pawn_material(board.side_to_move())) {
    const int R = 2 + depth / 4;
    board.make_null();
    int score = -alphabeta(board, std::max(0, depth - 1 - R), ply + 1, -beta, -beta + 1, false);
    board.unmake();
    if (abort_) return eval;
    if (score >= beta) {
      if (score >= kMateScore - 512) score = beta;
      return score;
    }
  }

  int best = -kInfScore;
  Move best_move{};
  int legal = 0;
  bool cutoff = false;

  auto search_one = [&](const Move& m) {
    const bool tactical =
        is_capture(board, m) || m.flag == MoveFlag::Promotion;
    const Color us = board.side_to_move();
    board.make(m);
    if (board.in_check(us)) {
      board.unmake();
      return;
    }
    ++legal;
    const int new_depth = depth - 1;
    int reduction = 0;
    if (!pv && !tactical && !in_check && depth >= 3 && legal >= 4 && new_depth > 0) {
      reduction = 1;
      if (legal >= 8) ++reduction;
      if (depth >= 6) ++reduction;
      reduction = std::min(reduction, new_depth);
    }

    int score;
    if (legal == 1) {
      score = -alphabeta(board, new_depth, ply + 1, -beta, -alpha, pv);
    } else {
      score = -alphabeta(board, new_depth - reduction, ply + 1, -alpha - 1, -alpha, false);
      if (reduction > 0 && score > alpha) {
        score = -alphabeta(board, new_depth, ply + 1, -alpha - 1, -alpha, false);
      }
      if (pv && score > alpha && score < beta) {
        score = -alphabeta(board, new_depth, ply + 1, -beta, -alpha, true);
      }
    }
    board.unmake();
    if (abort_) return;
    if (score > best) {
      best = score;
      best_move = m;
    }
    if (score > alpha) alpha = score;
    if (alpha >= beta) {
      if (!tactical) on_quiet_cutoff(ply, m, depth);
      cutoff = true;
    }
  };

  if (!hash_move.is_null()) {
    const Piece p = board.piece_at(hash_move.from);
    if (p != Piece::None && color_of(p) == board.side_to_move()) {
      search_one(hash_move);
    }
  }

  if (!cutoff && !abort_) {
    MoveList captures;
    board.generate_pseudo(captures, MoveGen::Captures);
    order_moves(board, captures, hash_move, ply);
    for (const Move& m : captures) {
      if (!hash_move.is_null() && m == hash_move) continue;
      search_one(m);
      if (cutoff || abort_) break;
    }
  }

  if (!cutoff && !abort_) {
    MoveList quiets;
    board.generate_pseudo(quiets, MoveGen::Quiets);
    order_moves(board, quiets, hash_move, ply);
    for (const Move& m : quiets) {
      if (!hash_move.is_null() && m == hash_move) continue;
      if (legal > 0 && !pv && !in_check && depth <= 2 && eval > -kInfScore / 2 &&
          eval + 200 * depth <= orig_alpha) {
        continue;
      }
      search_one(m);
      if (cutoff || abort_) break;
    }
  }

  if (abort_) return eval > -kInfScore / 2 ? eval : board.evaluate();
  if (legal == 0) return terminal_score(board, ply);

  TTFlag flag = TTFlag::Exact;
  if (best <= orig_alpha) flag = TTFlag::Upper;
  else if (best >= beta) flag = TTFlag::Lower;
  tt().store(key, depth, ply, best, flag, best_move);
  return best;
}

SearchResult Searcher::search(Board& board, const SearchLimits& limits) {
  limits_ = limits;
  nodes_ = 0;
  abort_ = false;
  for (int p = 0; p < kMaxPly; ++p) {
    killers_[p][0] = Move{};
    killers_[p][1] = Move{};
  }
  for (int i = 0; i < 64; ++i) {
    for (int j = 0; j < 64; ++j) history_[i][j] = 0;
  }

  const double start = now_seconds();
  use_deadline_ = limits.max_seconds > 0;
  deadline_ = use_deadline_ ? start + limits.max_seconds : 0.0;

  double target = limits.target_seconds;
  if (target <= 0 && use_deadline_) target = limits.max_seconds * 0.70;
  use_soft_ = target > 0;
  soft_deadline_ = use_soft_ ? start + target : 0.0;

  SearchResult result;
  MoveList moves = board.legal_moves();
  if (moves.empty()) {
    result.score = terminal_score(board, 0);
    result.seconds = now_seconds() - start;
    result.nodes = nodes_;
    result.depth = 0;
    return result;
  }

  Move hash = tt().probe_move(board.hash());
  order_moves(board, moves, hash, 0);
  Move best = moves[0];
  int best_score = -kInfScore;
  int completed_depth = 0;
  const int max_depth = std::max(1, limits.depth);
  double last_iter = 0.0;

  if (moves.size() == 1) {
    result.best_move = best;
    result.score = 0;
    result.depth = 0;
    result.nodes = nodes_;
    result.seconds = now_seconds() - start;
    return result;
  }

  for (int depth = 1; depth <= max_depth; ++depth) {
    if (abort_ || time_up()) break;
    if (depth > 1 && soft_stop()) break;
    if (depth > 1 && last_iter > 0 && use_soft_) {
      const double remaining = soft_deadline_ - now_seconds();
      if (remaining < last_iter * 1.5) break;
    }

    const double iter_start = now_seconds();
    int alpha = -kInfScore;
    int iter_score = -kInfScore;
    Move iter_best = best;
    bool finished = true;

    for (int i = 0; i < moves.size(); ++i) {
      if ((abort_ || time_up()) && (completed_depth > 0 || iter_score != -kInfScore)) {
        finished = false;
        break;
      }
      board.make(moves[i]);
      int score;
      if (limits.mode == SearchMode::Minimax) {
        score = -minimax(board, depth - 1, 1);
      } else if (i == 0) {
        score = -alphabeta(board, depth - 1, 1, -kInfScore, kInfScore, true);
      } else {
        score = -alphabeta(board, depth - 1, 1, -alpha - 1, -alpha, false);
        if (score > alpha) {
          score = -alphabeta(board, depth - 1, 1, -kInfScore, kInfScore, true);
        }
      }
      board.unmake();
      if (abort_ && completed_depth > 0) {
        finished = false;
        break;
      }
      if (score > iter_score) {
        iter_score = score;
        iter_best = moves[i];
        alpha = std::max(alpha, score);
      }
    }

    last_iter = now_seconds() - iter_start;
    if (!finished) break;
    if (iter_score == -kInfScore) break;

    best = iter_best;
    best_score = iter_score;
    completed_depth = depth;
    tt().store(board.hash(), depth, 0, best_score, TTFlag::Exact, best);
    for (int i = 0; i < moves.size(); ++i) {
      if (moves[i] == best) {
        while (i > 0) {
          std::swap(moves[i], moves[i - 1]);
          --i;
        }
        break;
      }
    }
    if (best_score >= kMateScore - 64) break;
  }

  result.best_move = best;
  result.score = best_score;
  result.depth = completed_depth;
  result.nodes = nodes_;
  result.seconds = now_seconds() - start;
  return result;
}

SearchResult search(Board& board, const SearchLimits& limits) {
  Searcher s;
  return s.search(board, limits);
}

}  // namespace chess
