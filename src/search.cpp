#include "search.hpp"

#include "see.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <utility>
#include <vector>

namespace chess {
namespace {

int piece_value(PieceType t) { return see_piece_value(t); }

double now_seconds() {
  using clock = std::chrono::steady_clock;
  return std::chrono::duration<double>(clock::now().time_since_epoch()).count();
}

enum class TTFlag : std::uint8_t { Exact = 0, Lower = 1, Upper = 2 };

struct alignas(16) TTEntry {
  std::uint64_t key = 0;
  Move move{};
  std::int16_t score = 0;
  std::int8_t depth = -1;
  std::uint8_t gen_flag = 0;  // bits 0-1 flag, bits 4-7 generation

  TTFlag flag() const { return static_cast<TTFlag>(gen_flag & 3u); }
  std::uint8_t generation() const { return static_cast<std::uint8_t>(gen_flag >> 4); }
  void set_meta(TTFlag f, std::uint8_t gen) {
    gen_flag = static_cast<std::uint8_t>((gen << 4) | (static_cast<std::uint8_t>(f) & 3u));
  }
};
static_assert(sizeof(TTEntry) == 16, "TT entry should be one quarter of a cache line");

struct alignas(64) TTCluster {
  TTEntry e[4];
};
static_assert(sizeof(TTCluster) == 64, "TT cluster should be one cache line");

constexpr int kTTBuckets = 1 << 18;
constexpr int kTTCluster = 4;

std::int16_t pack_tt_score(int s) {
  if (s >= kMateScore - 512) return static_cast<std::int16_t>(32000 - (kMateScore - s));
  if (s <= -kMateScore + 512) return static_cast<std::int16_t>(-32000 + (kMateScore + s));
  return static_cast<std::int16_t>(std::clamp(s, -30000, 30000));
}

int unpack_tt_score(std::int16_t s) {
  if (s >= 31000) return kMateScore - (32000 - s);
  if (s <= -31000) return -kMateScore + (32000 + s);
  return s;
}

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
  std::vector<TTCluster> t;
  std::uint8_t generation = 0;
  TranspositionTable() : t(kTTBuckets) {}

  void new_search() { generation = static_cast<std::uint8_t>((generation + 1) & 15); }

  TTCluster& cluster(std::uint64_t k) { return t[k & (kTTBuckets - 1)]; }
  const TTCluster& cluster(std::uint64_t k) const { return t[k & (kTTBuckets - 1)]; }

  const TTEntry* find(std::uint64_t k) const {
    const TTCluster& c = cluster(k);
    for (int i = 0; i < kTTCluster; ++i) {
      if (c.e[i].key == k && c.e[i].depth >= 0) return &c.e[i];
    }
    return nullptr;
  }

  Move probe_move(std::uint64_t k) const {
    const TTEntry* e = find(k);
    return e ? e->move : Move{};
  }

  bool cutoff(std::uint64_t k, int depth, int ply, int alpha, int beta, int& score) const {
    const TTEntry* e = find(k);
    if (!e || e->depth < depth) return false;
    const int s = from_tt(unpack_tt_score(e->score), ply);
    if (e->flag() == TTFlag::Exact) {
      score = s;
      return true;
    }
    if (e->flag() == TTFlag::Lower && s >= beta) {
      score = s;
      return true;
    }
    if (e->flag() == TTFlag::Upper && s <= alpha) {
      score = s;
      return true;
    }
    return false;
  }

  void store(std::uint64_t k, int depth, int ply, int score, TTFlag flag, Move m) {
    TTCluster& c = cluster(k);
    int replace = 0;
    int best_rank = -1'000'000;
    for (int i = 0; i < kTTCluster; ++i) {
      TTEntry& e = c.e[i];
      if (e.key == k && e.depth >= 0) {
        if (e.depth > depth && e.generation() == generation) return;
        replace = i;
        best_rank = 1'000'000;
        break;
      }
      if (e.depth < 0) {
        replace = i;
        best_rank = 1'000'000;
        break;
      }
      const int age = (generation - e.generation()) & 15;
      const int rank = age * 16 - e.depth;
      if (rank > best_rank) {
        best_rank = rank;
        replace = i;
      }
    }
    TTEntry& e = c.e[replace];
    e.key = k;
    e.depth = static_cast<std::int8_t>(std::clamp(depth, 0, 127));
    e.score = pack_tt_score(to_tt(score, ply));
    e.set_meta(flag, generation);
    e.move = m;
  }
};

TranspositionTable& tt() {
  static TranspositionTable table;
  return table;
}

struct LmrTable {
  int t[64][64]{};
  LmrTable() {
    for (int d = 1; d < 64; ++d) {
      for (int m = 1; m < 64; ++m) {
        t[d][m] = static_cast<int>(std::lround(std::log(static_cast<double>(d)) *
                                               std::log(static_cast<double>(m)) / 2.25));
      }
    }
  }
};

const LmrTable kLmr{};

int lmr_reduction(int depth, int move_number) {
  return kLmr.t[std::clamp(depth, 0, 63)][std::clamp(move_number, 0, 63)];
}

}  // namespace

bool Searcher::is_capture(const Board& board, const Move& m) const {
  if (m.flag == MoveFlag::EnPassant) return true;
  return board.piece_at(m.to) != Piece::None;
}

int Searcher::move_score(const Board& board, const Move& m, const Move& hash, int ply) const {
  if (!hash.is_null() && m == hash) return 1'000'000;
  if (m.flag == MoveFlag::Promotion) {
    const int promo = 800'000 + piece_value(m.promotion);
    if (is_capture(board, m)) {
      const int s = see(board, m);
      return s >= 0 ? promo + 1'000 : promo + s;
    }
    return promo;
  }
  if (is_capture(board, m)) {
    Piece captured = m.flag == MoveFlag::EnPassant
                         ? make_piece(opposite(board.side_to_move()), PieceType::Pawn)
                         : board.piece_at(m.to);
    Piece attacker = board.piece_at(m.from);
    const int s = see(board, m);
    if (s >= 0) return 100'000 + s + 10 * piece_value(type_of(captured)) - piece_value(type_of(attacker));
    return s;
  }
  if (ply >= 0 && ply < kMaxPly) {
    if (m == killers_[ply][0]) return 90'000;
    if (m == killers_[ply][1]) return 80'000;
  }
  if (m.flag == MoveFlag::CastleKingside || m.flag == MoveFlag::CastleQueenside) return 50;
  return history_[m.from.index()][m.to.index()];
}

void Searcher::score_moves(const Board& board, const MoveList& moves, int* scores, const Move& hash,
                           int ply) const {
  for (int i = 0; i < moves.size(); ++i) {
    scores[i] = move_score(board, moves[i], hash, ply);
  }
}

void Searcher::pick_next(MoveList& moves, int* scores, int idx) {
  int best = idx;
  for (int i = idx + 1; i < moves.size(); ++i) {
    if (scores[i] > scores[best]) best = i;
  }
  if (best != idx) {
    std::swap(moves[idx], moves[best]);
    std::swap(scores[idx], scores[best]);
  }
}

void Searcher::order_moves(Board& board, MoveList& moves, const Move& hash, int ply) const {
  int scores[MoveList::kMax];
  score_moves(board, moves, scores, hash, ply);
  for (int i = 0; i < moves.size(); ++i) pick_next(moves, scores, i);
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
    int scores[MoveList::kMax];
    score_moves(board, moves, scores, tt().probe_move(board.hash()), ply);
    int best = -kInfScore;
    int legal = 0;
    const Color us = board.side_to_move();
    for (int i = 0; i < moves.size(); ++i) {
      pick_next(moves, scores, i);
      const Move& m = moves[i];
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
  int scores[MoveList::kMax];
  score_moves(board, captures, scores, tt().probe_move(board.hash()), ply);
  const Color us = board.side_to_move();
  for (int i = 0; i < captures.size(); ++i) {
    pick_next(captures, scores, i);
    const Move& m = captures[i];
    if (m.flag != MoveFlag::Promotion && scores[i] < 0 && !board.gives_check(m)) continue;
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
  if (!in_check) eval_stack_[ply] = eval;
  else eval_stack_[ply] = ply > 0 ? eval_stack_[ply - 1] : 0;
  const bool improving = ply < 2 || eval_stack_[ply] >= eval_stack_[ply - 2];

  if (!pv && !in_check && depth <= 3 && eval < kMateScore - 512 && eval > -kMateScore + 512 &&
      eval - (improving ? 90 : 120) * depth >= beta) {
    return eval;
  }

  if (!pv && !in_check && depth >= 3 && eval >= beta &&
      board.has_non_pawn_material(board.side_to_move())) {
    const int R = 2 + depth / 4 + (improving ? 1 : 0);
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
    const bool tactical = is_capture(board, m) || m.flag == MoveFlag::Promotion;
    const bool killer = ply >= 0 && ply < kMaxPly && (m == killers_[ply][0] || m == killers_[ply][1]);
    const int hist = history_[m.from.index()][m.to.index()];
    const Color us = board.side_to_move();
    board.make(m);
    if (board.in_check(us)) {
      board.unmake();
      return;
    }
    ++legal;
    const int new_depth = depth - 1;
    int reduction = 0;
    if (!tactical && !in_check && depth >= 3 && legal >= 3 && new_depth > 0) {
      reduction = lmr_reduction(depth, legal);
      if (pv) --reduction;
      if (killer) --reduction;
      if (hist > 4'000) --reduction;
      if (!improving) ++reduction;
      if (!pv && legal >= 12 && hist < 200) ++reduction;
      reduction = std::clamp(reduction, 0, new_depth);
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
    int scores[MoveList::kMax];
    score_moves(board, captures, scores, hash_move, ply);
    for (int i = 0; i < captures.size(); ++i) {
      pick_next(captures, scores, i);
      const Move& m = captures[i];
      if (!hash_move.is_null() && m == hash_move) continue;
      search_one(m);
      if (cutoff || abort_) break;
    }
  }

  if (!cutoff && !abort_) {
    MoveList quiets;
    board.generate_pseudo(quiets, MoveGen::Quiets);
    int scores[MoveList::kMax];
    score_moves(board, quiets, scores, hash_move, ply);
    for (int i = 0; i < quiets.size(); ++i) {
      pick_next(quiets, scores, i);
      const Move& m = quiets[i];
      if (!hash_move.is_null() && m == hash_move) continue;
      if (legal > 0 && !pv && !in_check && depth <= 2 && eval > -kInfScore / 2 &&
          eval + (improving ? 200 : 140) * depth <= orig_alpha) {
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
  tt().new_search();
  for (int p = 0; p < kMaxPly; ++p) {
    killers_[p][0] = Move{};
    killers_[p][1] = Move{};
  }
  std::memset(history_, 0, sizeof(history_));
  std::memset(eval_stack_, 0, sizeof(eval_stack_));
  eval_stack_[0] = board.evaluate();

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

  int stable_hits = 0;
  int prev_score = 0;
  Move prev_best{};

  for (int depth = 1; depth <= max_depth; ++depth) {
    if (abort_ || time_up()) break;
    if (depth > 1) {
      const bool unstable = stable_hits < 2;
      if (!unstable && soft_stop()) break;
      if (!unstable && last_iter > 0 && use_soft_) {
        const double remaining = soft_deadline_ - now_seconds();
        if (remaining < last_iter * 1.5) break;
      }
    }

    const double iter_start = now_seconds();
    const bool use_asp = depth > 1 && limits.mode != SearchMode::Minimax;
    int delta = 25;
    int asp_alpha = -kInfScore;
    int asp_beta = kInfScore;
    if (use_asp) {
      asp_alpha = prev_score - delta;
      asp_beta = prev_score + delta;
    }

    int iter_score = -kInfScore;
    Move iter_best = best;
    bool finished = false;

    while (true) {
      iter_score = -kInfScore;
      iter_best = best;
      finished = true;
      int alpha = asp_alpha;

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
          score = -alphabeta(board, depth - 1, 1, -asp_beta, -asp_alpha, true);
        } else {
          score = -alphabeta(board, depth - 1, 1, -alpha - 1, -alpha, false);
          if (score > alpha && score < asp_beta) {
            score = -alphabeta(board, depth - 1, 1, -asp_beta, -alpha, true);
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
        }
        if (score > alpha) alpha = score;
        if (alpha >= asp_beta) break;
      }

      if (!finished || abort_) break;
      if (iter_score == -kInfScore) {
        finished = false;
        break;
      }
      if (use_asp && iter_score <= asp_alpha && asp_alpha > -kInfScore) {
        if (time_up()) {
          finished = false;
          break;
        }
        delta *= 2;
        asp_alpha = delta > 500 ? -kInfScore : iter_score - delta;
        continue;
      }
      if (use_asp && iter_score >= asp_beta && asp_beta < kInfScore) {
        if (time_up()) {
          finished = false;
          break;
        }
        delta *= 2;
        asp_beta = delta > 500 ? kInfScore : iter_score + delta;
        continue;
      }
      break;
    }

    last_iter = now_seconds() - iter_start;
    if (!finished) break;
    if (iter_score == -kInfScore) break;

    if (completed_depth > 0 && iter_best == prev_best &&
        std::abs(iter_score - prev_score) < 35) {
      ++stable_hits;
    } else {
      stable_hits = 0;
    }
    prev_best = iter_best;
    prev_score = iter_score;

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
