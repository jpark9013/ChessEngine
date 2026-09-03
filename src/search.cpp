#include "search.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>

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

int mvv_lva(const Board& board, const Move& m) {
  int score = 0;
  if (m.flag == MoveFlag::Promotion) score += 800 + piece_value(m.promotion);
  Piece captured = Piece::None;
  if (m.flag == MoveFlag::EnPassant) captured = make_piece(opposite(board.side_to_move()), PieceType::Pawn);
  else captured = board.piece_at(m.to);
  if (captured != Piece::None) {
    score += 10000 + 10 * piece_value(type_of(captured)) - piece_value(type_of(board.piece_at(m.from)));
  }
  if (m.flag == MoveFlag::CastleKingside || m.flag == MoveFlag::CastleQueenside) score += 50;
  return score;
}

double now_seconds() {
  using clock = std::chrono::steady_clock;
  return std::chrono::duration<double>(clock::now().time_since_epoch()).count();
}

}  // namespace

void Searcher::order_moves(Board& board, MoveList& moves) const {
  std::sort(moves.begin(), moves.end(), [&](const Move& a, const Move& b) {
    return mvv_lva(board, a) > mvv_lva(board, b);
  });
}

bool Searcher::time_up() const {
  return use_deadline_ && now_seconds() >= deadline_;
}

int Searcher::terminal_score(Board& board, int ply) {
  if (board.in_check()) return -kMateScore + ply;
  return 0;
}

int Searcher::quiescence(Board& board, int ply, int alpha, int beta) {
  ++nodes_;
  if ((nodes_ & 2047ULL) == 0 && time_up()) return board.evaluate();

  if (board.halfmove_clock() >= 100 || board.repetition_count() >= 3) return 0;

  if (board.in_check()) {
    MoveList moves = board.legal_moves();
    if (moves.empty()) return terminal_score(board, ply);
    order_moves(board, moves);
    int best = -kInfScore;
    for (const Move& m : moves) {
      board.make(m);
      int score = -quiescence(board, ply + 1, -beta, -alpha);
      board.unmake();
      best = std::max(best, score);
      alpha = std::max(alpha, score);
      if (alpha >= beta) break;
    }
    return best;
  }

  const int stand = board.evaluate();
  if (stand >= beta) return stand;
  if (stand > alpha) alpha = stand;

  MoveList captures = board.legal_captures();
  order_moves(board, captures);
  for (const Move& m : captures) {
    board.make(m);
    int score = -quiescence(board, ply + 1, -beta, -alpha);
    board.unmake();
    if (score >= beta) return score;
    if (score > alpha) alpha = score;
  }
  return alpha;
}

int Searcher::minimax(Board& board, int depth, int ply) {
  ++nodes_;
  if ((nodes_ & 2047ULL) == 0 && time_up()) return board.evaluate();
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

int Searcher::alphabeta(Board& board, int depth, int ply, int alpha, int beta) {
  ++nodes_;
  if ((nodes_ & 2047ULL) == 0 && time_up()) return board.evaluate();
  if (board.halfmove_clock() >= 100 || board.repetition_count() >= 3) return 0;

  MoveList moves = board.legal_moves();
  if (moves.empty()) return terminal_score(board, ply);

  if (depth <= 0) {
    if (limits_.mode == SearchMode::AlphaBetaQuiescence) {
      return quiescence(board, ply, alpha, beta);
    }
    return board.evaluate();
  }

  order_moves(board, moves);
  int best = -kInfScore;
  for (const Move& m : moves) {
    board.make(m);
    int score = -alphabeta(board, depth - 1, ply + 1, -beta, -alpha);
    board.unmake();
    best = std::max(best, score);
    alpha = std::max(alpha, score);
    if (alpha >= beta) break;
  }
  return best;
}

SearchResult Searcher::search(Board& board, const SearchLimits& limits) {
  limits_ = limits;
  nodes_ = 0;
  use_deadline_ = limits.max_seconds > 0;
  deadline_ = use_deadline_ ? now_seconds() + limits.max_seconds : 0.0;

  SearchResult result;
  const double start = now_seconds();
  MoveList moves = board.legal_moves();
  if (moves.empty()) {
    result.score = terminal_score(board, 0);
    result.seconds = now_seconds() - start;
    result.nodes = nodes_;
    result.depth = 0;
    return result;
  }

  order_moves(board, moves);
  Move best = moves[0];
  int best_score = -kInfScore;
  const int depth = std::max(1, limits.depth);

  for (const Move& m : moves) {
    if (time_up() && best_score != -kInfScore) break;
    board.make(m);
    int score;
    if (limits.mode == SearchMode::Minimax) {
      score = -minimax(board, depth - 1, 1);
    } else {
      score = -alphabeta(board, depth - 1, 1, -kInfScore, kInfScore);
    }
    board.unmake();
    if (score > best_score) {
      best_score = score;
      best = m;
    }
  }

  result.best_move = best;
  result.score = best_score;
  result.depth = depth;
  result.nodes = nodes_;
  result.seconds = now_seconds() - start;
  return result;
}

SearchResult search(Board& board, const SearchLimits& limits) {
  Searcher s;
  return s.search(board, limits);
}

}  // namespace chess
