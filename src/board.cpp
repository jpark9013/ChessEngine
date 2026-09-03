#include "board.hpp"
#include "attacks.hpp"
#include "zobrist.hpp"

#include <algorithm>
#include <cctype>
#include <sstream>
#include <stdexcept>

namespace chess {
namespace {

int piece_index(Piece p) { return static_cast<int>(p) - 1; }

int castle_mask_lost_on_square(Square sq) {
  int mask = 0;
  if (sq == Square(0, 0)) mask |= kCastleWQ;
  if (sq == Square(0, 4)) mask |= kCastleWK | kCastleWQ;
  if (sq == Square(0, 7)) mask |= kCastleWK;
  if (sq == Square(7, 0)) mask |= kCastleBQ;
  if (sq == Square(7, 4)) mask |= kCastleBK | kCastleBQ;
  if (sq == Square(7, 7)) mask |= kCastleBK;
  return mask;
}

constexpr int kPieceValue[] = {0, 100, 320, 330, 500, 900, 0};

// Piece-square tables stored a1..h8, white's perspective.
constexpr int kPst[7][64] = {
    {0},
    {0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10,-20,-20, 10, 10,  5,
     5, -5,-10,  0,  0,-10, -5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5,  5, 10, 25, 25, 10,  5,  5,
    10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,
     0,  0,  0,  0,  0,  0,  0,  0},
    {-50,-40,-30,-30,-30,-30,-40,-50,
     -40,-20,  0,  0,  0,  0,-20,-40,
     -30,  0, 10, 15, 15, 10,  0,-30,
     -30,  5, 15, 20, 20, 15,  5,-30,
     -30,  0, 15, 20, 20, 15,  0,-30,
     -30,  5, 10, 15, 15, 10,  5,-30,
     -40,-20,  0,  5,  5,  0,-20,-40,
     -50,-40,-30,-30,-30,-30,-40,-50},
    {-20,-10,-10,-10,-10,-10,-10,-20,
     -10,  5,  0,  0,  0,  0,  5,-10,
     -10, 10, 10, 10, 10, 10, 10,-10,
     -10,  0, 10, 10, 10, 10,  0,-10,
     -10,  5,  5, 10, 10,  5,  5,-10,
     -10,  0,  5, 10, 10,  5,  0,-10,
     -10,  0,  0,  0,  0,  0,  0,-10,
     -20,-10,-10,-10,-10,-10,-10,-20},
    {0,  0,  0,  5,  5,  0,  0,  0,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     5, 10, 10, 10, 10, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0},
    {-20,-10,-10, -5, -5,-10,-10,-20,
     -10,  0,  5,  0,  0,  0,  0,-10,
     -10,  5,  5,  5,  5,  5,  0,-10,
       0,  0,  5,  5,  5,  5,  0, -5,
      -5,  0,  5,  5,  5,  5,  0, -5,
     -10,  0,  5,  5,  5,  5,  0,-10,
     -10,  0,  0,  0,  0,  0,  0,-10,
     -20,-10,-10, -5, -5,-10,-10,-20},
    {20, 30, 10,  0,  0, 10, 30, 20,
     20, 20,  0,  0,  0,  0, 20, 20,
    -10,-20,-20,-20,-20,-20,-20,-10,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30}
};

constexpr int kKingEndgame[64] = {
    -50,-30,-30,-30,-30,-30,-30,-50,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -50,-40,-30,-20,-20,-30,-40,-50
};

int pst_index(Color c, Square sq) {
  return c == Color::White ? sq.index() : (sq.index() ^ 56);
}

int eval_delta(Piece p, Square sq) {
  if (p == Piece::None) return 0;
  const Color c = color_of(p);
  const PieceType t = type_of(p);
  const int idx = pst_index(c, sq);
  int val = kPieceValue[static_cast<int>(t)];
  if (t != PieceType::King) val += kPst[static_cast<int>(t)][idx];
  return c == Color::White ? val : -val;
}

int king_mg_delta(Piece p, Square sq) {
  if (type_of(p) != PieceType::King) return 0;
  const int idx = pst_index(color_of(p), sq);
  const int val = kPst[6][idx];
  return color_of(p) == Color::White ? val : -val;
}

int king_eg_delta(Piece p, Square sq) {
  if (type_of(p) != PieceType::King) return 0;
  const int idx = pst_index(color_of(p), sq);
  const int val = kKingEndgame[idx];
  return color_of(p) == Color::White ? val : -val;
}

}  // namespace

Board::Board() {
  attacks::init();
  startpos();
}

void Board::clear() {
  squares_.fill(Piece::None);
  pieces_.fill(0);
  occupancy_.fill(0);
  stm_ = Color::White;
  castling_ = 0;
  ep_ = Square::none_index;
  halfmove_ = 0;
  fullmove_ = 1;
  hash_ = 0;
  white_king_ = Square();
  black_king_ = Square();
  eval_noking_ = 0;
  king_mg_ = 0;
  king_eg_ = 0;
  queens_ = 0;
  undo_.clear();
  history_.clear();
}

void Board::put(Square sq, Piece p) {
  const int i = sq.index();
  const Piece old = squares_[i];
  if (old != Piece::None) {
    const Bitboard b = bit(i);
    pieces_[piece_index(old)] &= ~b;
    occupancy_[static_cast<int>(color_of(old))] &= ~b;
    if (type_of(old) == PieceType::Queen) --queens_;
    if (type_of(old) == PieceType::King) {
      king_mg_ -= king_mg_delta(old, sq);
      king_eg_ -= king_eg_delta(old, sq);
    } else {
      eval_noking_ -= eval_delta(old, sq);
    }
  }
  squares_[i] = p;
  if (p == Piece::None) return;
  const Bitboard b = bit(i);
  pieces_[piece_index(p)] |= b;
  occupancy_[static_cast<int>(color_of(p))] |= b;
  if (p == Piece::WKing) white_king_ = sq;
  if (p == Piece::BKing) black_king_ = sq;
  if (type_of(p) == PieceType::Queen) ++queens_;
  if (type_of(p) == PieceType::King) {
    king_mg_ += king_mg_delta(p, sq);
    king_eg_ += king_eg_delta(p, sq);
  } else {
    eval_noking_ += eval_delta(p, sq);
  }
}

void Board::remove(Square sq) { put(sq, Piece::None); }

Bitboard Board::piece_bb(Color c, PieceType t) const {
  return pieces_[piece_index(make_piece(c, t))];
}

void Board::set_piece(Square sq, Piece p) { put(sq, p); }

void Board::find_kings() {
  white_king_ = Square();
  black_king_ = Square();
  for (int i = 0; i < 64; ++i) {
    if (squares_[i] == Piece::WKing) white_king_ = Square(i);
    if (squares_[i] == Piece::BKing) black_king_ = Square(i);
  }
}

void Board::rebuild_hash() {
  hash_ = 0;
  for (int i = 0; i < 64; ++i) {
    if (squares_[i] != Piece::None) hash_ ^= zobrist::piece(squares_[i], Square(i));
  }
  hash_ ^= zobrist::castle(castling_);
  if (stm_ == Color::Black) hash_ ^= zobrist::side_to_move();
  if (ep_ != Square::none_index) hash_ ^= zobrist::en_passant_file(Square(ep_).file());
}

void Board::startpos() {
  *this = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
}

bool Board::is_attacked(Square sq, Color by) const {
  if (!sq.valid()) return false;
  const int s = sq.index();
  const Bitboard occ = occupancy();
  if (attacks::pawn(opposite(by), s) & piece_bb(by, PieceType::Pawn)) return true;
  if (attacks::knight(s) & piece_bb(by, PieceType::Knight)) return true;
  if (attacks::king(s) & piece_bb(by, PieceType::King)) return true;
  const Bitboard bishops = piece_bb(by, PieceType::Bishop) | piece_bb(by, PieceType::Queen);
  if (attacks::bishop(s, occ) & bishops) return true;
  const Bitboard rooks = piece_bb(by, PieceType::Rook) | piece_bb(by, PieceType::Queen);
  if (attacks::rook(s, occ) & rooks) return true;
  return false;
}

bool Board::can_castle(Color c, bool kingside) const {
  const int rights = kingside
                         ? (c == Color::White ? kCastleWK : kCastleBK)
                         : (c == Color::White ? kCastleWQ : kCastleBQ);
  if ((castling_ & rights) == 0) return false;
  if (in_check(c)) return false;

  const int rank = (c == Color::White) ? 0 : 7;
  const Square king = Square(rank, 4);
  if (piece_at(king) != make_piece(c, PieceType::King)) return false;

  const Bitboard occ = occupancy();
  if (kingside) {
    if (piece_at(Square(rank, 7)) != make_piece(c, PieceType::Rook)) return false;
    if (occ & (bit(Square(rank, 5).index()) | bit(Square(rank, 6).index()))) return false;
    if (is_attacked(Square(rank, 5), opposite(c))) return false;
    if (is_attacked(Square(rank, 6), opposite(c))) return false;
  } else {
    if (piece_at(Square(rank, 0)) != make_piece(c, PieceType::Rook)) return false;
    if (occ & (bit(Square(rank, 1).index()) | bit(Square(rank, 2).index()) |
               bit(Square(rank, 3).index()))) {
      return false;
    }
    if (is_attacked(Square(rank, 2), opposite(c))) return false;
    if (is_attacked(Square(rank, 3), opposite(c))) return false;
  }
  return true;
}

void Board::add_pawn_moves(MoveList& out, Square from, MoveGen gen) const {
  const Color us = stm_;
  const int dir = (us == Color::White) ? 1 : -1;
  const int start_rank = (us == Color::White) ? 1 : 6;
  const int promo_rank = (us == Color::White) ? 7 : 0;
  const Color them = opposite(us);
  const bool want_quiet = gen != MoveGen::Captures;
  const bool want_cap = gen != MoveGen::Quiets;

  auto add_promo_or_quiet = [&](Square to, MoveFlag flag, bool is_capture) {
    if (to.rank() == promo_rank) {
      out.push(Move::make(from, to, MoveFlag::Promotion, PieceType::Queen));
      out.push(Move::make(from, to, MoveFlag::Promotion, PieceType::Rook));
      out.push(Move::make(from, to, MoveFlag::Promotion, PieceType::Bishop));
      out.push(Move::make(from, to, MoveFlag::Promotion, PieceType::Knight));
    } else if ((want_quiet && !is_capture) || (want_cap && is_capture)) {
      out.push(Move::make(from, to, flag));
    }
  };

  const int r = from.rank();
  const int f = from.file();

  if (want_quiet) {
    const int r1 = r + dir;
    if (on_board(r1, f) && piece_at(Square(r1, f)) == Piece::None) {
      add_promo_or_quiet(Square(r1, f), MoveFlag::Normal, false);
      const int r2 = r + 2 * dir;
      if (r == start_rank && on_board(r2, f) && piece_at(Square(r2, f)) == Piece::None) {
        out.push(Move::make(from, Square(r2, f), MoveFlag::DoublePawn));
      }
    }
  }

  if (!want_cap) return;

  for (int df : {-1, 1}) {
    const int nr = r + dir;
    const int nf = f + df;
    if (!on_board(nr, nf)) continue;
    Square to(nr, nf);
    Piece dest = piece_at(to);
    if (dest != Piece::None && color_of(dest) == them) {
      add_promo_or_quiet(to, MoveFlag::Normal, true);
    } else if (ep_ != Square::none_index && to == Square(ep_)) {
      out.push(Move::make(from, to, MoveFlag::EnPassant));
    }
  }
}

void Board::generate_pseudo(MoveList& out, MoveGen gen) const {
  const Color us = stm_;
  const Bitboard us_occ = occupancy_[static_cast<int>(us)];
  const Bitboard them_occ = occupancy_[static_cast<int>(opposite(us))];
  const Bitboard occ = us_occ | them_occ;

  auto emit = [&](int from, Bitboard dests) {
    if (gen == MoveGen::Captures) dests &= them_occ;
    else if (gen == MoveGen::Quiets) dests &= ~occ;
    else dests &= ~us_occ;
    while (dests) {
      const int to = pop_lsb(dests);
      out.push(Move::make(Square(from), Square(to)));
    }
  };

  Bitboard pawns = piece_bb(us, PieceType::Pawn);
  while (pawns) add_pawn_moves(out, Square(pop_lsb(pawns)), gen);

  Bitboard knights = piece_bb(us, PieceType::Knight);
  while (knights) {
    const int from = pop_lsb(knights);
    emit(from, attacks::knight(from));
  }

  Bitboard bishops = piece_bb(us, PieceType::Bishop);
  while (bishops) {
    const int from = pop_lsb(bishops);
    emit(from, attacks::bishop(from, occ));
  }

  Bitboard rooks = piece_bb(us, PieceType::Rook);
  while (rooks) {
    const int from = pop_lsb(rooks);
    emit(from, attacks::rook(from, occ));
  }

  Bitboard queens = piece_bb(us, PieceType::Queen);
  while (queens) {
    const int from = pop_lsb(queens);
    emit(from, attacks::queen(from, occ));
  }

  Bitboard kings = piece_bb(us, PieceType::King);
  while (kings) {
    const int from = pop_lsb(kings);
    emit(from, attacks::king(from));
  }

  if (gen != MoveGen::Captures) {
    if (can_castle(us, true)) {
      const int rank = (us == Color::White) ? 0 : 7;
      out.push(Move::make(Square(rank, 4), Square(rank, 6), MoveFlag::CastleKingside));
    }
    if (can_castle(us, false)) {
      const int rank = (us == Color::White) ? 0 : 7;
      out.push(Move::make(Square(rank, 4), Square(rank, 2), MoveFlag::CastleQueenside));
    }
  }
}

MoveList Board::pseudo_legal_moves() const {
  MoveList out;
  generate_pseudo(out, MoveGen::All);
  return out;
}

MoveList Board::legal_moves() {
  MoveList pseudo, legal;
  generate_pseudo(pseudo, MoveGen::All);
  const Color us = stm_;
  for (const Move& m : pseudo) {
    make(m);
    const bool ok = !in_check(us);
    unmake();
    if (ok) legal.push(m);
  }
  return legal;
}

MoveList Board::legal_captures() {
  MoveList pseudo, legal;
  generate_pseudo(pseudo, MoveGen::Captures);
  const Color us = stm_;
  for (const Move& m : pseudo) {
    make(m);
    const bool ok = !in_check(us);
    unmake();
    if (ok) legal.push(m);
  }
  return legal;
}

bool Board::has_non_pawn_material(Color c) const {
  return (piece_bb(c, PieceType::Knight) | piece_bb(c, PieceType::Bishop) |
          piece_bb(c, PieceType::Rook) | piece_bb(c, PieceType::Queen)) != 0;
}

bool Board::is_legal(const Move& move) {
  MoveList ms = legal_moves();
  for (const Move& m : ms) {
    if (m == move) return true;
  }
  return false;
}

bool Board::gives_check(const Move& move) {
  const Color them = opposite(stm_);
  make(move);
  const bool check = in_check(them);
  unmake();
  return check;
}

void Board::make(const Move& move) {
  Undo u;
  u.move = move;
  u.captured = Piece::None;
  u.castling = castling_;
  u.ep = ep_;
  u.halfmove = halfmove_;
  u.fullmove = fullmove_;
  u.hash = hash_;
  u.white_king = white_king_;
  u.black_king = black_king_;
  u.stm = stm_;

  const Color us = stm_;
  const Color them = opposite(us);
  Piece moving = piece_at(move.from);

  if (move.flag == MoveFlag::EnPassant) {
    Square cap(move.from.rank(), move.to.file());
    u.captured = piece_at(cap);
    remove(cap);
  } else {
    u.captured = piece_at(move.to);
  }

  const bool capture = u.captured != Piece::None;
  const bool pawn_move = type_of(moving) == PieceType::Pawn;
  if (pawn_move || capture) halfmove_ = 0;
  else ++halfmove_;

  remove(move.from);

  if (move.flag == MoveFlag::Promotion) {
    put(move.to, make_piece(us, move.promotion));
  } else {
    put(move.to, moving);
  }

  if (move.flag == MoveFlag::CastleKingside) {
    const int rank = move.from.rank();
    Piece rook = piece_at(Square(rank, 7));
    remove(Square(rank, 7));
    put(Square(rank, 5), rook);
  } else if (move.flag == MoveFlag::CastleQueenside) {
    const int rank = move.from.rank();
    Piece rook = piece_at(Square(rank, 0));
    remove(Square(rank, 0));
    put(Square(rank, 3), rook);
  }

  int lost = castle_mask_lost_on_square(move.from) | castle_mask_lost_on_square(move.to);
  if (move.flag == MoveFlag::EnPassant) {
    lost |= castle_mask_lost_on_square(Square(move.from.rank(), move.to.file()));
  }
  castling_ &= ~lost;

  ep_ = Square::none_index;
  if (move.flag == MoveFlag::DoublePawn) {
    Square skip(move.from.rank() + ((us == Color::White) ? 1 : -1), move.from.file());
    ep_ = skip.index();
  }

  if (us == Color::Black) ++fullmove_;
  stm_ = them;

  rebuild_hash();
  undo_.push_back(u);
  history_.push_back(hash_);
}

void Board::make_null() {
  Undo u;
  u.move = Move{};
  u.captured = Piece::None;
  u.castling = castling_;
  u.ep = ep_;
  u.halfmove = halfmove_;
  u.fullmove = fullmove_;
  u.hash = hash_;
  u.white_king = white_king_;
  u.black_king = black_king_;
  u.stm = stm_;

  if (ep_ != Square::none_index) hash_ ^= zobrist::en_passant_file(Square(ep_).file());
  ep_ = Square::none_index;
  hash_ ^= zobrist::side_to_move();
  stm_ = opposite(stm_);

  undo_.push_back(u);
  history_.push_back(hash_);
}

void Board::unmake() {
  if (undo_.empty()) throw std::logic_error("unmake without make");
  Undo u = undo_.back();
  undo_.pop_back();
  history_.pop_back();

  stm_ = u.stm;
  castling_ = u.castling;
  ep_ = u.ep;
  halfmove_ = u.halfmove;
  fullmove_ = u.fullmove;
  hash_ = u.hash;
  white_king_ = u.white_king;
  black_king_ = u.black_king;

  if (u.move.is_null()) return;

  const Move& move = u.move;
  if (move.flag == MoveFlag::CastleKingside) {
    const int rank = move.from.rank();
    Piece rook = piece_at(Square(rank, 5));
    remove(Square(rank, 5));
    put(Square(rank, 7), rook);
  } else if (move.flag == MoveFlag::CastleQueenside) {
    const int rank = move.from.rank();
    Piece rook = piece_at(Square(rank, 3));
    remove(Square(rank, 3));
    put(Square(rank, 0), rook);
  }

  Piece moved = piece_at(move.to);
  remove(move.to);
  if (move.flag == MoveFlag::Promotion) {
    put(move.from, make_piece(stm_, PieceType::Pawn));
  } else {
    put(move.from, moved);
  }

  if (move.flag == MoveFlag::EnPassant) {
    Square cap(move.from.rank(), move.to.file());
    put(cap, u.captured);
  } else if (u.captured != Piece::None) {
    put(move.to, u.captured);
  }

  white_king_ = u.white_king;
  black_king_ = u.black_king;
  hash_ = u.hash;
}

int Board::repetition_count() const {
  if (history_.empty()) return 1;
  const std::uint64_t h = history_.back();
  int n = 0;
  for (std::uint64_t x : history_) {
    if (x == h) ++n;
  }
  return n;
}

bool Board::is_insufficient_material() const {
  int knights = 0;
  int bishops = 0;
  int light_bishops = 0;
  int dark_bishops = 0;
  for (int i = 0; i < 64; ++i) {
    PieceType t = type_of(squares_[i]);
    if (t == PieceType::None || t == PieceType::King) continue;
    if (t == PieceType::Pawn || t == PieceType::Rook || t == PieceType::Queen) return false;
    if (t == PieceType::Knight) ++knights;
    if (t == PieceType::Bishop) {
      ++bishops;
      if (((i >> 3) + (i & 7)) % 2 == 0) ++light_bishops;
      else ++dark_bishops;
    }
  }
  if (knights == 0 && bishops == 0) return true;          // K vs K
  if (knights <= 1 && bishops == 0) return true;          // K+N vs K
  if (knights == 0 && bishops == 1) return true;          // K+B vs K
  if (knights == 0 && light_bishops > 0 && dark_bishops == 0) return true;
  if (knights == 0 && dark_bishops > 0 && light_bishops == 0) return true;
  return false;
}

GameStatus Board::status() {
  GameStatus s;
  if (repetition_count() >= 3) {
    s.result = Result::Draw;
    s.draw = DrawReason::Repetition;
    return s;
  }
  if (halfmove_ >= 100) {
    s.result = Result::Draw;
    s.draw = DrawReason::FiftyMove;
    return s;
  }
  if (is_insufficient_material()) {
    s.result = Result::Draw;
    s.draw = DrawReason::Insufficient;
    return s;
  }
  MoveList ms = legal_moves();
  if (ms.empty()) {
    if (in_check()) {
      s.checkmate = true;
      s.result = (stm_ == Color::White) ? Result::BlackWin : Result::WhiteWin;
    } else {
      s.result = Result::Draw;
      s.draw = DrawReason::Stalemate;
    }
  }
  return s;
}

int Board::evaluate_white() const {
  return eval_noking_ + (queens_ == 0 ? king_eg_ : king_mg_);
}

int Board::evaluate() const {
  int s = evaluate_white();
  return stm_ == Color::White ? s : -s;
}

std::string Board::to_string(bool unicode) const {
  std::string out;
  for (int r = 7; r >= 0; --r) {
    out += static_cast<char>('1' + r);
    out += ' ';
    for (int f = 0; f < 8; ++f) {
      Piece p = piece_at(Square(r, f));
      if (unicode) out += piece_unicode(p);
      else out += chess::to_string(p);
      if (f < 7) out += ' ';
    }
    out += '\n';
  }
  out += "  a b c d e f g h\n";
  return out;
}

Board Board::from_fen(std::string_view fen) {
  Board b{EmptyTag{}};

  std::string s(fen);
  std::istringstream in(s);
  std::string placement, stm, castle, ep, half, full;
  if (!(in >> placement >> stm >> castle >> ep)) {
    throw std::invalid_argument("invalid FEN: " + s);
  }
  in >> half >> full;

  int rank = 7, file = 0;
  for (char c : placement) {
    if (c == '/') {
      if (file != 8 || rank <= 0) throw std::invalid_argument("invalid FEN ranks");
      --rank;
      file = 0;
      continue;
    }
    if (std::isdigit(static_cast<unsigned char>(c))) {
      file += c - '0';
      if (file > 8) throw std::invalid_argument("invalid FEN file overflow");
      continue;
    }
    Piece p = piece_from_fen(c);
    if (p == Piece::None || !on_board(rank, file)) {
      throw std::invalid_argument("invalid FEN piece");
    }
    b.put(Square(rank, file), p);
    ++file;
  }
  if (rank != 0 || file != 8) throw std::invalid_argument("invalid FEN board");

  if (stm == "w") b.stm_ = Color::White;
  else if (stm == "b") b.stm_ = Color::Black;
  else throw std::invalid_argument("invalid FEN side");

  b.castling_ = 0;
  if (castle != "-") {
    for (char c : castle) {
      if (c == 'K') b.castling_ |= kCastleWK;
      else if (c == 'Q') b.castling_ |= kCastleWQ;
      else if (c == 'k') b.castling_ |= kCastleBK;
      else if (c == 'q') b.castling_ |= kCastleBQ;
      else throw std::invalid_argument("invalid FEN castling");
    }
  }

  if (ep == "-") b.ep_ = Square::none_index;
  else b.ep_ = Square::from_algebraic(ep).index();

  b.halfmove_ = half.empty() ? 0 : std::stoi(half);
  b.fullmove_ = full.empty() ? 1 : std::stoi(full);
  b.find_kings();
  b.rebuild_hash();
  b.history_.push_back(b.hash_);
  return b;
}

std::string Board::fen() const {
  std::string out;
  for (int r = 7; r >= 0; --r) {
    int empty = 0;
    for (int f = 0; f < 8; ++f) {
      Piece p = piece_at(Square(r, f));
      if (p == Piece::None) {
        ++empty;
      } else {
        if (empty) out += static_cast<char>('0' + empty);
        empty = 0;
        out += piece_to_fen(p);
      }
    }
    if (empty) out += static_cast<char>('0' + empty);
    if (r) out += '/';
  }

  out += (stm_ == Color::White) ? " w " : " b ";

  std::string cr;
  if (castling_ & kCastleWK) cr += 'K';
  if (castling_ & kCastleWQ) cr += 'Q';
  if (castling_ & kCastleBK) cr += 'k';
  if (castling_ & kCastleBQ) cr += 'q';
  out += cr.empty() ? "-" : cr;

  out += ' ';
  out += (ep_ == Square::none_index) ? "-" : Square(ep_).algebraic();
  out += ' ';
  out += std::to_string(halfmove_);
  out += ' ';
  out += std::to_string(fullmove_);
  return out;
}

std::string Board::to_san(const Move& move) {
  auto suffix = [&]() -> std::string {
    make(move);
    std::string s;
    if (in_check()) {
      MoveList replies = legal_moves();
      s = replies.empty() ? "#" : "+";
    }
    unmake();
    return s;
  };

  if (move.flag == MoveFlag::CastleKingside) return "O-O" + suffix();
  if (move.flag == MoveFlag::CastleQueenside) return "O-O-O" + suffix();

  Piece moving = piece_at(move.from);
  PieceType t = type_of(moving);
  const bool capture = move.flag == MoveFlag::EnPassant || piece_at(move.to) != Piece::None;

  std::string san;
  if (t == PieceType::Pawn) {
    if (capture) {
      san += static_cast<char>('a' + move.from.file());
      san += 'x';
    }
    san += move.to.algebraic();
    if (move.flag == MoveFlag::Promotion) {
      san += '=';
      san += piece_to_fen(make_piece(Color::White, move.promotion));
    }
  } else {
    san += piece_to_fen(make_piece(Color::White, t));

    MoveList others = legal_moves();
    bool file_clash = false, rank_clash = false, any = false;
    for (const Move& m : others) {
      if (m.to != move.to || m.from == move.from) continue;
      if (type_of(piece_at(m.from)) != t) continue;
      any = true;
      if (m.from.file() == move.from.file()) file_clash = true;
      if (m.from.rank() == move.from.rank()) rank_clash = true;
    }
    if (any) {
      if (!file_clash) san += static_cast<char>('a' + move.from.file());
      else if (!rank_clash) san += static_cast<char>('1' + move.from.rank());
      else {
        san += static_cast<char>('a' + move.from.file());
        san += static_cast<char>('1' + move.from.rank());
      }
    }
    if (capture) san += 'x';
    san += move.to.algebraic();
  }
  return san + suffix();
}

Move Board::parse_uci(std::string_view uci) {
  std::string s(uci);
  if (s.size() < 4) throw std::invalid_argument("invalid UCI move");
  Square from = Square::from_algebraic(s.substr(0, 2));
  Square to = Square::from_algebraic(s.substr(2, 2));
  PieceType promo = PieceType::None;
  if (s.size() >= 5) {
    switch (s[4]) {
      case 'q': case 'Q': promo = PieceType::Queen; break;
      case 'r': case 'R': promo = PieceType::Rook; break;
      case 'b': case 'B': promo = PieceType::Bishop; break;
      case 'n': case 'N': promo = PieceType::Knight; break;
      default: throw std::invalid_argument("invalid UCI promotion");
    }
  }
  MoveList ms = legal_moves();
  for (const Move& m : ms) {
    if (m.from == from && m.to == to && m.promotion == promo) return m;
  }
  throw std::invalid_argument("illegal UCI move: " + s);
}

Move Board::parse_san(std::string_view san) {
  std::string want(san);
  while (!want.empty() && (want.back() == '+' || want.back() == '#' || want.back() == '!' || want.back() == '?')) {
    want.pop_back();
  }
  // strip "e.p." / "ep"
  auto strip = [&](const std::string& token) {
    auto pos = want.find(token);
    if (pos != std::string::npos) want.erase(pos, token.size());
  };
  strip(" e.p.");
  strip("e.p.");
  strip(" ep");

  MoveList ms = legal_moves();
  Move found{};
  int matches = 0;
  for (const Move& m : ms) {
    std::string got = to_san(m);
    while (!got.empty() && (got.back() == '+' || got.back() == '#')) got.pop_back();
    if (got == want) {
      found = m;
      ++matches;
    }
  }
  if (matches != 1) {
    throw std::invalid_argument("cannot parse SAN: " + std::string(san));
  }
  return found;
}

}  // namespace chess
