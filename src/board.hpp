#pragma once

#include "bitboard.hpp"
#include "types.hpp"

#include <array>
#include <cstdint>
#include <ostream>
#include <string_view>
#include <vector>

namespace chess {

struct Undo {
  Move move{};
  Piece captured = Piece::None;
  int castling = 0;
  int ep = Square::none_index;
  int halfmove = 0;
  int fullmove = 0;
  std::uint64_t hash = 0;
  Square white_king{};
  Square black_king{};
  Color stm = Color::White;
};

class Board {
 public:
  Board();

  static Board from_fen(std::string_view fen);
  std::string fen() const;

  Piece piece_at(Square sq) const { return squares_[sq.index()]; }
  Color side_to_move() const { return stm_; }
  int castling_rights() const { return castling_; }
  Square ep_square() const { return Square(ep_); }
  int halfmove_clock() const { return halfmove_; }
  int fullmove_number() const { return fullmove_; }
  Square king_square(Color c) const { return c == Color::White ? white_king_ : black_king_; }
  std::uint64_t hash() const { return hash_; }
  int ply_from_root() const { return static_cast<int>(undo_.size()); }

  void set_piece(Square sq, Piece p);
  void clear();
  void startpos();

  void make(const Move& move);
  void unmake();

  bool is_attacked(Square sq, Color by) const;
  bool in_check() const { return in_check(stm_); }
  bool in_check(Color c) const { return is_attacked(king_square(c), opposite(c)); }

  MoveList legal_moves();
  MoveList legal_captures();
  MoveList pseudo_legal_moves() const;

  bool is_legal(const Move& move);
  bool gives_check(const Move& move);

  GameStatus status();
  bool is_insufficient_material() const;
  int repetition_count() const;

  int evaluate() const;       // from side-to-move
  int evaluate_white() const; // white-positive centipawns

  std::string to_string(bool unicode = false) const;
  std::string to_san(const Move& move);
  Move parse_uci(std::string_view uci);
  Move parse_san(std::string_view san);

  const std::array<Piece, 64>& squares() const { return squares_; }

 private:
  struct EmptyTag {};
  explicit Board(EmptyTag) { clear(); }

  void put(Square sq, Piece p);
  void remove(Square sq);
  void rebuild_hash();
  void find_kings();
  void generate_pseudo(MoveList& out, bool captures_only) const;
  void add_pawn_moves(MoveList& out, Square from, bool captures_only) const;
  bool can_castle(Color c, bool kingside) const;
  Bitboard piece_bb(Color c, PieceType t) const;
  Bitboard occupancy() const { return occupancy_[0] | occupancy_[1]; }

  std::array<Piece, 64> squares_{};
  std::array<Bitboard, 12> pieces_{};
  std::array<Bitboard, 2> occupancy_{};
  Color stm_ = Color::White;
  int castling_ = kCastleWK | kCastleWQ | kCastleBK | kCastleBQ;
  int ep_ = Square::none_index;
  int halfmove_ = 0;
  int fullmove_ = 1;
  std::uint64_t hash_ = 0;
  Square white_king_{0, 4};
  Square black_king_{7, 4};
  std::vector<Undo> undo_;
  std::vector<std::uint64_t> history_;
};

inline std::ostream& operator<<(std::ostream& os, const Board& b) {
  return os << b.to_string();
}

}  // namespace chess
