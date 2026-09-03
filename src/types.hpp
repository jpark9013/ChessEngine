#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace chess {

enum class Color : std::uint8_t { White = 0, Black = 1 };

inline Color opposite(Color c) {
  return static_cast<Color>(static_cast<int>(c) ^ 1);
}

inline const char* to_string(Color c) {
  return c == Color::White ? "white" : "black";
}

enum class PieceType : std::uint8_t {
  None = 0,
  Pawn,
  Knight,
  Bishop,
  Rook,
  Queen,
  King
};

inline const char* to_string(PieceType t) {
  switch (t) {
    case PieceType::Pawn: return "pawn";
    case PieceType::Knight: return "knight";
    case PieceType::Bishop: return "bishop";
    case PieceType::Rook: return "rook";
    case PieceType::Queen: return "queen";
    case PieceType::King: return "king";
    default: return "none";
  }
}

enum class Piece : std::uint8_t {
  None = 0,
  WPawn, WKnight, WBishop, WRook, WQueen, WKing,
  BPawn, BKnight, BBishop, BRook, BQueen, BKing
};

inline Color color_of(Piece p) {
  return p <= Piece::WKing ? Color::White : Color::Black;
}

inline PieceType type_of(Piece p) {
  if (p == Piece::None) return PieceType::None;
  int i = static_cast<int>(p);
  if (i <= 6) return static_cast<PieceType>(i);
  return static_cast<PieceType>(i - 6);
}

inline Piece make_piece(Color c, PieceType t) {
  if (t == PieceType::None) return Piece::None;
  int base = (c == Color::White) ? 0 : 6;
  return static_cast<Piece>(base + static_cast<int>(t));
}

inline char piece_to_fen(Piece p) {
  switch (p) {
    case Piece::WPawn: return 'P';
    case Piece::WKnight: return 'N';
    case Piece::WBishop: return 'B';
    case Piece::WRook: return 'R';
    case Piece::WQueen: return 'Q';
    case Piece::WKing: return 'K';
    case Piece::BPawn: return 'p';
    case Piece::BKnight: return 'n';
    case Piece::BBishop: return 'b';
    case Piece::BRook: return 'r';
    case Piece::BQueen: return 'q';
    case Piece::BKing: return 'k';
    default: return ' ';
  }
}

inline Piece piece_from_fen(char c) {
  switch (c) {
    case 'P': return Piece::WPawn;
    case 'N': return Piece::WKnight;
    case 'B': return Piece::WBishop;
    case 'R': return Piece::WRook;
    case 'Q': return Piece::WQueen;
    case 'K': return Piece::WKing;
    case 'p': return Piece::BPawn;
    case 'n': return Piece::BKnight;
    case 'b': return Piece::BBishop;
    case 'r': return Piece::BRook;
    case 'q': return Piece::BQueen;
    case 'k': return Piece::BKing;
    default: return Piece::None;
  }
}

inline std::string piece_unicode(Piece p) {
  switch (p) {
    case Piece::WKing: return "♔";
    case Piece::WQueen: return "♕";
    case Piece::WRook: return "♖";
    case Piece::WBishop: return "♗";
    case Piece::WKnight: return "♘";
    case Piece::WPawn: return "♙";
    case Piece::BKing: return "♚";
    case Piece::BQueen: return "♛";
    case Piece::BRook: return "♜";
    case Piece::BBishop: return "♝";
    case Piece::BKnight: return "♞";
    case Piece::BPawn: return "♟";
    default: return ".";
  }
}

inline std::string to_string(Piece p) {
  char c = piece_to_fen(p);
  return c == ' ' ? std::string(".") : std::string(1, c);
}

constexpr int kCastleWK = 1;
constexpr int kCastleWQ = 2;
constexpr int kCastleBK = 4;
constexpr int kCastleBQ = 8;

class Square {
 public:
  static constexpr int none_index = 64;

  constexpr Square() : index_(none_index) {}
  explicit constexpr Square(int index) : index_(index) {}
  constexpr Square(int rank, int file) : index_(rank * 8 + file) {}

  static Square from_algebraic(std::string_view s) {
    if (s.size() != 2 || s[0] < 'a' || s[0] > 'h' || s[1] < '1' || s[1] > '8') {
      throw std::invalid_argument("invalid square: " + std::string(s));
    }
    return Square(s[1] - '1', s[0] - 'a');
  }

  constexpr int index() const { return index_; }
  constexpr int rank() const { return index_ >> 3; }
  constexpr int file() const { return index_ & 7; }
  constexpr bool valid() const { return index_ >= 0 && index_ < 64; }

  std::string algebraic() const {
    if (!valid()) return "-";
    return {static_cast<char>('a' + file()), static_cast<char>('1' + rank())};
  }

  friend constexpr bool operator==(Square a, Square b) { return a.index_ == b.index_; }
  friend constexpr bool operator!=(Square a, Square b) { return !(a == b); }

 private:
  int index_;
};

inline constexpr bool on_board(int rank, int file) {
  return rank >= 0 && rank < 8 && file >= 0 && file < 8;
}

enum class MoveFlag : std::uint8_t {
  Normal = 0,
  DoublePawn,
  EnPassant,
  CastleKingside,
  CastleQueenside,
  Promotion
};

struct Move {
  Square from{};
  Square to{};
  PieceType promotion = PieceType::None;
  MoveFlag flag = MoveFlag::Normal;

  static Move make(Square from, Square to,
                   MoveFlag flag = MoveFlag::Normal,
                   PieceType promotion = PieceType::None) {
    return Move{from, to, promotion, flag};
  }

  bool is_null() const { return !from.valid() || !to.valid(); }

  std::string uci() const {
    if (is_null()) return "0000";
    std::string s = from.algebraic() + to.algebraic();
    if (promotion != PieceType::None) {
      switch (promotion) {
        case PieceType::Queen: s += 'q'; break;
        case PieceType::Rook: s += 'r'; break;
        case PieceType::Bishop: s += 'b'; break;
        case PieceType::Knight: s += 'n'; break;
        default: break;
      }
    }
    return s;
  }
};

inline bool operator==(const Move& a, const Move& b) {
  return a.from == b.from && a.to == b.to && a.promotion == b.promotion && a.flag == b.flag;
}

inline bool operator!=(const Move& a, const Move& b) { return !(a == b); }

class MoveList {
 public:
  static constexpr int kMax = 256;

  void push(Move m) { moves_[size_++] = m; }
  int size() const { return size_; }
  bool empty() const { return size_ == 0; }
  void clear() { size_ = 0; }

  Move& operator[](int i) { return moves_[i]; }
  const Move& operator[](int i) const { return moves_[i]; }

  Move* begin() { return moves_; }
  Move* end() { return moves_ + size_; }
  const Move* begin() const { return moves_; }
  const Move* end() const { return moves_ + size_; }

  std::vector<Move> to_vector() const { return {begin(), end()}; }

 private:
  Move moves_[kMax]{};
  int size_ = 0;
};

enum class Result {
  Ongoing,
  WhiteWin,
  BlackWin,
  Draw
};

enum class DrawReason {
  None,
  Stalemate,
  FiftyMove,
  Repetition,
  Insufficient
};

struct GameStatus {
  Result result = Result::Ongoing;
  DrawReason draw = DrawReason::None;
  bool checkmate = false;
};

inline const char* to_string(Result r) {
  switch (r) {
    case Result::WhiteWin: return "white";
    case Result::BlackWin: return "black";
    case Result::Draw: return "draw";
    default: return "ongoing";
  }
}

inline constexpr int kMateScore = 100000;
inline constexpr int kInfScore = 1000000;

}  // namespace chess
