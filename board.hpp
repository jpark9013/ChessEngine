#ifndef ADSCHESS_BOARD_HPP
#define ADSCHESS_BOARD_HPP

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <chrono>
#include <cmath>
#include <deque>
#include <functional>
#include <memory>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

namespace Chess {

struct Coord;
class Move;
class Settings;

class Timer;

class Board;

enum Piece {
  Empty,
  WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing,
  BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing,
  Placeholder
};

enum class Side {
  White, Black, Empty, Neither
};

enum class Winner {
  White, Black, Neither, Tie
};

std::string to_string(const Coord &coord);

std::string to_string(const Move &move);

std::string to_string(const Settings &settings);

std::string to_string(const Board &board);

std::string to_string(Piece piece);

std::string to_string(Side side);

std::string to_string(Winner winner);

// https://www.chessprogramming.org/Simplified_Evaluation_Function#Piece_Values
std::array<int, 14> piece_values = {{
                                        0,
                                        100, 320, 330, 500, 900, 0,
                                        -100, -320, -330, -500, -900, 0,
                                        0
                                    }};

using small = std::uint8_t;
using i64 = std::uint64_t;

struct Coord {
  // 0 -> (1 << 8) - 1
  small i = 0, j = 0;

  Coord() = default;

  Coord(small _i, small _j) : i(_i), j(_j) {}

  friend bool operator== (const Coord &a, const Coord &b) {
    return a.i == b.i && a.j == b.j;
  }

  friend bool operator!= (const Coord &a, const Coord &b) {
    return !(a == b);
  }
};

std::string to_string(const Coord &coord) {
  return "(" + std::to_string(coord.i) + ", " + std::to_string(coord.j) + ")";
}

class Move {

 private:

  static constexpr small SHIFT = 1 << 3;

  // get num in a zone to the left of ind 3
  static inline small zone(small x) {
    static constexpr small a = std::numeric_limits<small>::max() - (1 << 3) + 1;
    return (x & a) >> 3;
  }

  // set all bits in zone to 0 left of 3
  static inline void zero(small &x) {
    x &= ((1 << 3) - 1);
  }

 public:

  // strictly where the piece is from, and where it will go
  Coord from, to;

  Move() = default;

  Move(const Coord &_from, const Coord &_to) : from(_from), to(_to) {}

  Move(small a, small b, small c, small d) : from(Coord(a, b)), to(Coord(c, d)) {}

  Move(small a, small b, small c, small d, Piece p) : from(Coord(a, b)), to(Coord(a, b)) {
    set_promotion(p);
  }

  Move(small a, small b, small c, small d, small e, small f) : from(Coord(a, b)), to(Coord(a, b)) {
    set_en_passant(Coord(e, f));
  }

  // p -> 0 ... 15
  // so 4 bits needed
  // 3 bits needed to encode original move
  // so bitshift left by 3
  void set_promotion(Piece p) {
    to.i |= SHIFT;
    from.i |= (static_cast<small>(p) << 3);
  }

  void set_en_passant(const Coord &c) {
    to.j |= SHIFT;
    from.i |= (c.i << 3);
    from.j |= (c.j << 3);
  }

  // returns the Piece
  // and removes the set bit
  Piece unpack_promotion() {
    to.i ^= SHIFT;
    Piece p = static_cast<Piece>(zone(from.i));
    zero(from.i);
    return p;
  }

  // returns the square of the pawn that was captured by en passant
  // and removes the set bit
  Coord unpack_en_passant() {
    to.j ^= SHIFT;
    Coord c = {zone(to.i), zone(to.j)};
    zero(to.i);
    zero(to.j);
    return c;
  }

  bool is_promotion() const {
    return to.i & SHIFT;
  }

  bool is_en_passant() const {
    return to.j & SHIFT;
  }

  friend bool operator== (const Move &a, const Move &b) {
    return a.from == b.from && a.to == b.to;
  }

  friend bool operator!= (const Move &a, const Move &b) {
    return !(a == b);
  }
};

std::string to_string(const Move &move) {
  return "[" + to_string(move.from) + ", " + to_string(move.to) + "]";
}

Side piece_side(Piece piece) {
  switch (piece) {
    case WhitePawn:
    case WhiteKnight:
    case WhiteBishop:
    case WhiteRook:
    case WhiteQueen:
    case WhiteKing:
      return Side::White;
    case BlackPawn:
    case BlackKnight:
    case BlackBishop:
    case BlackRook:
    case BlackQueen:
    case BlackKing:
      return Side::Black;
    case Empty:
      return Side::Empty;
    default:
      return Side::Neither;
  }
}

bool is_empty(Piece piece) {
  return piece == Empty;
}

bool is_pawn(Piece piece) {
  return piece == WhitePawn || piece == BlackPawn;
}

bool is_knight(Piece piece) {
  return piece == WhiteKnight || piece == BlackKnight;
}

bool is_bishop(Piece piece) {
  return piece == WhiteBishop || piece == BlackBishop;
}

bool is_rook(Piece piece) {
  return piece == WhiteRook || piece == BlackRook;
}

bool is_queen(Piece piece) {
  return piece == WhiteQueen || piece == BlackQueen;
}

bool is_king(Piece piece) {
  return piece == WhiteKing || piece == BlackKing;
}

Side opp_side(Side side) {
  return side == Side::Black ? Side::White : Side::Black;
}

Piece get_pawn(Side side) {
  return side == Side::White ? WhitePawn : BlackPawn;
}

Piece get_knight(Side side) {
  return side == Side::White ? WhiteKnight : BlackKnight;
}

Piece get_bishop(Side side) {
  return side == Side::White ? WhiteBishop : BlackBishop;
}

Piece get_rook(Side side) {
  return side == Side::White ? WhiteRook : BlackRook;
}

Piece get_queen(Side side) {
  return side == Side::White ? WhiteQueen : BlackQueen;
}

Piece get_king(Side side) {
  return side == Side::White ? WhiteKing : BlackKing;
}

Piece get_opp_piece(Piece piece) {
  switch (piece) {
    case WhitePawn:
      return BlackPawn;
    case BlackPawn:
      return WhitePawn;
    case WhiteKnight:
      return BlackKnight;
    case BlackKnight:
      return WhiteKnight;
    case WhiteRook:
      return BlackRook;
    case BlackRook:
      return WhiteRook;
    case WhiteQueen:
      return BlackQueen;
    case BlackQueen:
      return WhiteQueen;
    case WhiteKing:
      return BlackKing;
    case BlackKing:
      return WhiteKing;
    default:
      assert(false);
  }
}

int opp(int x) {
  return 7 - x;
}

bool is_same_side(Piece a, Piece b) {
  return piece_side(a) == piece_side(b);
}

// returns false if one is empty
bool is_opp_side(Piece a, Piece b) {
  Side c = piece_side(a), d = piece_side(b);
  return (c == Side::White && d == Side::Black) || (c == Side::Black && d == Side::White);
}

bool is_output_complex = true;

void set_output_complex(bool b) {
  is_output_complex = b;
}

std::string to_string(Piece piece) {
  if (is_output_complex) {
    switch (piece) {
      case WhiteKing:
        return "♔";
      case WhiteQueen:
        return "♕";
      case WhiteRook:
        return "♖";
      case WhiteBishop:
        return "♗";
      case WhiteKnight:
        return "♘";
      case WhitePawn:
        return "♙";
      case BlackKing:
        return "♚";
      case BlackQueen:
        return "♛";
      case BlackRook:
        return "♜";
      case BlackBishop:
        return "♝";
      case BlackKnight:
        return "♞";
      case BlackPawn:
        return "♟︎";
      case Empty:
        return ".";
      default:
        assert(false);
        return "Impossible";
    }
  } else {
    switch (piece) {
      case WhiteKing:
        return "K";
      case Empty:
        return ".";
      case WhitePawn:
        return "P";
      case WhiteKnight:
        return "N";
      case WhiteBishop:
        return "B";
      case WhiteRook:
        return "R";
      case WhiteQueen:
        return "Q";
      case BlackPawn:
        return "p";
      case BlackKnight:
        return "n";
      case BlackBishop:
        return "b";
      case BlackRook:
        return "r";
      case BlackQueen:
        return "q";
      case BlackKing:
        return "k";
      default:
        assert(false);
        return "Impossible";
    }
  }
};

// only for move notation (Nd5 etc)
std::string to_notation(Piece piece) {
  switch (piece) {
    case WhiteKing:
    case BlackKing:
      return "K";
    case WhiteQueen:
    case BlackQueen:
      return "Q";
    case WhiteRook:
    case BlackRook:
      return "R";
    case WhiteBishop:
    case BlackBishop:
      return "B";
    case WhiteKnight:
    case BlackKnight:
      return "N";
    case WhitePawn:
    case BlackPawn:
      return "";
    default:
      assert(false);
      return "Impossible";
  }
};

std::string to_basic_str(Piece piece) {
  switch (piece) {
    case WhiteKing:
      return "K";
    case WhiteQueen:
      return "Q";
    case WhiteRook:
      return "R";
    case WhiteBishop:
      return "B";
    case WhiteKnight:
      return "N";
    case WhitePawn:
      return "P";
    case BlackKing:
      return "k";
    case BlackQueen:
      return "q";
    case BlackRook:
      return "r";
    case BlackBishop:
      return "b";
    case BlackKnight:
      return "n";
    case BlackPawn:
      return "p";
    case Empty:
      return ".";
    default:
      assert(false);
      return "Impossible";
  }
}

Piece to_piece(char c) {
  switch (c) {
    case ' ': return Empty;
    case 'K': return WhiteKing;
    case 'Q': return WhiteQueen;
    case 'R': return WhiteRook;
    case 'N': return WhiteKnight;
    case 'P': return WhitePawn;
    case 'k': return BlackKing;
    case 'q': return BlackQueen;
    case 'r': return BlackRook;
    case 'b': return BlackBishop;
    case 'n': return BlackKnight;
    case 'p': return BlackPawn;
    default: return Placeholder;
  }
}

Piece to_piece(const std::string &str) {
  assert(str.size() == 1);
  return to_piece(str[0]);
}

int piece_val(Piece piece) {
  return piece_values[static_cast<int>(piece)];
}

namespace {

const std::vector<int> king_moves_i = {-1, -1, -1, 0, 0, 1, 1, 1}, king_moves_j = {-1, 0, 1, -1, 1, -1, 0, 1};
const std::vector<int> knight_moves_i = {-2, -1, -2, 1, 1, 2, 1, 2}, knight_moves_j = {-1, -2, 1, 2, -2, -1, 2, 1};

}  // anon namespace

constexpr int h = 8, w = 8;

class Settings {

 private:

  i64 mask = (i64(1) << 49) + (i64(1) << 50);

  static inline i64 ones(short l, short r) {
    return (i64(1) << (r + 1)) - (i64(1) << l);
  }

  // mask = 0x10110101111
  // get_short(0, 4) should return 0x01111 = 15
  template<typename T>
  inline T get(short l, short r) const {
    return (mask & ones(l, r)) >> l;
  }

  inline void set(short l, short r, i64 x) {
    // first reset everything within l ... r
    mask &= ~ones(l, r);
    // then apply x
    mask |= (x << l);
  }

  Move lm = Move(0, 0, 0, 0);

 public:

  // getter and setter
#define gs(name, T, l, r) T name() const {return get<T>(l, r);} void set_##name(T x) {set(l, r, x);}

  gs(white_rook_queen_moved, bool, 0, 0)
  gs(black_rook_queen_moved, bool, 1, 1)
  gs(white_rook_king_moved, bool, 2, 2)
  gs(black_rook_king_moved, bool, 3, 3)
  gs(wp_recently_moved, small, 4, 7)
  gs(bp_recently_moved, small, 8, 11)
  gs(num_moves, int, 12, 19)
  gs(num_half_moves, int, 20, 27)
  gs(moves_since_capture, int, 28, 35)
  gs(wq_exists, bool, 58, 58)
  gs(bq_exists, bool, 59, 59)

#undef gs

  void increment_moves() {
    set_num_moves(num_moves() + 1);
  }

  void increment_half_moves() {
    set_num_half_moves(num_half_moves() + 1);
  }

  void increment_moves_since_capture() {
    set_moves_since_capture(moves_since_capture() + 1);
  }

  void flip_turn() {
    set_turn(get<bool>(36, 36) ? Side::White : Side::Black);
  }

  Side turn() const {
    // 1 -> Black, 0 -> White cuz 0 is default
    return get<bool>(36, 36) ? Side::Black : Side::White;
  }
  void set_turn(Side s) {
    set(36, 36, static_cast<i64>(s));
  }

  Piece last_captured() const {
    return static_cast<Piece>(get<int>(37, 40));
  }
  void set_last_captured(Piece piece) {
    set(37, 40, static_cast<i64>(piece));
  }

  Move last_move() const {
    return lm;
  }

  void set_last_move(const Move &m) {
    lm = m;
  }

  int eval() const {
    return get<int>(41, 57) - 25600;
  }

  void set_eval(int x) {
    set(41, 57, x + 25600);
  }

  Settings() {
    set_wq_exists(true);
    set_bq_exists(true);
    set_wp_recently_moved(8);
    set_bp_recently_moved(8);
    set_eval(0);
  }

  explicit Settings(i64 x, const Move &m) : mask(x), lm(m) {

  }
};

std::string to_string(const Settings &s) {
  std::string ret;

  auto add_bool = [&](const std::string &a, bool b) -> void {
    ret += a + ": ";
    ret += b ? "True\n" : "False\n";
  };

  auto add_int = [&](const std::string &a, int b) -> void {
    ret += a + ": ";
    ret += std::to_string(b) + "\n";
  };

  auto add_side = [&](const std::string &a, Side side) -> void {
    ret += a + ": ";
    ret += side == Side::White ? "White\n" : "Black\n";
  };

  auto add_piece = [&](const std::string &a, Piece piece) -> void {
    ret += a + ": ";
    ret += piece == Empty ? "Empty\n" : to_string(piece) + "\n";
  };

  auto add_move = [&](const std::string &a, const Move &m) -> void {
    ret += a + ": ";
    ret += to_string(m) + "\n";
  };;

  add_bool("White queenside Rook moved", s.white_rook_queen_moved());
  add_bool("Black queenside Rook moved", s.black_rook_queen_moved());
  add_bool("White kingside Rook moved", s.white_rook_king_moved());
  add_bool("Black kingside Rook moved", s.black_rook_king_moved());

  add_int("White pawn that recently moved", s.wp_recently_moved());
  add_int("Black pawn that recently moved", s.bp_recently_moved());
  add_int("Number of half moves", s.num_half_moves());
  add_int("Number of moves in the game", s.num_moves());
  add_int("Number of moves since last capture", s.moves_since_capture());

  add_side("Current side to move", s.turn());

  add_move("Last move made", s.last_move());
  add_piece("Last piece captured", s.last_captured());

  add_int("Eval", s.eval());
  add_bool("White queen exists", s.wq_exists());
  add_bool("Black queen exists", s.bq_exists());

  return ret;
}

class Timer {

 private:

  std::chrono::time_point<std::chrono::steady_clock> _begin, _end;
  bool started = false;
  double elapsed_time = 0;

 public:

  Timer() = default;

  void begin() {
    _begin = std::chrono::steady_clock::now();
    started = true;
  }

  void stop() {
    assert(started);
    started = false;
    _end = std::chrono::steady_clock::now();
    elapsed_time = double(std::chrono::duration_cast<std::chrono::microseconds>(_end - _begin).count()) / 1000000.0;
  }

  // returns elapsed in seconds
  double elapsed() {
    if (started) {
      // hasn't stopped yet
      _end = std::chrono::steady_clock::now();
    }
    elapsed_time = double(std::chrono::duration_cast<std::chrono::microseconds>(_end - _begin).count()) / 1000000.0;
    return elapsed_time;
  }

};

namespace {

// https://www.chessprogramming.org/Simplified_Evaluation_Function
int ps_table[8][8][8] = {
    // Empty
    {{0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0},
     {0,   0,   0,  0,   0,   0,  0,   0},
     {0,   0,   0,   0,   0,   0,   0,   0}},
    // Pawn
    {{0, 0, 0, 0, 0, 0, 0, 0},
     {50, 50, 50, 50, 50, 50, 50, 50},
     {10, 10, 20, 30, 30, 20, 10, 10},
     {5, 5, 10, 25, 25, 10, 5, 5},
     {0, 0, 0, 20, 20, 0,           0,   0},
     {5,   -5,  -10, 0,   0,   -10, -5,  5},
     {5,   10,  10, -20, -20, 10, 10,  5},
     {0,   0,   0,   0,   0,   0,   0,   0}},
    // Knight
    {{-50, -40, -30, -30, -30, -30, -40, -50},
     {-40, -20, 0,   0,   0,   0,   -20, -40},
     {-30, 0,   10,  15,  15,  10,  0,   -30},
     {-30, 5,   15,  20,  20,  15,  5,   -30},
     {-30, 0,   15,  20,  20,  15,  0,   -30},
     {-30, 5,   10,  15,  15,  10,  5,   -30},
     {-40, -20, 0,  5,   5,   0,  -20, -40},
     {-50, -40, -30, -30, -30, -30, -40, -50}},
    // Bishop
    {{-20, -10, -10, -10, -10, -10, -10, -20},
     {-10, 0,   0,   0,   0,   0,   0,   -10},
     {-10, 0,   5,   10,  10,  5,   0,   -10},
     {-10, 5,   5,   10,  10,  5,   5,   -10},
     {-10, 0,   10,  10,  10,  10,  0,   -10},
     {-10, 10,  10,  10,  10,  10,  10,  -10},
     {-10, 5,   0,  0,   0,   0,  5,   -10},
     {-20, -10, -10, -10, -10, -10, -10, -20}},
    // Rook
    {{0,   0,   0,   0,   0,   0,   0,   0},
     {5,   10,  10,  10,  10,  10,  10,  5},
     {-5,  0,   0,   0,   0,   0,   0,   -5},
     {-5,  0,   0,   0,   0,   0,   0,   -5},
     {-5,  0,   0,   0,   0,   0,   0,   -5},
     {-5,  0,   0,   0,   0,   0,   0,   -5},
     {-5,  0,   0,  0,   0,   0,  0,   -5},
     {0,   0,   0,   5,   5,   0,   0,   0}},
    // Queen
    {{-20, -10, -10, -5,  -5,  -10, -10, -20},
     {-10, 0,   0,   0,   0,   0,   0,   -10},
     {-10, 0,   5,   5,   5,   5,   0,   -10},
     {-5,  0,   5,   5,   5,   5,   0,   -5},
     {0,   0,   5,   5,   5,   5,   0,   -5},
     {-10, 5,   5,   5,   5,   5,   0,   -10},
     {-10, 0,   5,  0,   0,   0,  0,   -10},
     {-20, -10, -10, -5,  -5,  -10, -10, -20}},
    // King before endgame
    {{-30, -40, -40, -50, -50, -40, -40, -30},
     {-30, -40, -40, -50, -50, -40, -40, -30},
     {-30, -40, -40, -50, -50, -40, -40, -30},
     {-30, -40, -40, -50, -50, -40, -40, -30},
     {-20, -30, -30, -40, -40, -30, -30, -20},
     {-10, -20, -20, -20, -20, -20, -20, -10},
     {20,  20,  0,  0,   0,   0,  20,  20},
     {20,  30,  10,  0,   0,   10,  30,  20}},
    {{-50, -40, -30, -20, -20, -30, -40, -50},
     {-30, -20, -10, 0,   0,   -10, -20, -30},
     {-30, -10, 20,  30,  30,  20,  -10, -30},
     {-30, -10, 30,  40,  40,  30,  -10, -30},
     {-30, -10, 30,  40,  40,  30,  -10, -30},
     {-30, -10, 20,  30,  30,  20,  -10, -30},
     {-30, -30, 0,  0,   0,   0,  -30, -30},
     {-50, -30, -30, -30, -30, -30, -30, -50}}
};

}  // anonymous namespace

class Board {

 private:

  std::array<std::array<Piece, w>, h> board = {{
                                                   {WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen, WhiteKing,
                                                    WhiteBishop, WhiteKnight, WhiteRook},
                                                   {WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn,
                                                    WhitePawn, WhitePawn},
                                                   {Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty},
                                                   {Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty},
                                                   {Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty},
                                                   {Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty},
                                                   {BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn,
                                                    BlackPawn, BlackPawn},
                                                   {BlackRook, BlackKnight, BlackBishop, BlackQueen, BlackKing,
                                                    BlackBishop, BlackKnight, BlackRook}
                                               }};

  Settings settings;  // current settings, one ahead of past_settings.back()
  std::vector<Settings> past_settings;
  bool main_game = false;

  void undo() {
    assert(!past_settings.empty());
    Move lm = settings.last_move();
    if (lm.is_en_passant()) {
      // need to put other side pawn where it was too
      Coord c = lm.unpack_en_passant();
      board[c.i][c.j] = settings.last_captured();
      board[lm.from.i][lm.from.j] = get_pawn(opp_side(settings.turn()));
      board[lm.to.i][lm.to.j] = Empty;
    } else if (lm.is_promotion()) {
      // Pawn on opp side
      Piece p = get_pawn(opp_side(settings.turn()));
      board[lm.from.i][lm.from.j] = p;
      board[lm.to.i][lm.to.j] = settings.last_captured();
    } else {
      board[lm.from.i][lm.from.j] = board[lm.to.i][lm.to.j];
      board[lm.to.i][lm.to.j] = settings.last_captured();
      // deal with castling
      Piece piece = board[lm.from.i][lm.from.j];
      if (is_king(piece)) {
        Piece rook = lm.from.i == 0 ? WhiteRook : BlackRook;
        if (lm.to.j - lm.from.j == 2) {
          // kingside
          board[lm.to.i][5] = Empty;
          board[lm.from.i][7] = rook;
        } else if (lm.from.j - lm.to.j == 2) {
          // queenside
          board[lm.to.i][3] = Empty;
          board[lm.from.i][0] = rook;
        }
      }
    }
    settings = past_settings.back();
    past_settings.pop_back();
  }

  static bool valid(int a, int b) {
    return a >= 0 && a < h && b >= 0 && b < w;
  }

  Timer timer;

  bool is_capture(Move m) {
    if (m.is_promotion()) {
      m.unpack_promotion();
    }
    return m.is_en_passant() || board[m.to.i][m.to.j] != Empty;
  }

  std::shared_ptr<Board> copy() {
    return std::shared_ptr<Board>(std::make_shared<Board>(board, settings));
  }

  [[nodiscard]] bool pos_check(const Move &move, Side side = Side::Neither) const {
    if (side == Side::Neither) {
      side = settings.turn();
    }
    return new_move(move).in_check(side);
  }

  int DEPTH = 8;
  int sm = -1;

  // credit: sebastian bach youtube video for how it works
  // also credit: https://cse-robotics.engr.tamu.edu/dshell/cs420/asgn2/ for the pseudocode
  int ab_search(const std::shared_ptr<Board> &b, int depth, int alpha, int beta) {
    if (depth == DEPTH) {
      return b->static_evaluate();
    }
    bool white = b->side_turn() == Side::White;
    assert(depth < DEPTH);
    const int INF = 1e7;
    Winner wn = b->winner();
    if (wn == Winner::White) {
      return INF;
    } else if (wn == Winner::Black) {
      return -INF;
    } else if (wn == Winner::Tie) {
      return 0;
    }
    depth++;
    std::vector<Move> mvs = b->moves();
    if (white) {
      int max_eval = -INF;
      for (const Move &m : mvs) {
        b->move(m);
        int ev = ab_search(b, depth, alpha, beta);
        max_eval = std::max(max_eval, ev);
        if (max_eval > beta) {
          return max_eval;
        }
        alpha = std::max(alpha, max_eval);
        b->undo();
      }
      return max_eval;
    } else {
      int min_eval = INF;
      for (const Move &m : mvs) {
        b->move(m);
        int ev = ab_search(b, depth, alpha, beta);
        min_eval = std::min(min_eval, ev);
        if (min_eval < alpha) {
          return min_eval;
        }
        beta = std::min(beta, min_eval);
        b->undo();
      }
      return min_eval;
    }
  }

  // https://stackoverflow.com/questions/65122476/slow-chess-bot-need-to-go-faster
  int negamax(const std::shared_ptr<Board> &b, int depth, int alpha, int beta) {
    if (depth == DEPTH) {
      return b->static_evaluate();
    }
    static constexpr int INF = 1e8;
    Winner wn = b->winner();
    if (wn == Winner::White) {
      return INF;
    } else if (wn == Winner::Black) {
      return -INF;
    } else if (wn == Winner::Tie) {
      return 0;
    }
    int ev = -INF;
    // std::vector<std::shared_ptr<Board>> boards = b->children();
    std::vector<Move> mvs = moves();
    assert(!mvs.empty());
    for (const Move &m : mvs) {
      b->move(m);
      ev = -negamax(b, depth + 1, -beta, -alpha);
      alpha = std::max(alpha, ev);
      if (beta <= alpha) {
        return ev;
      }
    }
    return ev;
  }

  // https://www.c;hessprogramming.org/Quiescence_Search
  int quiescence(const std::shared_ptr<Board> &b, int alpha, int beta, int depth) {
    int ev = b->static_evaluate();
    if (depth == DEPTH) {
      return ev;
    }
    if (ev >= beta) {
      return beta;
    }
    if (ev > alpha) {
      alpha = ev;
    }
    std::vector<Move> mvs = b->moves();
    for (const Move &m : mvs) {
      b->move(m);
      int score = -quiescence(b, alpha, beta, depth + 1);
      b->undo();
      if (score >= beta) {
        return beta;
      }
      if (score > alpha) {
        alpha = score;
      }
    }
    return alpha;
  }

  int eval_at(Piece piece, int i, int j) {
    int val = piece_val(piece);

    if (piece == Empty) {
      return 0;
    }

    Side side = piece_side(piece);
    i = side == Side::White ? opp(i) : i;
    int mult = side == Side::White ? 1 : -1;

    auto access = [&](int a) -> int {
      return mult * ps_table[a][i][j];
    };

    if (piece == WhiteKing) {
      if (settings.bq_exists()) {
        return val + access(6);
      } else {
        return val + access(7);
      }
    } else if (piece == BlackKing) {
      if (settings.wq_exists()) {
        return val + access(6);
      } else {
        return val + access(7);
      }
    } else {
      static std::array<int, 14> index_map = {0, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 0};
      return val + access(index_map[static_cast<int>(piece)]);
    }
  }

 public:

  Board() = default;

  Board(const std::array<std::array<Piece, w>, h> &_board, const Settings &_settings) : board(_board), settings(_settings) {}

  void set_main_game() {
    main_game = true;
  }

  std::vector<std::shared_ptr<Board>> children() {
    std::vector<std::shared_ptr<Board>> ret;
    std::vector<Move> mvs = moves();
    ret.reserve(mvs.size());
    for (const Move &move : mvs) {
      std::shared_ptr<Board> b = child(move);
      ret.push_back(b);
    }
    return ret;
  }

  bool is_possible(const Move &move) {
    for (const Move &m : moves()) {
      if (m == move) {
        return true;
      }
    }
    return false;
  }

  std::shared_ptr<Board> child(const Move &m) {
    std::shared_ptr<Board> b = copy();
    b->move(m);
    return b;
  }

  // doesn't check for move legality
  void move(Move m) {
    if (!main_game) {
      past_settings.push_back(settings);
    } else {
      past_settings = {settings};
    }

    settings.set_wp_recently_moved(8);
    settings.set_bp_recently_moved(8);

    Piece piece = Placeholder;
    Piece piece_to = Placeholder;

    settings.set_last_move(m);

    if (m.is_promotion()) {
      piece = m.unpack_promotion();
    }

    if (m.is_en_passant()) {
      Coord c = m.unpack_en_passant();
      piece_to = board[c.i][c.j];
    }

    if (piece == Placeholder) {
      piece = board[m.from.i][m.from.j];
    }
    if (piece_to == Placeholder) {
      piece_to = board[m.to.i][m.to.j];
    }
    settings.set_last_captured(piece_to);
    if (piece_to == WhiteQueen) {
      settings.set_wq_exists(false);
    } else if (piece_to == BlackQueen) {
      settings.set_bq_exists(false);
    }

    // + piece after capture, - piece before capture, - piece captured
    settings.set_eval(settings.eval() + eval_at(piece, m.to.i, m.to.j)
                          - eval_at(piece_to, m.to.i, m.to.j) - eval_at(piece, m.from.i, m.from.j));

    if (is_pawn(piece) || piece_to != Empty) {
      settings.set_num_half_moves(0);
    } else {
      settings.increment_half_moves();
    }

    board[m.to.i][m.to.j] = board[m.from.i][m.from.j];
    board[m.from.i][m.from.j] = Empty;

    static Coord queen_rook_w(0, 0), king_rook_w(0, 7), queen_rook_b(7, 0), king_rook_b(7, 7), king_w(0, 4), king_b(7, 4);
    Coord from = m.from, to = m.to;

#define change_if(a, b) if (a == from || a == to) {settings.set_##b(true);}

    change_if(queen_rook_w, white_rook_queen_moved)
    change_if(king_rook_w, white_rook_king_moved)
    change_if(queen_rook_b, black_rook_queen_moved)
    change_if(king_rook_b, black_rook_king_moved)
    change_if(king_w, white_rook_queen_moved)
    change_if(king_w, white_rook_king_moved)
    change_if(king_b, black_rook_queen_moved)
    change_if(king_b, black_rook_king_moved)

#undef change_if

    if (is_king(piece) && std::abs(m.to.j - m.from.j) >= 2) {
      // queenside, move rook
      if (m.to.j == 2) {
        board[m.to.i][0] = Empty;
        board[m.to.i][3] = (m.to.i == 0 ? WhiteRook : BlackRook);
      } else if (m.to.j == 6) {
        board[m.to.i][7] = Empty;
        board[m.to.i][5] = (m.to.i == 0 ? WhiteRook : BlackRook);
      } else {
        assert(false);
      }
    } else if (is_pawn(piece)) {
      // en passant
      if (m.from.j != m.to.j && board[m.to.i][m.to.j] == Empty) {
        board[m.to.i][m.from.j] = Empty;
      } else if (m.from.j == m.to.j) {
        if (m.to.i - m.from.i == 2) {
          // white pawn
          settings.set_wp_recently_moved(m.to.j);
        } else if (m.from.i - m.to.i == 2) {
          // black pawn
          settings.set_bp_recently_moved(m.to.j);
        }
      }
    }

    if (settings.turn() == Side::Black) {
      settings.increment_moves();
    }
    settings.flip_turn();
  }

  [[nodiscard]] Board new_move(const Move &move) const {
    Board b = *this;
    b.move(move);
    return b;
  }

  Winner winner() {
    if (moves().empty()) {
      if (in_check()) {
        return (Winner) opp_side(settings.turn());
      } else {
        return Winner::Tie;
      }
    } else {
      if (settings.num_half_moves() == 100) {
        return Winner::Tie;
      }
      // 3 repeated move rule, prob not implementing
      return Winner::Neither;
    }
  }

  // need to implement special rules like en passant, castling etc
  std::vector<Move> moves() {
    std::vector<Move> mvs;

    auto add_moves_straight = [&](int begin_i, int begin_j, int dir_i, int dir_j) -> void {
      int a = begin_i + dir_i, b = begin_j + dir_j;

      while (valid(a, b)) {
        Side s = piece_side(board[a][b]);
        if (settings.turn() == s) {
          break;
        } else {
          mvs.emplace_back(begin_i, begin_j, a, b);
          if (s != Side::Empty) {
            break;
          }
        }
        a += dir_i;
        b += dir_j;
      }
    };

    auto add_moves_defined_cond = [&](int begin_i, int begin_j, const std::vector<int> &a, const std::vector<int> &b,
                                      const std::function<bool(Side)> &cond) -> void {
      int a_sz = (int) a.size();
      for (int i = 0; i < a_sz; i++) {
        int c = begin_i + a[i], d = begin_j + b[i];
        if (valid(c, d) && cond(piece_side(board[c][d]))) {
          mvs.emplace_back(begin_i, begin_j, c, d);
        }
      }
    };

    auto add_moves_defined = [&](int begin_i, int begin_j, const std::vector<int> &a,
                                 const std::vector<int> &b) -> void {
      int a_sz = (int) a.size();
      for (int i = 0; i < a_sz; i++) {
        int c = begin_i + a[i], d = begin_j + b[i];
        if (valid(c, d) && piece_side(board[c][d]) != settings.turn()) {
          mvs.emplace_back(begin_i, begin_j, c, d);
        }
      }
    };

    for (int i = 0; i < h; i++) {
      for (int j = 0; j < w; j++) {
        if (piece_side(board[i][j]) != settings.turn()) {
          continue;
        }
        if (settings.turn() == Side::White && board[i][j] == WhitePawn) {
          add_moves_defined_cond(i, j, {1, 1}, {-1, 1}, [&](Side x) { return x == Side::Black; });
          continue;
        }
        if (settings.turn() == Side::Black && board[i][j] == BlackPawn) {
          add_moves_defined_cond(i, j, {-1, -1}, {-1, 1}, [&](Side x) { return x == Side::White; });
          continue;
        }
        if (is_king(board[i][j])) {
          add_moves_defined(i, j, king_moves_i, king_moves_j);
        }
        if (is_knight(board[i][j])) {
          add_moves_defined(i, j, knight_moves_i, knight_moves_j);
        }
        if (is_bishop(board[i][j]) || is_queen(board[i][j])) {
          add_moves_straight(i, j, -1, -1);
          add_moves_straight(i, j, -1, 1);
          add_moves_straight(i, j, 1, -1);
          add_moves_straight(i, j, 1, 1);
        }
        if (is_rook(board[i][j]) || is_queen(board[i][j])) {
          add_moves_straight(i, j, -1, 0);
          add_moves_straight(i, j, 1, 0);
          add_moves_straight(i, j, 0, -1);
          add_moves_straight(i, j, 0, 1);
        }
      }
    }

    auto can_generic_castle = [&](int row, const std::vector<int> &cols) -> bool {

      if (in_check()) {
        return false;
      }

      for (int j : cols) {
        if (board[row][j] != Empty) {
          return false;
        }
      }

      for (int j : cols) {
        if (pos_check(Move(row, 4, row, j))) {
          return false;
        }
      }

      return true;
    };

    auto can_kingside_castle = [&](int row) -> bool {
      return can_generic_castle(row, {5, 6});
    };

    auto can_queenside_castle = [&](int row) -> bool {
      return can_generic_castle(row, {2, 3});
    };

    // castling
    if (settings.turn() == Side::White) {
      // can queenside
      if (!settings.white_rook_queen_moved() && can_queenside_castle(0)) {
        mvs.emplace_back(0, 4, 0, 2);
      }
      // can kingside
      if (!settings.white_rook_king_moved() && can_kingside_castle(0)) {
        mvs.emplace_back(0, 4, 0, 6);
      }
    }

    if (settings.turn() == Side::Black) {
      // can queenside
      if (!settings.black_rook_queen_moved() && can_queenside_castle(7)) {
        mvs.emplace_back(7, 4, 7, 2);
      }
      // can kingside
      if (!settings.black_rook_king_moved() && can_kingside_castle(7)) {
        mvs.emplace_back(7, 4, 7, 6);
      }
    }

//      auto check_en_passant = [&](int row, Piece piece, int recently_moved) -> void {
//        for (int j = 0; j < w; j++) {
//          if (board[row][j] != piece) {
//            continue;
//          }
//          if (j && recently_moved == j - 1) {
//            mvs.emplace_back(row, j, row + 1, j - 1, row, j - 1);
//          }
//          if (j < w - 1 && recently_moved == j + 1) {
//            mvs.emplace_back(row, j, row + 1, j - 1, row, j + 1);
//          }
//        }
//      };
//
//      // en passant
//      if (settings.turn() == Side::White) {
//        // for (int j = 0; j < )
//        check_en_passant(4, WhitePawn, settings.bp_recently_moved());
//      } else {
//        check_en_passant(3, BlackPawn, settings.wp_recently_moved());
//      }

    int dir = settings.turn() == Side::White ? 1 : -1;
    Piece tpawn = settings.turn() == Side::White ? WhitePawn : BlackPawn;

    auto check = [&](int i, int j) -> void {
      small prom = settings.turn() == Side::White ? 6 : 1;
      small ctwo = settings.turn() == Side::White ? 1 : 6;
      if (i == prom) {
        short l, r;
        if (settings.turn() == Side::White) {
          l = static_cast<short>(Piece::WhiteKnight);
          r = static_cast<short>(Piece::WhiteQueen);
        } else {
          l = static_cast<short>(Piece::BlackKnight);
          r = static_cast<short>(Piece::BlackQueen);
        }
        for (short k = l; k <= r; k++) {
          mvs.emplace_back(i, j, i + dir, j, static_cast<Piece>(k));
        }
      }
      if (board[i + dir][j] == Piece::Empty) {
        mvs.emplace_back(i, j, i + dir, j);
      }
      if (i == ctwo) {
        if (board[i + dir][j] == Piece::Empty && board[i + dir * 2][j] == Piece::Empty) {
          mvs.emplace_back(i, j, i + dir * 2, j);
        }
      }
    };

    for (small i = 1; i < h - 1; i++) {
      for (small j = 0; j < w; j++) {
        if (board[i][j] == tpawn) {
          check(i, j);
        }
      }
    }

    std::vector<Move> possible_moves;
    Side cur = settings.turn();
    for (const Move &m : mvs) {
      move(m);
      if (!in_check(cur)) {
        possible_moves.push_back(m);
      }
      undo();
    }

    return possible_moves;
  }

  bool in_check(Side side = Side::Neither) const {

    int king_i, king_j;

    if (side == Side::Neither) {
      side = settings.turn();
    }

    auto piece_exists = [&](const std::vector<int> &a, const std::vector<int> &b, Piece piece) -> bool {
      int a_sz = (int) a.size();
      for (int i = 0; i < a_sz; i++) {
        int c = king_i + a[i], d = king_j + b[i];
        if (valid(c, d) && board[c][d] == piece) {
          return true;
        }
      }
      return false;
    };

    auto line_exists = [&](int dir_i, int dir_j, Piece piece1, Piece piece2) -> bool {
      int a = king_i + dir_i, b = king_j + dir_j;
      while (valid(a, b)) {
        if (board[a][b] == piece1 || board[a][b] == piece2) {
          return true;
        } else if (board[a][b] != Empty) {
          return false;
        }
        a += dir_i;
        b += dir_j;
      }
      return false;
    };

    if (side == Side::White) {

      for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
          if (board[i][j] == WhiteKing) {
            king_i = i;
            king_j = j;
            break;
          }
        }
      }

      // only for move generation
      if (piece_exists(king_moves_i, king_moves_j, BlackKing)) {
        return true;
      }

      if (piece_exists(knight_moves_i, knight_moves_j, BlackKnight)) {
        return true;
      }

      // check in "rays" around king
      // up
      if (line_exists(1, 0, BlackRook, BlackQueen)) {
        return true;
      }
      // down
      if (line_exists(-1, 0, BlackRook, BlackQueen)) {
        return true;
      }
      // left
      if (line_exists(0, -1, BlackRook, BlackQueen)) {
        return true;
      }
      // right
      if (line_exists(0, 1, BlackRook, BlackQueen)) {
        return true;
      }
      // left-up
      if (line_exists(1, -1, BlackBishop, BlackQueen)) {
        return true;
      }
      // left-down
      if (line_exists(-1, -1, BlackBishop, BlackQueen)) {
        return true;
      }
      // right-down
      if (line_exists(-1, 1, BlackBishop, BlackQueen)) {
        return true;
      }
      // right-up
      if (line_exists(1, 1, BlackBishop, BlackQueen)) {
        return true;
      }

      return false;

    } else {

      for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
          if (board[i][j] == BlackKing) {
            king_i = i;
            king_j = j;
            break;
          }
        }
      }

      // only for move generation
      if (piece_exists(king_moves_i, king_moves_j, WhiteKing)) {
        return true;
      }

      if (piece_exists(knight_moves_i, knight_moves_j, WhiteKnight)) {
        return true;
      }

      // check in "rays" around king
      // up
      if (line_exists(1, 0, WhiteRook, WhiteQueen)) {
        return true;
      }
      // down
      if (line_exists(-1, 0, WhiteRook, WhiteQueen)) {
        return true;
      }
      // left
      if (line_exists(0, -1, WhiteRook, WhiteQueen)) {
        return true;
      }
      // right
      if (line_exists(0, 1, WhiteRook, WhiteQueen)) {
        return true;
      }
      // left-up
      if (line_exists(1, -1, WhiteBishop, WhiteQueen)) {
        return true;
      }
      // left-down
      if (line_exists(-1, -1, WhiteBishop, WhiteQueen)) {
        return true;
      }
      // right-down
      if (line_exists(-1, 1, WhiteBishop, WhiteQueen)) {
        return true;
      }
      // right-up
      if (line_exists(1, 1, WhiteBishop, WhiteQueen)) {
        return true;
      }

      return false;

    }
  }

  [[nodiscard]] Side side_turn() const {
    return settings.turn();
  }

  [[nodiscard]] std::string fen() const {

    auto convert = [&](Piece piece) -> char {
      switch (piece) {
        case WhitePawn:
          return 'P';
        case WhiteKnight:
          return 'N';
        case WhiteBishop:
          return 'B';
        case WhiteRook:
          return 'R';
        case WhiteQueen:
          return 'Q';
        case WhiteKing:
          return 'K';
        case BlackPawn:
          return 'p';
        case BlackKnight:
          return 'N';
        case BlackBishop:
          return 'b';
        case BlackRook:
          return 'r';
        case BlackQueen:
          return 'q';
        case BlackKing:
          return 'k';
        default:
          return '\0';
      }
    };

    std::string code;

    auto add_str = [&](const std::string &str) -> void {
      if (!code.empty()) {
        code += ' ';
      }
      code += (str.empty() ? "-" : str);
    };

    std::string main;
    for (int i = h - 1; i >= 0; i--) {
      int empty = 0;
      for (int j = 0; j < w; j++) {
        Piece piece = board[i][j];
        if (piece == Empty) {
          empty++;
        } else {
          if (empty) {
            main += char(empty + '0');
          }
          main += convert(piece);
          empty = 0;
        }
      }
      if (i) {
        main += '/';
      }
      if (empty) {
        main += char(empty + '0');
      }
    }

    add_str(main);

    std::string castling_rights;
    if (!settings.white_rook_queen_moved()) {
      castling_rights += 'Q';
    }
    if (!settings.white_rook_king_moved()) {
      castling_rights += 'K';
    }
    if (!settings.black_rook_queen_moved()) {
      castling_rights += 'q';
    }
    if (!settings.black_rook_king_moved()) {
      castling_rights += 'k';
    }

    add_str(castling_rights);

    std::string en_passant;
    if (settings.turn() == Side::White && settings.bp_recently_moved() != 8) {
      // can en passant black
      en_passant += char(settings.bp_recently_moved() + 'a');
      en_passant += '5';
    }
    if (settings.turn() == Side::Black && settings.wp_recently_moved() != 8) {
      en_passant += char(settings.wp_recently_moved() + 'a');
      en_passant += '3';
    }

    add_str(en_passant);

    // change this later (50 move rule)
    add_str(std::to_string(settings.num_half_moves()));

    add_str(std::to_string(settings.num_moves()));

    return code;

  }

  // let's assume game goes to 100 moves to be safe
  // total_seconds / 100 for each move, then
  // should not spend too much time on opening, lower depth works fine
  // but generally, more material -> more seconds bc more complex
  double recommended(double total_seconds) {
    double seconds = total_seconds / settings.num_moves();
    // correct for opening
    seconds *= sqrt(double(settings.num_moves())) / 10.0;
    // correct for material (min of both sides absolute value)
    static constexpr double default_material = 39;
    double cur_material = 0;
    for (int i = 0; i < h; i++) {
      for (int j = 0; j < w; j++) {
        cur_material += piece_val(board[i][j]);
      }
    }
    // average amt of material on board? -> assume 20
    double ratio = cur_material / 20;
    seconds *= ratio;
    return seconds;
  }

  // uses alpha-beta search to do this
  std::pair<Move, int> best_move(double seconds) {
    main_game = true;
    timer.begin();

    // calculate depth based on # of seconds
    // each board has on average ~20 children, ab search
    // can prune maybe 10, so ~10 children
    // can do approx ~10⁶ move-undo pairs per second
    // 10⁶ * seconds = 10 ^ depth
    // log(10⁶ * seconds) / log(10)
    // let's leave 1/5 time for space
    // log(10⁶ * seconds) / log(10) * 4 / 5 and take the floor
    int max_d = log(1e6 * seconds) / log(10.0) * 4.0 / 5.0;

    // fill in alpha, beta vals soon
    std::vector<Move> mvs = moves();
    int best = settings.turn() == Side::White ? -1e9 : 1e9;
    Move best_move = Move(-1, -1, -1, -1);
    for (const Move &move : mvs) {
      std::shared_ptr<Board> b = child(move);
      int m;
      if (sm == 1) m = ab_search(b, 1, -1e8, 1e8);
      else if (sm == 2) m = quiescence(b, -1e8, 1e8, 1);
      else if (sm == 3) m = negamax(b, 1, -1e8, 1e8);
      else assert(false);
      if ((settings.turn() == Side::White && m > best) || (settings.turn() == Side::Black && m < best)) {
        best = m;
        best_move = move;
      }
    }
    timer.stop();
    assert(best_move != Move(-1, -1, -1, -1));
    return {best_move, best};
  }

  std::array<Piece, w>& operator[] (int index) {
    return board[index];
  }

  const std::array<Piece, w>& operator[] (int index) const {
    return board[index];
  }

  int num_moves() const {
    return settings.num_moves();
  }

  int num_half_moves() const {
    return settings.num_half_moves();
  }

  // parse move from notation
  Move parse_move(std::string str) {

    small castle_height = settings.turn() == Side::White ? 0 : 7;
    if (str == "O-O" || str == "0-0") {
      return {4, castle_height, 6, castle_height};
    } else if (str == "O-O-O" || str == "0-0-0") {
      return {4, castle_height, 2, castle_height};
    }

    // we don't care if move is in check
    if (str.back() == '+') {
      str.pop_back();
    }

    bool capture = false;
    for (int i = 0; i < str.size(); i++) {
      if (str[i] == 'x') {
        str.erase(str.begin() + i);
        capture = true;
        break;
      }
    }

    int n = (int) str.size();
    small to_i = str[n - 2] - 'a';
    small to_j = str[n - 1] - '1';
    str.pop_back();
    str.pop_back();

    Piece piece = to_piece(str[0]);
    if (piece == Placeholder) {
      if (settings.turn() == Side::White) {
        piece = WhitePawn;
      } else {
        piece = BlackPawn;
      }
    } else {
      str.erase(str.begin());
    }

    // specified rows and cols (if applicable)
    int coord_i = -1, coord_j = -1;
    if (str.size() == 2) {
      coord_j = str[0] - 'a';
      coord_i = str[1] - '1';
    } else if (str.size() == 1) {
      if (str[0] >= 'a' && str[0] <= 'h') {
        coord_j = str[0] - 'a';
      } else {
        coord_i = str[0] - '1';
      }
    }

    small from_i = -1, from_j = -1;

    // gen squares around
    auto fits = [&](int a, int b) -> bool {
      return valid(a, b) && (a == coord_i || coord_i == -1) && (b == coord_j || coord_j == -1) &&
          piece == board[a][b];
    };

    auto try_set = [&](int a, int b) -> bool {
      if (fits(a, b)) {
        from_i = a;
        from_j = b;
        return true;
      }
      return false;
    };

    auto pawn = [&]() -> void {
      int i_dif = settings.turn() == Side::White ? -1 : 1;
      if (capture) {
        // check en passant
        try_set(to_i, to_j - 1);
        try_set(to_i, to_j + 1);
        try_set(to_i + i_dif, to_j - 1);
        try_set(to_i + i_dif, to_j + 1);
      } else {
        try_set(to_i + i_dif * 2, to_j);
        // this one takes priority, so we call it last
        // en passant is forced
        // make it forced it will be too funny
        try_set(to_i + i_dif, to_j);
      }
    };

    auto preset_check = [&](const std::vector<int> &a, const std::vector<int> &b) -> void {
      assert(a.size() == b.size());
      for (int i = 0; i < a.size(); i++) {
        int c = to_i + a[i], d = to_j + b[i];
        if (try_set(c, d)) {
          return;
        }
      }
    };

    auto knight = [&]() -> void {
      preset_check(knight_moves_i, knight_moves_j);
    };

    auto king = [&]() -> void {
      preset_check(king_moves_i, king_moves_j);
    };

    // use for diag and straight
    auto to_stop = [&](int a, int b) -> bool {
      return !valid(a, b) || is_opp_side(piece, board[a][b]);
    };

    auto check_pairs = [&](const std::vector<std::pair<int, int>> &vec) -> void {
      for (const auto &pair: vec) {
        int a = to_i + pair.first, b = to_j + pair.second;
        while (!to_stop(a, b)) {
          if (try_set(a, b)) {
            return;
          }
        }
        a += pair.first;
        b += pair.second;
      }
    };

    auto diag = [&]() -> void {
      // check all 4 dirs
      // -1 -1
      // -1 1
      // 1 -1
      // -1 -1
      check_pairs({{-1, -1},
                   {-1, 1},
                   {1,  -1},
                   {-1, -1}});
    };

    auto straight = [&]() -> void {
      // check all 4 dirs
      // -1 0
      // 1 0
      // 0 -1
      // 0 1
      check_pairs({{-1, 0},
                   {1,  0},
                   {0,  -1},
                   {0,  1}});
    };

    if (is_pawn(piece)) {
      pawn();
    } else if (is_knight(piece)) {
      knight();
    } else if (is_king(piece)) {
      king();
    } else {
      if (is_bishop(piece) || is_queen(piece)) {
        diag();
      }
      if (is_rook(piece) || is_queen(piece)) {
        straight();
      }
    }

    return {from_i, from_j, to_i, to_j};
  }

  // converts move to notation
  // must be called BEFORE the move is played
  std::string move_to_notation(Move move) {
    std::string ending;

    if (move.is_en_passant()) {
      move.unpack_en_passant();
      ending = " e.p.";
    } else if (move.is_promotion()) {
      Piece p = move.unpack_promotion();
      ending = "=" + to_basic_str(p);
    }

    Piece piece = board[move.from.i][move.from.j];

    // castling
    if (is_king(piece)) {
      int dis = abs(move.to.j - move.from.j);
      if (dis == 2) {
        return "O-O";
      } else if (dis == 3) {
        return "O-O-O";
      }
    }

    std::string notation;
    if (!is_pawn(piece)) {
      notation += to_notation(piece);
    }
    if (board[move.to.i][move.to.j] != Empty) {
      if (is_pawn(piece)) {
        notation += char(move.from.j + 'a');
      }
      notation += 'x';
    }

    // TODO: resolve multiple pieces on same row/col can hit the target square

    // check if multiple pieces can
    // specify row and col
    notation += char(move.to.j + 'a');
    notation += char(move.to.i + '1');

    if (pos_check(move, opp_side(settings.turn()))) {
      notation += '+';
    }

    notation += ending;

    return notation;
  }

  Settings get_settings() {
    return settings;
  }

  std::vector<std::string> notation_moves() {
    std::vector<std::string> v;
    std::vector<Move> m = moves();
    v.reserve(m.size());
    for (const Move &mm: m) {
      v.push_back(move_to_notation(mm));
    }
    return v;
  }

  // move, evaluation
  int static_evaluate() {
    return settings.eval();
  }

  static Board from_fen(const std::string &fen) {

    std::vector<std::string> strings;
    {
      std::string cur;
      for (char i: fen) {
        if (i == ' ') {
          strings.push_back(cur);
          cur = "";
        } else {
          cur += i;
        }
      }
      if (!cur.empty()) {
        strings.push_back(cur);
      }
    }

    auto convert_back = [&](char c) {
      switch (c) {
        case 'P':
          return WhitePawn;
        case 'N':
          return WhiteKnight;
        case 'B':
          return WhiteBishop;
        case 'R':
          return WhiteRook;
        case 'Q':
          return WhiteQueen;
        case 'K':
          return WhiteKing;
        case 'p':
          return BlackPawn;
        case 'n':
          return BlackKnight;
        case 'b':
          return BlackBishop;
        case 'r':
          return BlackRook;
        case 'q':
          return BlackQueen;
        case 'k':
          return BlackKing;
        default:
          return Placeholder;
      }
    };

    int i = 0, j = 0, index = 0;
    Settings s;
    std::array<std::array<Piece, h>, w> b{};

    // making the board
    for (char c: strings[0]) {
      if (c == '/') {
        i++;
        j = 0;
        continue;
      }

      if (isdigit(c)) {
        for (int k = 0; k < c - '0'; k++) {
          b[i][j] = Empty;
          j++;
        }
      } else {
        b[i][j] = convert_back(c);
        j++;
      }
    }

    s.set_turn(strings[1] == "w" ? Side::White : Side::Black);

    // do this later
    if (strings[2] == "-") {

    }

    return {b, s};
  }

  bool move_is_capture(const Move &move) {
    return board[move.to.i][move.to.j] != Empty;
  }

  void set_depth(int depth) {
    DEPTH = depth;
  }

  void set_search_mode(int search_mode) {
    sm = search_mode;
  }

};

namespace {

template<typename F>
std::string to_str(const Board &board, F f) {

  std::string repr;
  for (int i = h - 1; i >= 0; i--) {
    for (int j = 0; j < w; j++) {
      repr += f(board[i][j]);
      if (j < w - 1) {
        repr += ' ';
      }
    }
    if (i > 0) {
      repr += '\n';
    }
  }

  return repr;

}

}  // anon namespace

std::string to_string(const Board &board) {
  return to_str(board, [&](Piece piece) -> std::string { return to_string(piece); });
}

std::string to_basic_str(const Board &board) {
  return to_str(board, [&](Piece piece) -> std::string { return to_basic_str(piece); });
}

template<typename U>
U &operator<<(U &out, const Board &board) {
  return out << to_string(board);
}

} // namespace chess

#endif //ADSCHESS_BOARD_HPP
