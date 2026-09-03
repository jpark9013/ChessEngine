#include "see.hpp"

#include "attacks.hpp"
#include "bitboard.hpp"

#include <algorithm>

namespace chess {
namespace {

Bitboard side_pieces(const Bitboard pc[2][7], Color c) {
  const int i = static_cast<int>(c);
  return pc[i][static_cast<int>(PieceType::Pawn)] | pc[i][static_cast<int>(PieceType::Knight)] |
         pc[i][static_cast<int>(PieceType::Bishop)] | pc[i][static_cast<int>(PieceType::Rook)] |
         pc[i][static_cast<int>(PieceType::Queen)] | pc[i][static_cast<int>(PieceType::King)];
}

Bitboard attackers_to(int sq, Bitboard occ, const Bitboard pc[2][7]) {
  Bitboard att = attacks::pawn(Color::Black, sq) & pc[0][static_cast<int>(PieceType::Pawn)];
  att |= attacks::pawn(Color::White, sq) & pc[1][static_cast<int>(PieceType::Pawn)];
  att |= attacks::knight(sq) & (pc[0][static_cast<int>(PieceType::Knight)] |
                                pc[1][static_cast<int>(PieceType::Knight)]);
  att |= attacks::king(sq) & (pc[0][static_cast<int>(PieceType::King)] |
                              pc[1][static_cast<int>(PieceType::King)]);
  const Bitboard bishops = pc[0][static_cast<int>(PieceType::Bishop)] |
                           pc[1][static_cast<int>(PieceType::Bishop)] |
                           pc[0][static_cast<int>(PieceType::Queen)] |
                           pc[1][static_cast<int>(PieceType::Queen)];
  att |= attacks::bishop(sq, occ) & bishops;
  const Bitboard rooks = pc[0][static_cast<int>(PieceType::Rook)] |
                         pc[1][static_cast<int>(PieceType::Rook)] |
                         pc[0][static_cast<int>(PieceType::Queen)] |
                         pc[1][static_cast<int>(PieceType::Queen)];
  att |= attacks::rook(sq, occ) & rooks;
  return att;
}

int least_valuable(Bitboard attackers, Color side, const Bitboard pc[2][7], PieceType& type) {
  const int c = static_cast<int>(side);
  for (int t = static_cast<int>(PieceType::Pawn); t <= static_cast<int>(PieceType::King); ++t) {
    const Bitboard subset = attackers & pc[c][t];
    if (subset) {
      type = static_cast<PieceType>(t);
      return lsb(subset);
    }
  }
  return -1;
}

}  // namespace

int see(const Board& board, const Move& m) {
  if (m.is_null() || !m.from.valid() || !m.to.valid()) return 0;

  const int to = m.to.index();
  const int from = m.from.index();
  const Piece moved = board.piece_at(m.from);
  if (moved == Piece::None) return 0;

  Piece captured = Piece::None;
  int ep_sq = -1;
  if (m.flag == MoveFlag::EnPassant) {
    captured = make_piece(opposite(board.side_to_move()), PieceType::Pawn);
    ep_sq = Square(m.from.rank(), m.to.file()).index();
  } else {
    captured = board.piece_at(m.to);
  }

  Bitboard pc[2][7]{};
  Bitboard occ = 0;
  for (int i = 0; i < 64; ++i) {
    const Piece p = board.piece_at(Square(i));
    if (p == Piece::None) continue;
    pc[static_cast<int>(color_of(p))][static_cast<int>(type_of(p))] |= bit(i);
    occ |= bit(i);
  }

  int gain[32]{};
  int d = 0;
  gain[0] = see_piece_value(type_of(captured));
  if (m.flag == MoveFlag::Promotion) {
    gain[0] += see_piece_value(m.promotion) - see_piece_value(PieceType::Pawn);
  }

  const Color us = color_of(moved);
  occ ^= bit(from);
  pc[static_cast<int>(us)][static_cast<int>(type_of(moved))] ^= bit(from);
  if (ep_sq >= 0) {
    occ ^= bit(ep_sq);
    pc[static_cast<int>(color_of(captured))][static_cast<int>(PieceType::Pawn)] ^= bit(ep_sq);
  } else if (captured != Piece::None) {
    pc[static_cast<int>(color_of(captured))][static_cast<int>(type_of(captured))] ^= bit(to);
  }
  occ |= bit(to);

  PieceType next = m.flag == MoveFlag::Promotion ? m.promotion : type_of(moved);
  Color side = opposite(us);
  Bitboard att = attackers_to(to, occ, pc);

  while (d < 31) {
    PieceType t = PieceType::None;
    const int sq = least_valuable(att, side, pc, t);
    if (sq < 0) break;

    ++d;
    gain[d] = see_piece_value(next) - gain[d - 1];
    next = t;

    occ ^= bit(sq);
    pc[static_cast<int>(side)][static_cast<int>(t)] ^= bit(sq);
    att = attackers_to(to, occ, pc);
    if (t == PieceType::King && (att & side_pieces(pc, opposite(side)))) {
      --d;
      break;
    }
    side = opposite(side);
  }

  while (d > 0) {
    gain[d - 1] = -std::max(-gain[d - 1], gain[d]);
    --d;
  }
  return gain[0];
}

}  // namespace chess
