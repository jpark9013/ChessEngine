#pragma once

#include "board.hpp"
#include "types.hpp"

namespace chess {

inline int see_piece_value(PieceType t) {
  switch (t) {
    case PieceType::Pawn: return 100;
    case PieceType::Knight: return 320;
    case PieceType::Bishop: return 330;
    case PieceType::Rook: return 500;
    case PieceType::Queen: return 900;
    case PieceType::King: return 20000;
    default: return 0;
  }
}

// Net centipawns for the side that captures first, assuming both sides
// recapture with the least valuable attacker and stop when continuing loses.
// Non-captures score the hanging-piece sequence on the destination square.
int see(const Board& board, const Move& m);

}  // namespace chess
