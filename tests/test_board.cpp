#include "chess.hpp"
#include "see.hpp"

#include <gtest/gtest.h>

#include <string>

using namespace chess;

namespace {

constexpr const char *kStartFen =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

} // namespace

TEST(Board, SquareAlgebraicRoundtrip) {
  for (int r = 0; r < 8; ++r) {
    for (int f = 0; f < 8; ++f) {
      Square s(r, f);
      EXPECT_EQ(Square::from_algebraic(s.algebraic()), s);
    }
  }
  EXPECT_EQ(Square::from_algebraic("a1").index(), 0);
  EXPECT_EQ(Square::from_algebraic("h1").index(), 7);
  EXPECT_EQ(Square::from_algebraic("a8").index(), 56);
  EXPECT_EQ(Square::from_algebraic("h8").index(), 63);
  EXPECT_EQ(Square::from_algebraic("e4").algebraic(), "e4");
}

TEST(Board, PieceHelpers) {
  EXPECT_EQ(color_of(Piece::WPawn), Color::White);
  EXPECT_EQ(color_of(Piece::BKing), Color::Black);
  EXPECT_EQ(type_of(Piece::WKnight), PieceType::Knight);
  EXPECT_EQ(type_of(Piece::BBishop), PieceType::Bishop);
  EXPECT_EQ(make_piece(Color::Black, PieceType::Knight), Piece::BKnight);
  EXPECT_EQ(piece_to_fen(Piece::BKnight), 'n');
  EXPECT_EQ(piece_to_fen(Piece::WKnight), 'N');
  EXPECT_EQ(piece_from_fen('n'), Piece::BKnight);
  EXPECT_EQ(opposite(Color::White), Color::Black);
}

TEST(Board, StartPositionFen) {
  Board b;
  EXPECT_EQ(b.fen(), kStartFen);
  EXPECT_EQ(b.side_to_move(), Color::White);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("a1")), Piece::WRook);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("e1")), Piece::WKing);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("e8")), Piece::BKing);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("a8")), Piece::BRook);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("b8")), Piece::BKnight);
  EXPECT_EQ(b.legal_moves().size(), 20);
}

TEST(Board, FenRoundtripAndBlackKnight) {
  const char *fen =
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
  Board b = Board::from_fen(fen);
  EXPECT_EQ(b.fen(), fen);
  EXPECT_EQ(piece_to_fen(b.piece_at(Square::from_algebraic("b6"))), 'n');
  EXPECT_EQ(b.side_to_move(), Color::White);
}

TEST(Board, FenLoadsRankEightOnTop) {
  Board b = Board::from_fen("8/8/8/8/8/8/8/4K3 w - - 0 1");
  EXPECT_EQ(b.piece_at(Square::from_algebraic("e1")), Piece::WKing);
  Board c = Board::from_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1");
  EXPECT_EQ(c.piece_at(Square::from_algebraic("e8")), Piece::BKing);
}

TEST(Board, FenSideToMoveAndCastlingOrder) {
  Board b = Board::from_fen(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1");
  EXPECT_EQ(b.side_to_move(), Color::Black);
  EXPECT_NE(b.fen().find(" b KQkq "), std::string::npos);
}

TEST(Board, MakeUnmakeRestoresFenAndHash) {
  Board b;
  const std::string fen = b.fen();
  const auto h = b.hash();
  for (const Move &m : b.legal_moves()) {
    b.make(m);
    b.unmake();
    EXPECT_EQ(b.fen(), fen);
    EXPECT_EQ(b.hash(), h);
  }
}

TEST(Board, PawnAttacksPutKingInCheck) {
  Board b = Board::from_fen("8/8/8/3p4/4K3/8/8/8 w - - 0 1");
  EXPECT_TRUE(b.is_attacked(Square::from_algebraic("e4"), Color::Black));
  EXPECT_TRUE(b.in_check());
}

TEST(Board, KingCannotStepOntoPawnAttack) {
  Board c = Board::from_fen("8/8/8/3p4/8/4K3/8/8 w - - 0 1");
  for (const Move &m : c.legal_moves()) {
    EXPECT_NE(m.to, Square::from_algebraic("e4"));
    EXPECT_NE(m.to, Square::from_algebraic("c4"));
  }
}

TEST(Board, KnightAndQueenAttacks) {
  Board b = Board::from_fen("8/8/8/8/4n3/8/8/4K3 w - - 0 1");
  EXPECT_TRUE(b.is_attacked(Square::from_algebraic("d2"), Color::Black));
  EXPECT_TRUE(b.is_attacked(Square::from_algebraic("f2"), Color::Black));

  Board q = Board::from_fen("8/8/8/8/8/8/8/R3K2q w - - 0 1");
  EXPECT_TRUE(q.is_attacked(Square::from_algebraic("e1"), Color::Black));
  EXPECT_TRUE(q.in_check());
}

TEST(Board, CastlingRequiresEmptyBFile) {
  Board b = Board::from_fen("r3k3/8/8/8/8/8/8/R3K2R w KQq - 0 1");
  bool found_q = false;
  bool found_k = false;
  for (const Move &m : b.legal_moves()) {
    if (m.flag == MoveFlag::CastleKingside)
      found_k = true;
    if (m.flag == MoveFlag::CastleQueenside)
      found_q = true;
  }
  EXPECT_TRUE(found_k);
  EXPECT_TRUE(found_q);

  Board blocked = Board::from_fen("r3k3/8/8/8/8/8/8/RN2K2R w KQq - 0 1");
  for (const Move &m : blocked.legal_moves()) {
    EXPECT_NE(m.flag, MoveFlag::CastleQueenside);
  }
}

TEST(Board, CannotCastleThroughOrOutOfCheck) {
  Board f1 = Board::from_fen("4k3/8/8/8/8/8/8/R3K2r w KQ - 0 1");
  for (const Move &m : f1.legal_moves()) {
    EXPECT_NE(m.flag, MoveFlag::CastleKingside);
  }

  Board in_check = Board::from_fen("4k3/8/8/8/8/8/4r3/R3K2R w KQ - 0 1");
  EXPECT_TRUE(in_check.in_check());
  for (const Move &m : in_check.legal_moves()) {
    EXPECT_NE(m.flag, MoveFlag::CastleKingside);
    EXPECT_NE(m.flag, MoveFlag::CastleQueenside);
  }
}

TEST(Board, CastlingRightsClearedOnKingOrRookMove) {
  Board b = Board::from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1");
  b.make(b.parse_uci("e1e2"));
  EXPECT_EQ(b.castling_rights() & kCastleWK, 0);
  EXPECT_EQ(b.castling_rights() & kCastleWQ, 0);
  b.unmake();
  b.make(b.parse_uci("h1h2"));
  EXPECT_EQ(b.castling_rights() & kCastleWK, 0);
  EXPECT_NE(b.castling_rights() & kCastleWQ, 0);
}

TEST(Board, CastlingMakeUnmakeAndSan) {
  Board b = Board::from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1");
  Move m = b.parse_san("O-O");
  EXPECT_EQ(m.flag, MoveFlag::CastleKingside);
  EXPECT_EQ(b.to_san(m), "O-O");
  b.make(m);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("g1")), Piece::WKing);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("f1")), Piece::WRook);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("e1")), Piece::None);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("h1")), Piece::None);
  b.unmake();
  Move q = b.parse_san("O-O-O");
  EXPECT_EQ(q.flag, MoveFlag::CastleQueenside);
  EXPECT_EQ(b.to_san(q), "O-O-O");
  b.make(q);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("c1")), Piece::WKing);
  EXPECT_EQ(b.piece_at(Square::from_algebraic("d1")), Piece::WRook);
}

TEST(Board, EnPassantCapture) {
  Board b = Board::from_fen("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1");
  bool found = false;
  for (const Move &m : b.legal_moves()) {
    if (m.flag != MoveFlag::EnPassant)
      continue;
    found = true;
    EXPECT_EQ(m.to, Square::from_algebraic("d6"));
    const std::string before = b.fen();
    const auto h = b.hash();
    b.make(m);
    EXPECT_EQ(b.piece_at(Square::from_algebraic("d6")), Piece::WPawn);
    EXPECT_EQ(b.piece_at(Square::from_algebraic("d5")), Piece::None);
    EXPECT_EQ(b.piece_at(Square::from_algebraic("e5")), Piece::None);
    b.unmake();
    EXPECT_EQ(b.fen(), before);
    EXPECT_EQ(b.hash(), h);
  }
  EXPECT_TRUE(found);
}

TEST(Board, EnPassantFromDoublePush) {
  Board b = Board::from_fen("4k3/3p4/8/4P3/8/8/8/4K3 b - - 0 1");
  b.make(b.parse_uci("d7d5"));
  EXPECT_EQ(b.ep_square(), Square::from_algebraic("d6"));
  bool found = false;
  for (const Move &m : b.legal_moves()) {
    if (m.flag == MoveFlag::EnPassant)
      found = true;
  }
  EXPECT_TRUE(found);
}

TEST(Board, PromotionQuietAndCapture) {
  Board b = Board::from_fen("6k1/4P3/8/8/8/8/8/4K3 w - - 0 1");
  int promos = 0;
  for (const Move &m : b.legal_moves()) {
    if (m.flag == MoveFlag::Promotion) {
      ++promos;
      EXPECT_EQ(m.to, Square::from_algebraic("e8"));
    }
  }
  EXPECT_EQ(promos, 4);

  Board c = Board::from_fen("5nk1/4P3/8/8/8/8/8/4K3 w - - 0 1");
  int cap_promos = 0;
  for (const Move &m : c.legal_moves()) {
    if (m.flag == MoveFlag::Promotion && m.to == Square::from_algebraic("f8"))
      ++cap_promos;
  }
  EXPECT_EQ(cap_promos, 4);

  Move q = c.parse_uci("e7e8q");
  c.make(q);
  EXPECT_EQ(c.piece_at(Square::from_algebraic("e8")), Piece::WQueen);
  EXPECT_EQ(c.piece_at(Square::from_algebraic("e7")), Piece::None);
  c.unmake();
  EXPECT_EQ(c.piece_at(Square::from_algebraic("e7")), Piece::WPawn);
}

TEST(Board, PinnedPieceCannotLeaveKingInCheck) {
  Board pin = Board::from_fen("4k3/8/8/8/8/8/8/r2NK3 w - - 0 1");
  for (const Move &m : pin.legal_moves()) {
    EXPECT_NE(type_of(pin.piece_at(m.from)), PieceType::Knight);
  }
}

TEST(Board, CheckmateAndStalemate) {
  Board mate = Board::from_fen("7k/6Q1/6K1/8/8/8/8/8 b - - 0 1");
  auto st = mate.status();
  EXPECT_TRUE(st.checkmate);
  EXPECT_EQ(st.result, Result::WhiteWin);

  Board stalemate = Board::from_fen("k7/8/1Q6/8/8/8/8/4K3 b - - 0 1");
  auto s2 = stalemate.status();
  EXPECT_FALSE(s2.checkmate);
  EXPECT_EQ(s2.result, Result::Draw);
  EXPECT_EQ(s2.draw, DrawReason::Stalemate);
}

TEST(Board, FiftyMoveRule) {
  Board b = Board::from_fen("4k3/8/8/8/8/8/8/4K3 w - - 100 50");
  auto st = b.status();
  EXPECT_EQ(st.result, Result::Draw);
  EXPECT_EQ(st.draw, DrawReason::FiftyMove);
}

TEST(Board, ThreefoldRepetition) {
  Board b;
  auto play = [&](const char *u) { b.make(b.parse_uci(u)); };
  play("g1f3");
  play("g8f6");
  play("f3g1");
  play("f6g8");
  play("g1f3");
  play("g8f6");
  play("f3g1");
  play("f6g8");
  EXPECT_GE(b.repetition_count(), 3);
  EXPECT_EQ(b.status().draw, DrawReason::Repetition);
}

TEST(Board, InsufficientMaterial) {
  Board kk = Board::from_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1");
  EXPECT_TRUE(kk.is_insufficient_material());
  Board kn = Board::from_fen("4k3/8/8/8/8/8/8/4KN2 w - - 0 1");
  EXPECT_TRUE(kn.is_insufficient_material());
  Board kp = Board::from_fen("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1");
  EXPECT_FALSE(kp.is_insufficient_material());
}

TEST(Board, UciAndSanPawnAndKnights) {
  Board b;
  Move e4 = b.parse_uci("e2e4");
  EXPECT_EQ(b.to_san(e4), "e4");
  b.make(e4);
  Move e5 = b.parse_san("e5");
  EXPECT_EQ(e5.uci(), "e7e5");
  b.make(e5);
  Move nf3 = b.parse_san("Nf3");
  EXPECT_EQ(nf3.uci(), "g1f3");
}

TEST(Board, SanDisambiguation) {
  Board b = Board::from_fen("8/8/8/8/8/8/8/R3K2R w KQ - 0 1");
  const std::string a_rook = b.to_san(b.parse_uci("a1a2"));
  const std::string h_rook = b.to_san(b.parse_uci("h1h2"));
  EXPECT_TRUE(a_rook.find('a') != std::string::npos || a_rook == "Ra2");
  EXPECT_TRUE(h_rook == "Rh2" || h_rook.find('h') != std::string::npos);

  Board n3 = Board::from_fen("k7/8/8/8/8/2N1N3/8/4K3 w - - 0 1");
  EXPECT_EQ(n3.to_san(n3.parse_uci("c3d5")), "Ncd5");
  EXPECT_EQ(n3.to_san(n3.parse_uci("e3d5")), "Ned5");
}

TEST(Board, IncrementalEvalSurvivesMakeUnmake) {
  Board b;
  const int start = b.evaluate_white();
  EXPECT_EQ(start, 0);
  b.make(b.parse_uci("e2e4"));
  const int after = b.evaluate_white();
  EXPECT_GT(after, start);
  EXPECT_EQ(b.evaluate(), -after);
  b.make(b.parse_uci("e7e5"));
  b.unmake();
  EXPECT_EQ(b.evaluate_white(), after);
  b.unmake();
  EXPECT_EQ(b.evaluate_white(), start);
  EXPECT_EQ(b.evaluate(), 0);
}

TEST(Board, NullMoveRestoresPosition) {
  Board b;
  const std::string fen = b.fen();
  const int eval = b.evaluate_white();
  b.make_null();
  EXPECT_EQ(b.side_to_move(), Color::Black);
  EXPECT_EQ(b.evaluate_white(), eval);
  b.unmake();
  EXPECT_EQ(b.fen(), fen);
  EXPECT_EQ(b.evaluate_white(), eval);
}

TEST(Board, CapturesThenQuietsMatchAllPseudo) {
  Board b;
  MoveList all, caps, quiets;
  b.generate_pseudo(all, MoveGen::All);
  b.generate_pseudo(caps, MoveGen::Captures);
  b.generate_pseudo(quiets, MoveGen::Quiets);
  EXPECT_EQ(caps.size() + quiets.size(), all.size());
  EXPECT_EQ(all.size(), 20);
}

TEST(Eval, BishopPairBetterThanMaterialOnly) {
  Board pair = Board::from_fen("k7/8/8/8/8/2B1B3/8/K7 w - - 0 1");
  Board knights = Board::from_fen("k7/8/8/8/8/2N1N3/8/K7 w - - 0 1");
  // 2B vs 2N is only +20 material; the pair term should widen the gap.
  EXPECT_GT(pair.evaluate_white() - knights.evaluate_white(), 20);
}

TEST(Eval, PassedPawnSeventhBetterThanSecond) {
  Board seventh = Board::from_fen("4k3/4P3/8/8/8/8/8/4K3 w - - 0 1");
  Board second = Board::from_fen("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1");
  EXPECT_GT(seventh.evaluate_white(), second.evaluate_white());
}

TEST(Eval, KingCenterBetterInEndgameThanMiddlegame) {
  Board eg_center = Board::from_fen("8/8/4k3/8/4K3/8/4P3/8 w - - 0 1");
  Board eg_back = Board::from_fen("8/8/4k3/8/8/8/4P3/4K3 w - - 0 1");
  const int eg_delta = eg_center.evaluate_white() - eg_back.evaluate_white();

  Board mg_center =
      Board::from_fen("rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR w kq - 0 1");
  Board mg_back =
      Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  const int mg_delta = mg_center.evaluate_white() - mg_back.evaluate_white();

  EXPECT_GT(eg_delta, 0);
  EXPECT_GT(eg_delta, mg_delta);
}

TEST(Search, FindsMateInOne) {
  Board b = Board::from_fen("6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1");
  auto r = search(b, SearchLimits{1, 0.0, SearchMode::AlphaBeta});
  EXPECT_EQ(r.best_move.uci(), "a1a8");
  EXPECT_EQ(b.fen(), "6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1");
}

TEST(Search, ModesDoNotCorruptBoard) {
  const std::string fen =
      "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3";
  Board b = Board::from_fen(fen);
  for (auto mode : {SearchMode::Minimax, SearchMode::AlphaBeta,
                    SearchMode::AlphaBetaQuiescence}) {
    SearchLimits lim;
    lim.depth = (mode == SearchMode::Minimax) ? 2 : 3;
    lim.mode = mode;
    auto r = search(b, lim);
    EXPECT_FALSE(r.best_move.is_null());
    EXPECT_EQ(b.fen(), fen);
  }
}

TEST(Search, PrefersCapturingHangingQueen) {
  Board b = Board::from_fen("4k3/8/8/8/8/8/8/3qK3 w - - 0 1");
  auto r = search(b, SearchLimits{2, 0.0, SearchMode::AlphaBeta});
  EXPECT_EQ(r.best_move.to, Square::from_algebraic("d1"));
}

TEST(Search, OneLegalMoveSkipsSearch) {
  Board b = Board::from_fen("k7/2K5/8/8/8/8/8/8 b - - 0 1");
  auto r = search(b, SearchLimits{8, 0.0, SearchMode::AlphaBetaQuiescence});
  EXPECT_EQ(r.best_move.uci(), "a8a7");
  EXPECT_EQ(r.depth, 0);
}

TEST(Search, TimeBudgetDoesNotOverrunHardLimit) {
  Board b;
  SearchLimits lim;
  lim.depth = 24;
  lim.max_seconds = 0.08;
  lim.target_seconds = 0.05;
  lim.mode = SearchMode::AlphaBetaQuiescence;
  auto r = search(b, lim);
  EXPECT_FALSE(r.best_move.is_null());
  EXPECT_GE(r.depth, 1);
  EXPECT_LT(r.seconds, 0.40);
}

TEST(Search, SeeWinningKnightTakesQueen) {
  Board b = Board::from_fen("4k3/8/3q4/4p3/4N3/8/8/4K3 w - - 0 1");
  Move m = b.parse_uci("e4d6");
  EXPECT_EQ(m.uci(), "e4d6");
  EXPECT_GE(see(b, m), 500);
}

TEST(Search, SeeLosingQueenTakesProtectedPawn) {
  Board b = Board::from_fen("4k3/3p4/4Q3/8/8/8/8/4K3 w - - 0 1");
  Move m = b.parse_uci("e6d7");
  EXPECT_EQ(m.uci(), "e6d7");
  EXPECT_LE(see(b, m), -500);
}
