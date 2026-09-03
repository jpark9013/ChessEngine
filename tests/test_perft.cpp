#include "chess.hpp"

#include <gtest/gtest.h>

#include <string>

using namespace chess;

TEST(Perft, Startpos) {
  Board b;
  EXPECT_EQ(perft(b, 1), 20u);
  EXPECT_EQ(perft(b, 2), 400u);
  EXPECT_EQ(perft(b, 3), 8902u);
  EXPECT_EQ(perft(b, 4), 197281u);
  EXPECT_EQ(perft(b, 5), 4865609u);
}

TEST(Perft, Kiwipete) {
  Board b = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
  EXPECT_EQ(perft(b, 1), 48u);
  EXPECT_EQ(perft(b, 2), 2039u);
  EXPECT_EQ(perft(b, 3), 97862u);
  EXPECT_EQ(perft(b, 4), 4085603u);
}

TEST(Perft, Position3) {
  Board b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1");
  EXPECT_EQ(perft(b, 1), 14u);
  EXPECT_EQ(perft(b, 2), 191u);
  EXPECT_EQ(perft(b, 3), 2812u);
  EXPECT_EQ(perft(b, 4), 43238u);
  EXPECT_EQ(perft(b, 5), 674624u);
}

TEST(Perft, Position4) {
  Board b = Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
  EXPECT_EQ(perft(b, 1), 6u);
  EXPECT_EQ(perft(b, 2), 264u);
  EXPECT_EQ(perft(b, 3), 9467u);
  EXPECT_EQ(perft(b, 4), 422333u);
}

TEST(Perft, Position5) {
  Board b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
  EXPECT_EQ(perft(b, 1), 44u);
  EXPECT_EQ(perft(b, 2), 1486u);
  EXPECT_EQ(perft(b, 3), 62379u);
  EXPECT_EQ(perft(b, 4), 2103487u);
}

TEST(Perft, Position6) {
  Board b =
      Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
  EXPECT_EQ(perft(b, 1), 46u);
  EXPECT_EQ(perft(b, 2), 2079u);
  EXPECT_EQ(perft(b, 3), 89890u);
  EXPECT_EQ(perft(b, 4), 3894594u);
}

TEST(Perft, DoesNotChangePosition) {
  const char* fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
  Board b = Board::from_fen(fen);
  perft(b, 3);
  EXPECT_EQ(b.fen(), fen);
}
