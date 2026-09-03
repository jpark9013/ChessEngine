"""Smoke-test the chessengine Python module. PYTHONPATH must include the build dir."""

from __future__ import annotations

import unittest

REQUIRED = [
    "Board",
    "Move",
    "Square",
    "Color",
    "Piece",
    "PieceType",
    "MoveFlag",
    "Result",
    "DrawReason",
    "SearchMode",
    "SearchLimits",
    "SearchResult",
    "GameStatus",
    "perft",
    "search",
    "opposite",
    "make_piece",
    "color_of",
    "type_of",
]


class TestChessengine(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        try:
            import chessengine as ce
        except ImportError as exc:  # pragma: no cover - env hint
            raise unittest.SkipTest(
                f"cannot import chessengine ({exc}); set PYTHONPATH to the CMake build dir"
            ) from exc
        cls.ce = ce

    def test_public_names(self) -> None:
        missing = [name for name in REQUIRED if not hasattr(self.ce, name)]
        self.assertEqual(missing, [])

    def test_start_fen_and_opening(self) -> None:
        board = self.ce.Board()
        self.assertEqual(
            board.fen(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        )
        self.assertEqual(len(board.legal_moves()), 20)
        self.assertEqual(self.ce.perft(board, 1), 20)

    def test_san_push_and_unmake(self) -> None:
        board = self.ce.Board()
        board.push_san("e4")
        board.push_san("e5")
        board.push_san("Nf3")
        self.assertEqual(board.side_to_move(), self.ce.Color.BLACK)
        self.assertEqual(
            board.piece_at(self.ce.Square.from_algebraic("f3")),
            self.ce.Piece.W_KNIGHT,
        )
        board.pop()
        self.assertEqual(
            board.piece_at(self.ce.Square.from_algebraic("g1")),
            self.ce.Piece.W_KNIGHT,
        )

    def test_perft_two(self) -> None:
        self.assertEqual(self.ce.Board().perft(2), 400)

    def test_mate_in_one_does_not_mutate(self) -> None:
        fen = "6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1"
        mate = self.ce.Board.from_fen(fen)
        result = mate.search(depth=1, mode=self.ce.SearchMode.ALPHABETA)
        self.assertEqual(result.best_move.uci(), "a1a8")
        self.assertEqual(mate.fen(), fen)

    def test_black_knight_fen_letter(self) -> None:
        kiwipete = self.ce.Board.from_fen(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        )
        self.assertIn("n", kiwipete.fen().split()[0])

    def test_copy_fen(self) -> None:
        board = self.ce.Board()
        board.push_san("e4")
        self.assertEqual(board.copy().fen(), board.fen())


if __name__ == "__main__":
    unittest.main()
