#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "chess.hpp"
#include "attacks.hpp"
#include "zobrist.hpp"

namespace py = pybind11;

PYBIND11_MODULE(chessengine, m) {
  chess::attacks::init();
  chess::zobrist::init();
  m.doc() = "ChessEngine: legal move generation, search, FEN, and SAN/UCI notation.";

  py::enum_<chess::Color>(m, "Color")
      .value("WHITE", chess::Color::White)
      .value("BLACK", chess::Color::Black);

  py::enum_<chess::PieceType>(m, "PieceType")
      .value("NONE", chess::PieceType::None)
      .value("PAWN", chess::PieceType::Pawn)
      .value("KNIGHT", chess::PieceType::Knight)
      .value("BISHOP", chess::PieceType::Bishop)
      .value("ROOK", chess::PieceType::Rook)
      .value("QUEEN", chess::PieceType::Queen)
      .value("KING", chess::PieceType::King);

  py::enum_<chess::Piece>(m, "Piece")
      .value("NONE", chess::Piece::None)
      .value("W_PAWN", chess::Piece::WPawn)
      .value("W_KNIGHT", chess::Piece::WKnight)
      .value("W_BISHOP", chess::Piece::WBishop)
      .value("W_ROOK", chess::Piece::WRook)
      .value("W_QUEEN", chess::Piece::WQueen)
      .value("W_KING", chess::Piece::WKing)
      .value("B_PAWN", chess::Piece::BPawn)
      .value("B_KNIGHT", chess::Piece::BKnight)
      .value("B_BISHOP", chess::Piece::BBishop)
      .value("B_ROOK", chess::Piece::BRook)
      .value("B_QUEEN", chess::Piece::BQueen)
      .value("B_KING", chess::Piece::BKing);

  py::enum_<chess::MoveFlag>(m, "MoveFlag")
      .value("NORMAL", chess::MoveFlag::Normal)
      .value("DOUBLE_PAWN", chess::MoveFlag::DoublePawn)
      .value("EN_PASSANT", chess::MoveFlag::EnPassant)
      .value("CASTLE_KINGSIDE", chess::MoveFlag::CastleKingside)
      .value("CASTLE_QUEENSIDE", chess::MoveFlag::CastleQueenside)
      .value("PROMOTION", chess::MoveFlag::Promotion);

  py::enum_<chess::Result>(m, "Result")
      .value("ONGOING", chess::Result::Ongoing)
      .value("WHITE_WIN", chess::Result::WhiteWin)
      .value("BLACK_WIN", chess::Result::BlackWin)
      .value("DRAW", chess::Result::Draw);

  py::enum_<chess::DrawReason>(m, "DrawReason")
      .value("NONE", chess::DrawReason::None)
      .value("STALEMATE", chess::DrawReason::Stalemate)
      .value("FIFTY_MOVE", chess::DrawReason::FiftyMove)
      .value("REPETITION", chess::DrawReason::Repetition)
      .value("INSUFFICIENT", chess::DrawReason::Insufficient);

  py::enum_<chess::SearchMode>(m, "SearchMode")
      .value("MINIMAX", chess::SearchMode::Minimax)
      .value("ALPHABETA", chess::SearchMode::AlphaBeta)
      .value("ALPHABETA_QUIESCENCE", chess::SearchMode::AlphaBetaQuiescence);

  py::class_<chess::Square>(m, "Square")
      .def(py::init<int>())
      .def(py::init<int, int>(), py::arg("rank"), py::arg("file"))
      .def_static("from_algebraic", &chess::Square::from_algebraic)
      .def("index", &chess::Square::index)
      .def("rank", &chess::Square::rank)
      .def("file", &chess::Square::file)
      .def("valid", &chess::Square::valid)
      .def("algebraic", &chess::Square::algebraic)
      .def("__str__", &chess::Square::algebraic)
      .def("__repr__", [](chess::Square s) { return "Square('" + s.algebraic() + "')"; })
      .def("__eq__", [](chess::Square a, chess::Square b) { return a == b; });

  py::class_<chess::Move>(m, "Move")
      .def(py::init<>())
      .def_readwrite("from_sq", &chess::Move::from)
      .def_readwrite("to_sq", &chess::Move::to)
      .def_readwrite("promotion", &chess::Move::promotion)
      .def_readwrite("flag", &chess::Move::flag)
      .def("uci", &chess::Move::uci)
      .def("__str__", &chess::Move::uci)
      .def("__repr__", [](const chess::Move& mv) { return "Move('" + mv.uci() + "')"; })
      .def("__eq__", [](const chess::Move& a, const chess::Move& b) { return a == b; });

  py::class_<chess::GameStatus>(m, "GameStatus")
      .def_readonly("result", &chess::GameStatus::result)
      .def_readonly("draw", &chess::GameStatus::draw)
      .def_readonly("checkmate", &chess::GameStatus::checkmate);

  py::class_<chess::SearchLimits>(m, "SearchLimits")
      .def(py::init<>())
      .def_readwrite("depth", &chess::SearchLimits::depth)
      .def_readwrite("max_seconds", &chess::SearchLimits::max_seconds)
      .def_readwrite("target_seconds", &chess::SearchLimits::target_seconds)
      .def_readwrite("mode", &chess::SearchLimits::mode);

  py::class_<chess::SearchResult>(m, "SearchResult")
      .def_readonly("best_move", &chess::SearchResult::best_move)
      .def_readonly("score", &chess::SearchResult::score)
      .def_readonly("depth", &chess::SearchResult::depth)
      .def_readonly("nodes", &chess::SearchResult::nodes)
      .def_readonly("seconds", &chess::SearchResult::seconds);

  py::class_<chess::Board>(m, "Board")
      .def(py::init<>())
      .def_static("from_fen", &chess::Board::from_fen)
      .def("fen", &chess::Board::fen)
      .def("piece_at", &chess::Board::piece_at)
      .def("side_to_move", &chess::Board::side_to_move)
      .def("castling_rights", &chess::Board::castling_rights)
      .def("ep_square", &chess::Board::ep_square)
      .def("halfmove_clock", &chess::Board::halfmove_clock)
      .def("fullmove_number", &chess::Board::fullmove_number)
      .def("king_square", &chess::Board::king_square)
      .def("hash", &chess::Board::hash)
      .def("push", &chess::Board::make, py::arg("move"))
      .def("pop", &chess::Board::unmake)
      .def("push_uci", [](chess::Board& b, std::string_view u) {
        chess::Move m = b.parse_uci(u);
        b.make(m);
        return m;
      })
      .def("push_san", [](chess::Board& b, std::string_view s) {
        chess::Move m = b.parse_san(s);
        b.make(m);
        return m;
      })
      .def("parse_uci", &chess::Board::parse_uci)
      .def("parse_san", &chess::Board::parse_san)
      .def("to_san", &chess::Board::to_san)
      .def("in_check", [](const chess::Board& b) { return b.in_check(); })
      .def("is_attacked", &chess::Board::is_attacked)
      .def("is_legal", &chess::Board::is_legal)
      .def("gives_check", &chess::Board::gives_check)
      .def("legal_moves", [](chess::Board& b) { return b.legal_moves().to_vector(); })
      .def("legal_captures", [](chess::Board& b) { return b.legal_captures().to_vector(); })
      .def("status", &chess::Board::status)
      .def("evaluate", &chess::Board::evaluate)
      .def("evaluate_white", &chess::Board::evaluate_white)
      .def("perft", [](chess::Board& b, int depth) { return chess::perft(b, depth); })
      .def("search", [](chess::Board& b, int depth, chess::SearchMode mode,
                        double seconds, double target) {
            chess::SearchLimits lim;
            lim.depth = depth;
            lim.mode = mode;
            lim.max_seconds = seconds;
            lim.target_seconds = target;
            return chess::search(b, lim);
          },
          py::arg("depth") = 4,
          py::arg("mode") = chess::SearchMode::AlphaBetaQuiescence,
          py::arg("max_seconds") = 0.0,
          py::arg("target_seconds") = 0.0)
      .def("copy", [](const chess::Board& b) { return b; })
      .def("__str__", [](const chess::Board& b) { return b.to_string(false); })
      .def("__repr__", [](const chess::Board& b) { return "Board('" + b.fen() + "')"; });

  m.def("perft", &chess::perft);
  m.def("search", [](chess::Board& b, int depth, chess::SearchMode mode,
                     double seconds, double target) {
          chess::SearchLimits lim;
          lim.depth = depth;
          lim.mode = mode;
          lim.max_seconds = seconds;
          lim.target_seconds = target;
          return chess::search(b, lim);
        },
        py::arg("board"),
        py::arg("depth") = 4,
        py::arg("mode") = chess::SearchMode::AlphaBetaQuiescence,
        py::arg("max_seconds") = 0.0,
        py::arg("target_seconds") = 0.0);
  m.def("opposite", &chess::opposite);
  m.def("make_piece", &chess::make_piece);
  m.def("color_of", &chess::color_of);
  m.def("type_of", &chess::type_of);
}
