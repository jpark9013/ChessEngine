#include "chess.hpp"

#include <iostream>
#include <string>
#include <vector>

namespace {

void line() { std::cout << "---------------------\n"; }

std::string prompt(const std::string& question, const std::vector<std::string>& allowed = {}) {
  std::cout << question << "\n";
  std::string s;
  if (allowed.empty()) {
    std::getline(std::cin, s);
    if (s.empty()) std::getline(std::cin, s);
    line();
    return s;
  }
  while (true) {
    if (!(std::cin >> s)) return {};
    for (const auto& a : allowed) {
      if (s == a) {
        line();
        return s;
      }
    }
    std::cout << "Invalid response. Try again: ";
  }
}

void print_position(chess::Board& board) {
  std::cout << board.to_string(false);
  std::cout << "FEN: " << board.fen() << "\n";
  std::cout << "Side: " << chess::to_string(board.side_to_move()) << "\n";
  std::cout << "Eval: " << (board.evaluate_white() >= 0 ? "+" : "")
            << board.evaluate_white() / 100.0 << "\n";
  auto moves = board.legal_moves();
  std::cout << "Legal: ";
  for (int i = 0; i < moves.size(); ++i) {
    std::cout << (i + 1) << "." << board.to_san(moves[i]) << " ";
  }
  std::cout << "\n";
}

void print_result(chess::Board& board) {
  auto st = board.status();
  if (st.checkmate) {
    std::cout << (st.result == chess::Result::WhiteWin ? "WHITE WINS\n" : "BLACK WINS\n");
  } else if (st.result == chess::Result::Draw) {
    std::cout << "DRAW\n";
  }
}

chess::SearchResult ai_move(chess::Board& board, const chess::SearchLimits& limits) {
  std::cout << "Engine thinking...\n";
  auto r = chess::search(board, limits);
  std::cout << "Engine plays " << board.to_san(r.best_move)
            << "  (" << r.score << " cp, depth " << r.depth
            << ", " << r.nodes << " nodes, " << r.seconds << "s)\n";
  board.make(r.best_move);
  return r;
}

void play_human(chess::Board& board) {
  auto moves = board.legal_moves();
  std::vector<std::string> options;
  for (int i = 0; i < moves.size(); ++i) {
    options.push_back(board.to_san(moves[i]));
    options.push_back(moves[i].uci());
    options.push_back(std::to_string(i + 1));
  }
  std::string s = prompt("Your move (SAN, UCI, or number):", options);
  for (int i = 0; i < moves.size(); ++i) {
    if (s == options[i * 3] || s == options[i * 3 + 1] || s == options[i * 3 + 2]) {
      board.make(moves[i]);
      return;
    }
  }
}

void ai_vs_ai(chess::SearchLimits limits) {
  chess::Board board;
  while (board.status().result == chess::Result::Ongoing) {
    print_position(board);
    line();
    ai_move(board, limits);
  }
  print_position(board);
  print_result(board);
}

void ai_vs_human(chess::SearchLimits limits) {
  chess::Board board;
  bool human_white = prompt("White or Black? Type White or Black:", {"White", "Black"}) == "White";
  print_position(board);
  if (human_white) {
    play_human(board);
    print_position(board);
  }
  while (true) {
    if (board.status().result != chess::Result::Ongoing) break;
    line();
    ai_move(board, limits);
    print_position(board);
    if (board.status().result != chess::Result::Ongoing) break;
    line();
    play_human(board);
    print_position(board);
  }
  print_result(board);
}

}  // namespace

int main() {
  std::cout << "ChessEngine\n";
  line();

  std::vector<std::string> depths;
  for (int i = 1; i <= 8; ++i) depths.push_back(std::to_string(i));
  int depth = std::stoi(prompt("Search depth (1-8):", depths));

  std::string mode = prompt(
      "Search mode?\n1. Minimax\n2. Alpha-beta\n3. Alpha-beta + quiescence",
      {"1", "2", "3"});

  chess::SearchLimits limits;
  limits.depth = depth;
  limits.mode = static_cast<chess::SearchMode>(std::stoi(mode));

  if (prompt("1 = AI vs AI, 2 = AI vs human:", {"1", "2"}) == "1") {
    ai_vs_ai(limits);
  } else {
    ai_vs_human(limits);
  }
  return 0;
}
