#include <iostream>
#include <iomanip>

#include "board.hpp"
#include "random.hpp"

using Chess::Board;
using Chess::Move;
using Chess::Side;
using Chess::Winner;

int depth = -1;
int sm = -1;

void ai_vs_ai() {
  std::shared_ptr<Board> board(std::make_shared<Board>());
  board->set_main_game();
  board->set_depth(depth);
  board->set_search_mode(sm);

  std::vector<std::string> moves;

  float current_eval;

  auto print_b = [&](const std::shared_ptr<Board> &b) -> void {
    std::cout << "---------------------------------------------" << std::endl;
    if (!moves.empty()) {
      std::cout << "Last move: " << moves.back() << std::endl;
    }
    int cur_m = (int) moves.size() + 1;
    std::cout << "Current move: " << (cur_m / 2 + cur_m % 2) << std::endl;
    std::cout << "Side to move: " << (b->side_turn() == Side::White ? "White" : "Black") << std::endl;
    std::cout << "Possible next moves: ";
    for (const std::string &str : b->notation_moves()) {
      std::cout << str << ' ';
    }
    std::cout << std::endl;
    std::cout << *b << std::endl << std::endl;
    std::cout << "Current evaluation: " << (current_eval < 0 ? "" : "+") << current_eval << std::endl;
    std::cout << "Moves so far: ";
    for (int i = 0; i < moves.size(); i++) {
      if (i % 2 == 0) {
        std::cout << i / 2 + 1 << ". ";
      }
      std::cout << moves[i] << ' ';
    }
    std::cout << std::endl;
  };

  auto go1 = [&]() -> void {
    print_b(board);
    Random random;
    while (board->winner() == Winner::Neither) {
      auto pair = board->best_move(1);
      auto p = board->child(pair.first);
      moves.push_back(board->move_to_notation(pair.first));
      current_eval = float(pair.second) / 100;
      print_b(p);
      board = p;
    }
    std::string w;
    if (board->winner() == Winner::White) {
      w = "WHITE WON";
    } else if (board->winner() == Winner::Black) {
      w = "BLACK WON";
    } else {
      w = "DRAW";
    }
    std::cout << w << std::endl;
  };

  go1();
}

void line() {
  std::cout << "---------------------" << std::endl;
}

std::string prompt(const std::string &o, const std::vector<std::string> &responses = {}) {
  std::cout << o << std::endl;
  std::string s;
  if (!responses.empty()) {
    std::cin >> s;
    bool match = false;
    while (!match) {
      for (const std::string &pos_response: responses) {
        if (s == pos_response) {
          match = true;
          break;
        }
      }
      if (!match) {
        std::cout << "Invalid response. Please enter a different response: ";
        std::cin >> s;
      }
    }
  }
  line();
  return s;
}

void ai_vs_human() {
  Board board;
  board.set_main_game();
  board.set_depth(depth);
  board.set_search_mode(sm);

  auto turn = [&]() -> void {
    if (board.side_turn() == Side::White) {
      std::cout << "White to Play" << std::endl;
    } else {
      std::cout << "Black to Play" << std::endl;
    }
  };

  auto ai_play = [&]() -> void {
    std::vector<Move> mvs = board.moves();
    std::cout << "Possible moves: " << std::endl;
    for (int i = 0; i < (int) mvs.size(); i++) {
      std::cout << i + 1 << ". " << board.move_to_notation(mvs[i]) << ' ';
    }
    std::cout << std::endl << "AI is thinking..." << std::endl;
    auto pair = board.best_move(1);
    Move m = pair.first;
    std::string ms = board.move_to_notation(m);
    std::cout << "AI played: " << ms << std::endl;
    board.move(m);
    std::cout << board << std::endl;
  };

  auto human_play = [&]() -> void {
    std::vector<Move> mvs = board.moves();
    std::cout << "Possible moves: " << std::endl;
    std::vector<std::string> ns;
    ns.reserve(mvs.size());
    for (const Move &m : mvs) {
      ns.push_back(board.move_to_notation(m));
    }
    for (int i = 0; i < (int) mvs.size(); i++) {
      std::cout << i + 1 << ". " << ns[i] << ' ';
    }
    std::cout << std::endl;
    std::vector<std::string> responses = ns;
    for (int i = 1; i <= ns.size(); i++) {
      responses.push_back(std::to_string(i));
    }
    std::string s = prompt("Play a move, either indicate the notation or the number: ", responses);
    int ind = -1;
    for (int i = 0; i < responses.size(); i++) {
      if (s == responses[i]) {
        ind = i;
        break;
      }
    }
    assert(ind != -1);
    ind %= (int) mvs.size();
    board.move(mvs[ind]);
    std::cout << board << std::endl;
  };

  if (prompt("White or Black? Type White or Black: ", {"White", "Black"}) == "White") {
    turn();
    std::cout << board << std::endl;
    human_play();
  }

  auto pr_winner = [&]() -> void {
    if (board.winner() == Winner::White) {
      std::cout << "WHITE WON" << std::endl;
    } else if (board.winner() == Winner::Black) {
      std::cout << "BLACK WON" << std::endl;
    } else {
      std::cout << "DRAW" << std::endl;
    }
  };

  // start ai play
  while (true) {
    line();
    turn();
    ai_play();
    if (board.winner() != Winner::Neither) {
      pr_winner();
      break;
    }
    line();
    turn();
    human_play();
    if (board.winner() != Winner::Neither) {
      pr_winner();
      break;
    }
  }
}

// g++ -std=c++2a -o cur main.cpp && ./cur
void solve() {
  std::cout << std::fixed << std::setprecision(2);

  prompt("Chess Game");
  Chess::set_output_complex(prompt("Output with real chess pieces or letters? 1 for real chess pieces, 2 for letters. Real chess pieces may have "
                                   "misalignment so it looks wrong at times: ", {"1", "2"}) == "1");

  std::vector<std::string> depths;
  for (int i = 1; i <= 10; i++) {
    depths.push_back(std::to_string(i));
  }

  depth = std::stoi(prompt("What depth should the engine search? Higher depth is slower, but can be more accurate. Enter a number between 1 and 10: ", depths));

  sm = std::stoi(prompt("Which search mode should the engine use?\n1. Alpha Beta Search\n2. Quiescence\n3. Negamax", {"1", "2", "3"}));

  if (prompt("AI vs AI, or AI vs human? Answer 1 for AI vs AI and 2 for AI vs human: ", {"1", "2"}) == "1") {
    ai_vs_ai();
  } else {
    ai_vs_human();
  }
}

void solve1() {
  Chess::set_output_complex(false);
  Board b;
  std::cout << to_string(b) << std::endl;
  std::cout << "Possible moves: " << std::endl;
  for (const std::string &str : b.notation_moves()) {
    std::cout << str << ' ';
  }
  std::cout << std::endl;
  std::cout << to_string(b) << std::endl;
}

int main() {
  // solve1();
  solve();
  return 0;
}