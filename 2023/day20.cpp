#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <exception>
#include <fstream>
#include <sstream>
#include <string_view>
#include <tuple>
#include <deque>

struct Input {
  char type = 0;
  std::string lhs;
  std::vector<std::string> rhs;
};

void push_comma_separated(std::istringstream& iss,
                          std::vector<std::string>& out) {
  std::string word;
  while (iss >> word) {
    if (word.back() == ',') {
      out.push_back(word.substr(0, word.size() - 1));
    } else {
      out.push_back(word);
    }
  }
}

Input from_line(const std::string& line) {
  if (line.find("broadcaster") == 0) {
    std::cout << "from line called on broadcaster\n";
    throw std::exception();
  }
  std::istringstream iss(line);
  Input input;
  iss >> input.type;
  iss >> input.lhs;
  std::string ignore;
  iss >> ignore;  // ->
  push_comma_separated(iss, input.rhs);
  return input;
}

auto read_file(const char* filepath) {
  if (auto fs = std::ifstream(filepath)) {
    std::map<std::string, char> type_of;
    std::map<std::string, std::vector<std::string>> neighbors;
    std::string line;
    while (std::getline(fs, line)) {
      if (0 == line.find("broadcaster")) {
        std::vector<std::string> br_ns;
        constexpr auto start = std::string_view("broadcaster -> ").size();
        std::istringstream iss(line.substr(start));
        push_comma_separated(iss, br_ns);
        neighbors["broadcaster"] = std::move(br_ns);
      } else {
        const auto [type, lhs, ns] = from_line(line);
        type_of[lhs] = type;
        neighbors[lhs] = std::move(ns);
      }
    }
    return std::make_tuple(type_of, neighbors);
  } else {
    std::cout << "invalid filepath\n";
    throw std::exception();
  }
}

struct Pulse {
  std::string src, dest;
  bool high;
};

// when summing, don't forget to count the initial pulse from button to
// broadcast

constexpr char flip_ch = '%';
constexpr char conv_ch = '&';
constexpr char *broadcast = "broadcast";

auto get_converge_neighbors(const std::map<std::string, char> &type_of, const std::map<std::string, std::vector<std::string>> &neighbors) {
   std::map<std::string, std::vector<std::string>> out;
  for (const auto &[key, val] : neighbors) {
    for (const auto &dest : val) {
      const auto found_type = type_of.find(dest);
      if (found_type == type_of.cend() || found_type->second != conv_ch) { continue; }
      auto [iter, worked] = out.insert({dest, std::vector<std::string>()});
      iter->second.push_back(key);
    }
  }
  return out;
}

auto get_flip_on(const std::map<std::string, char> &type_of) {
  std::map<std::string, bool> flip_on;
  for (const auto &[key, val] : type_of) {
    if (val == flip_ch) {
      flip_on[key] = false;
    }
  }
  return flip_on;
}

void foo1() {
  constexpr char filepath[] = "/tmp/example2.txt";
  const auto [type_of, neighbors] = read_file(filepath);
  // key is dest, only convergence, inner key is src
  std::map<std::string, std::map<std::string, bool>> last_sent_to_from;
  const auto converge_neighbors = get_converge_neighbors(type_of, neighbors);
  for (const auto &[key, val] : neighbors) {
    // "they initially default to remembering a low pulse for each input."
    last_sent_from.insert({key, false});  // this includes broadcast
  }
  // "Flip-flop modules ... are initially off."
  auto flip_on = get_flip_on(type_of);
  std::deque<Pulse> que;
  // push button
  last_sent_from[broadcast] = false;
  for (const auto& dest: neighbors.at(broadcast)) {
    que.push_back({broadcast, dest, false});
  }
  while (!que.empty()) {
    const auto [src, dest, high] = que.front();
    que.pop_front();
    last_sent_from[dest] = high;
    const auto found = type_of.find(dest);
    if (found != type_of.cend()) {
      switch (found->second) {
        case '%': {  // flip
          auto flip_iter = flip_on.find(dest);
          const auto old_val = flip_iter->second;
          flip_iter->second = !old_val;
          for (const auto &neigh : neighbors.at(dest)) {
            // "If it was off, it turns on and sends a high pulse. If it was on,
            // it turns off and sends a low pulse."
            que.push_back({dest, neigh, !old_val});
          }
          break;
        }
        case '&': {  // converge
          // todo not sure about this; there's a single toplevel map
          // should there be a separate map for every one? ideally yes.
          last_sent_from[src] = high;

          break;
        }
        default: {
          std::cout << "invalid found->second\n";
          throw std::exception();
        }
      }
    } else {  // dest no type; maybe output or something
      // todo
    }
  }
}

int main() {
  foo1();
}
