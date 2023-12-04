#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <numeric>

struct Card {
  int number;
  std::vector<int> left, right;
};

int count_match(const Card &card) {
  int count = 0;
  for (const auto &num : card.right) {
    if (card.left.cend() != std::find(card.left.cbegin(), card.left.cend(), num)) {
      ++count;
    }
  }
  return count;
}

Card read_card(const std::string &line) {
  Card card;
  std::istringstream iss(line);
  const auto colon_pos = line.find(":");
  const auto bracket_pos = line.find(" | ");
  const auto first = line.substr(colon_pos + 1, bracket_pos - colon_pos - 1);
  const auto second = line.substr(bracket_pos + 3);
  std::istringstream first_iss(first), second_iss(second);
  int num;
  while (first_iss >> num) { card.left.push_back(num); }
  int count = 0;
  while (second_iss >> num) { card.right.push_back(num); }
  return card;
}

int value(const int count) {
  if (count == 0) { return 0; }
  int r = 1;
  for (int i = 1; i < count; ++i) {
    r *= 2;
  }
  return r;
}

int main() {
  constexpr char path[] = "/usr/local/google/home/xdavidliu/Documents/temp/data.txt";
  if (auto fs = std::ifstream(path)) {
    std::string line;
    std::vector<Card> cards;
    while (std::getline(fs, line) && !line.empty()) {
      cards.push_back(read_card(line));
    }
    std::vector<int> part1_counts(cards.size(), 0);
    for (std::size_t i = 0; i < cards.size(); ++i) {
      part1_counts[i] = count_match(cards[i]);

    }
    long part1 = 0;
    for (const auto &num : part1_counts) { part1 += value(num); }
    std::cout << "part 1 = " << part1 << '\n';  // 25231

    std::vector<long> part2_counts(cards.size(), 1);
    for (std::size_t i = 0; i < cards.size(); ++i) {
      const auto v = part1_counts[i];
      for (std::size_t k = i + 1; k <= i + v && k < cards.size(); ++k) {
        part2_counts[k] += part2_counts[i];
      }
    }
    std::cout << "part 2 = " << std::accumulate(part2_counts.cbegin(), part2_counts.cend(), 0) << '\n';  // 9721255
  }
}
