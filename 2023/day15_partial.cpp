#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <string_view>

int hash(const std::string_view instruction) {
  int current = 0;
  for (const auto ch : instruction) {
    current = ((current + ch) * 17) % 256;
  }
  return current;
}

auto split_commas(const std::string &line) {
  std::vector<std::string> out;
  auto comma_pos = line.find(',');
  std::size_t beg = 0;
  while (comma_pos != std::string::npos) {
    out.push_back(line.substr(beg, comma_pos - beg));
    beg = comma_pos + 1;
    comma_pos = line.find(',', beg);
  }
  out.push_back(line.substr(beg, line.size() - beg));
  return out;
}

int main() {
  if (auto fs = std::ifstream("/tmp/data.txt")) {
    std::string line;
    std::getline(fs, line);
    const auto tokens = split_commas(line);
    long part1 = 0;
    for (const auto &token : tokens) { part1 += hash(token); }
    std::cout << "part 1 = " << part1 << '\n';
  }
}
