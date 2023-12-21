#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <fstream>
#include <algorithm>
#include <exception>
#include <utility>

auto read_grid(const char *filepath) {
  if (auto fs = std::ifstream(filepath)) {
    std::string line;
    std::vector<std::string> grid;
    while (std::getline(fs, line)) {
      grid.push_back(line);
    }
    return grid;
  } else {
    throw std::exception();
  }
}

auto find_start(const std::vector<std::string> &grid) {
  for (std::size_t r = 0; r < grid.size(); ++r) {
    for (std::size_t c = 0; c < grid.front().size(); ++c) {
      if ('S' == grid[r][c]) { return std::make_pair(r, c); }
    }
  }
  throw std::exception();
}

void foo1() {
  constexpr char filepath[] = "/tmp/data.txt";
  const auto grid = read_grid(filepath);
  std::set<std::pair<long, long>> curr, next;
  curr.insert(find_start(grid));
  for (int i = 0; i < 64; ++i) {
    next.clear();
    constexpr int diffs[4][2] = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};

    for (const auto &[row_cur, col_cur] : curr) {
      for (const auto [dr, dc] : diffs) {
        const auto row = row_cur + dr, col = col_cur + dc;
        if (row < 0 || row >= grid.size()) { continue; }
        if (col < 0 || col >= grid.front().size()) { continue; }
        if (grid[row][col] == '#') { continue; }
        next.insert({row, col});
      }
    }
    curr.swap(next);
  }
  std::cout << "part 1 = " << curr.size() << '\n';
}

int main() {
  foo1();
}
