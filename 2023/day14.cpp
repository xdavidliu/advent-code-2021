#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <algorithm>
#include <map>

void drop_up(std::vector<std::string>& grid, const std::size_t col) {
  std::size_t src = 1, dest = 0;
  while (dest + 1 < grid.size() && src < grid.size()) {
    if (grid[dest][col] != '.') {
      ++dest;
      src = dest + 1;
    } else if (grid[src][col] == '#') {
      dest = src + 1;
      src = dest + 1;
    } else if (grid[src][col] == '.') {
      ++src;
    } else {  // src is 'O'
      grid[dest][col] = 'O';
      grid[src][col] = '.';
      ++dest;
      src = dest + 1;
    }
  }
}

void drop_down(std::vector<std::string>& grid, const std::size_t col) {
  std::size_t src = grid.size() - 2, dest = grid.size() - 1;
  while (dest >= 1 && dest != -1 && dest != -2 && src != -1 && src != -2) {
    if (grid[dest][col] != '.') {
      --dest;
      src = dest - 1;
    } else if (grid[src][col] == '#') {
      dest = src - 1;
      src = dest - 1;
    } else if (grid[src][col] == '.') {
      --src;
    } else {  // src is 'O'
      grid[dest][col] = 'O';
      grid[src][col] = '.';
      --dest;
      src = dest - 1;
    }
  }
}

void all_drop_up(std::vector<std::string>& grid) {
  for (std::size_t col = 0; col < grid.front().size(); ++col) {
    drop_up(grid, col);
  }
}

void all_drop_down(std::vector<std::string>& grid) {
  for (std::size_t col = 0; col < grid.front().size(); ++col) {
    drop_down(grid, col);
  }
}

void drop_left(std::vector<std::string>& grid, const std::size_t row) {
  std::size_t src = 1, dest = 0;
  while (dest + 1 < grid.front().size() && src < grid.front().size()) {
    if (grid[row][dest] != '.') {
      ++dest;
      src = dest + 1;
    } else if (grid[row][src] == '#') {
      dest = src + 1;
      src = dest + 1;
    } else if (grid[row][src] == '.') {
      ++src;
    } else {  // src is 'O'
      grid[row][dest] = 'O';
      grid[row][src] = '.';
      ++dest;
      src = dest + 1;
    }
  }
}

void drop_right(std::vector<std::string>& grid, const std::size_t row) {
  std::size_t src = grid.front().size() - 2, dest = grid.front().size() - 1;
  while (dest >= 1 && dest != -1 && dest != -2 && src != -1 && src != -2) {
    if (grid[row][dest] != '.') {
      --dest;
      src = dest - 1;
    } else if (grid[row][src] == '#') {
      dest = src - 1;
      src = dest - 1;
    } else if (grid[row][src] == '.') {
      --src;
    } else {  // src is 'O'
      grid[row][dest] = 'O';
      grid[row][src] = '.';
      --dest;
      src = dest - 1;
    }
  }
}

void all_drop_left(std::vector<std::string>& grid) {
  for (std::size_t row = 0; row < grid.size(); ++row) {
    drop_left(grid, row);
  }
}

void all_drop_right(std::vector<std::string>& grid) {
  for (std::size_t row = 0; row < grid.size(); ++row) {
    drop_right(grid, row);
  }
}

void print(std::vector<std::string>& grid) {
  for (const auto& line: grid) {
    std::cout << line << '\n';
  }
}

auto tally(const std::vector<std::string>& grid) {
  std::size_t out = 0;
  for (std::size_t row = 0; row < grid.size(); ++row) {
    std::size_t count = 0;
    for (const auto& ch: grid[row]) {
      if (ch == 'O') { ++count; }
    }
    out += count * (grid.size() - row);
  }
  return out;
}

void solve1(std::vector<std::string>& grid) {
  all_drop_up(grid);
  std::cout << "part 1 = " << tally(grid) << '\n';  // 109385
}

auto flatten(const std::vector<std::string> &grid) {
  std::string out;
  out.reserve(grid.size() * grid.front().size());
  for (const auto &line : grid) {
    std::copy(line.cbegin(), line.cend(), std::back_inserter(out));
  }
  return out;
}

auto unflatten(const std::string &flat, const std::size_t cols) {
  std::vector<std::string> out;
  for (std::size_t start = 0; start < flat.size(); start += cols) {
    out.push_back(flat.substr(start, cols));
  }
  return out;
}

void solve2(std::vector<std::string>& grid_after_solve1) {
  auto &grid = grid_after_solve1;
  all_drop_left(grid);
  all_drop_down(grid);
  all_drop_right(grid);
  long cycles = 1, first, second;
  std::map<std::string, long> seen;
  seen[flatten(grid)] = cycles;
  while (true) {
    all_drop_up(grid);
    all_drop_left(grid);
    all_drop_down(grid);
    all_drop_right(grid);
    ++cycles;
    const auto &[iter, succ] = seen.insert({flatten(grid), cycles});
    if (!succ) {  // cycle detected
      first = iter->second;
      second = cycles;
      break;
    }
  }
  const long n = (1000000000L - first) % (second - first) + first;
  const auto found =
      std::find_if(seen.cbegin(), seen.cend(), [n] (decltype(*seen.cbegin()) elem) { return elem.second == n; });
  if (found != seen.cend()) {
    std::cout << "part 2 = " << tally(unflatten(found->first, grid.front().size())) << '\n';
    // 93102
  }
}

void foo1() {
  constexpr char filepath[] = "/tmp/data.txt";
  std::vector<std::string> grid;
  if (auto fs = std::ifstream(filepath)) {
    std::string line;
    while (std::getline(fs, line)) {
      grid.push_back(line);
    }
    solve1(grid);
    solve2(grid);
  } else {
    throw std::exception();
  }
}

int main() {
  foo1();
}
