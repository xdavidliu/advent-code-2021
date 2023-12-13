#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <exception>

/*
 * idea: collect grid, also grid of counts above and below
 * only check rows where entire row has same number here
 * same for cols
 */

bool row_before_is_mirror(const std::size_t row,
                          const std::vector<std::string>& grid) {
  for (std::size_t col = 0; col < grid.front().size(); ++col) {
    for (std::size_t up = row - 1,
             down = row;  // https://stackoverflow.com/a/27882587/2990344
         up != -1 && down != grid.size();
         --up, ++down) {
      if (grid[down][col] != grid[up][col]) { return false; }
    }
  }
  return true;
}

bool col_before_is_mirror(const std::size_t col,
                          const std::vector<std::string>& grid) {
  for (std::size_t row = 0; row < grid.size(); ++row) {
    for (std::size_t left = col - 1, right = col;
         left != -1 && right != grid.front().size();
         --left, ++right) {
      if (grid[row][left] != grid[row][right]) { return false; }
    }
  }
  return true;
}

auto solve(const std::vector<std::string>& grid) {
  std::size_t out = 0;
  for (std::size_t row = 1; row < grid.size(); ++row) {
    if (row_before_is_mirror(row, grid)) {
      out += 100 * row;
    }
  }
  for (std::size_t col = 1; col < grid.front().size(); ++col) {
    if (col_before_is_mirror(col, grid)) {
      out += col;
    }
  }
  return out;
}

auto read_grids(const char* filepath) {
  if (auto fs = std::ifstream(filepath)) {
    std::vector<std::vector<std::string>> grids;
    std::string line;
    while (fs) {
      std::vector<std::string> grid;
      while (std::getline(fs, line) && !line.empty()) {
        grid.push_back(line);
      }
      grids.push_back(grid);
    }
    return grids;
  } else {
    throw std::exception();
  }
}

int main() {
  constexpr char filepath[] = "/tmp/data.txt";
  const auto grids = read_grids(filepath);
  std::size_t part1 = 0;
  for (const auto &grid : grids) {
    part1 += solve(grid);
  }
  std::cout << "part 1 = " << part1 << '\n';  // 28651
}
