#include <iostream>
#include <vector>
#include <string>
#include <exception>
#include <fstream>
#include <algorithm>
#include <cmath>

// hypothesis; it's just manhattan distance

bool is_dot(const char ch) { return ch == '.'; }

long count_between(const std::size_t left, const std::size_t right, const std::vector<std::size_t> &indices) {
  if (left > right) {
    return count_between(right, left, indices);
  }
  const auto first_empty_between = std::upper_bound(indices.cbegin(), indices.cend(), left);
  const auto first_empty_after = std::upper_bound(indices.cbegin(), indices.cend(), right);
  return static_cast<long>(first_empty_after - first_empty_between);
}

std::vector<std::string> read_grid(const char *filepath) {
  if (auto fs = std::ifstream(filepath)) {
    std::vector<std::string> grid;
    std::string line;
    while (std::getline(fs, line)) {
      grid.push_back(line);
    }
    return grid;
  } else {
    throw std::exception();
  }
}

std::vector<std::size_t> find_empty_rows(const std::vector<std::string> &grid) {
  std::vector<std::size_t> empty_rows;
  for (std::size_t r = 0; r < grid.size(); ++r) {
    if (std::all_of(grid[r].cbegin(), grid[r].cend(), is_dot)) {
      empty_rows.push_back(r);
    }
  }
  return empty_rows;
}

std::vector<std::size_t> find_empty_cols(const std::vector<std::string> &grid) {
  std::vector<std::size_t> empty_cols;
  for (std::size_t c = 0; c < grid.front().size(); ++c) {
    bool col_empty = true;
    for (std::size_t r = 0; r < grid.size(); ++r) {
      if (!is_dot(grid[r][c])) {
        col_empty = false;
        break;
      }
    }
    if (col_empty) {
      empty_cols.push_back(c);
    }
  }
  return empty_cols;
}

std::vector<std::pair<std::size_t, std::size_t>> find_galaxies(const std::vector<std::string> &grid) {
  std::vector<std::pair<std::size_t, std::size_t>> galaxies;
  for (std::size_t r = 0; r < grid.size(); ++r) {
    for (std::size_t c = 0; c < grid.front().size(); ++c) {
      if (grid[r][c] == '#') {
        galaxies.emplace_back(r, c);
      }
    }
  }
  return galaxies;
}

int main() {
  const auto grid = read_grid("/tmp/data.txt");
  const auto empty_rows = find_empty_rows(grid);
  const auto empty_cols = find_empty_cols(grid);
  const auto galaxies = find_galaxies(grid);
  long part1 = 0;
  for (std::size_t i = 0; i+1 < galaxies.size(); ++i) {
    const auto [ri, ci] = galaxies[i];
    for (std::size_t k = i+1; k < galaxies.size(); ++k) {
      const auto [rk, ck] = galaxies[k];
      part1 += std::abs(static_cast<long>(rk) - static_cast<long>(ri));
      part1 += std::abs(static_cast<long>(ck) - static_cast<long>(ci));
      part1 += count_between(ri, rk, empty_rows);
      part1 += count_between(ci, ck, empty_cols);
    }
  }
  std::cout << "part 1 = " << part1 << '\n';  // 9599070
}
