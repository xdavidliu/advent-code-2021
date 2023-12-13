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

std::vector<std::vector<int>> zeros(const int rows, const int cols) {
    std::vector<int> zero_row(cols);
    return std::vector<std::vector<int>>(rows, zero_row);
}

constexpr char rock = '#';

auto count_down(const std::vector<std::string> &grid) {
    auto out = zeros(grid.size(), grid.front().size());
    for (int c = 0; c < grid.front().size(); ++c) {
        int count = 0;
        for (int r = 0; r < grid.size(); ++r) {
            if (grid[r][c] == rock) {
                ++count;
            }
            out[r][c] = count;
        }
    }
    return out;
}

auto count_up(const std::vector<std::string> &grid) {
    auto out = zeros(grid.size(), grid.front().size());
    for (int c = 0; c < grid.front().size(); ++c) {
        int count = 0;
        for (int r = grid.size() - 1; r != -1; --r) {
            if (grid[r][c] == rock) {
                ++count;
            }
            out[r][c] = count;
        }
    }
    return out;
}

// todo: count left, count right

auto read_grids(const char *filepath) {
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
    constexpr char filepath[] = "/home/xdavidliu/Documents/temp/example.txt";
    const auto grids = read_grids(filepath);
    std::cout << grids.size() << ' ' << grids[0].size() << ' ' << grids[1].size();
}
