#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <algorithm>

void drop(std::vector<std::string> &grid, const std::size_t col) {
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

void print(std::vector<std::string> &grid) {
    for (const auto &line : grid) {
        std::cout << line << '\n';
    }
}

auto tally(std::vector<std::string> &grid) {
    std::size_t out = 0;
    for (std::size_t row = 0; row < grid.size(); ++row) {
        std::size_t count = 0;
        for (const auto &ch : grid[row]) {
            if (ch == 'O') { ++count; }
        }
        out += count * (grid.size() - row);
    }
    return out;
}

void solve(std::vector<std::string> &grid) {
    for (std::size_t col = 0; col < grid.front().size(); ++col) {
        drop(grid, col);
    }
    std::cout << "part 1 = " << tally(grid) << '\n';  // 109385
}

void foo1() {
    constexpr char filepath[] = "/home/xdavidliu/Documents/temp/data.txt";
    std::vector<std::string> grid;
    if (auto fs = std::ifstream(filepath)) {
        std::string line;
        while (std::getline(fs, line)) {
            grid.push_back(line);
        }
        solve(grid);
    } else {
        throw std::exception();
    }
}

int main() {
    foo1();
}
