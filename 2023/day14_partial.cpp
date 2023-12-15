#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <algorithm>

char no_circle(const char ch) {
    return ch == 'O' ? '.' : ch;
}

std::string line_with_no_circles(const std::string &line) {
    std::string out;
    std::transform(line.cbegin(), line.cend(), std::back_inserter(out), no_circle);
    return out;
}

std::vector<std::string> with_no_circles(const std::vector<std::string> &grid) {
    std::vector<std::string> out;
    std::transform(grid.cbegin(), grid.cend(), std::back_inserter(out), line_with_no_circles);
    return out;
}

void drop(const std::vector<std::string> &grid, std::vector<std::string> &other, const std::size_t col) {
    std::size_t src = 1, dest = 0;
    while (true) {  // ???
        if (grid[dest][col] == ) {

        }

    }


}

void solve(const std::vector<std::string> &grid) {
    auto other = with_no_circles(grid);
    for (std::size_t col = 0; col < grid.front().size(); ++col) {
        drop(grid, other, col);
    }
    // todo: sum and print answer
}

void foo1() {
    constexpr char filepath[] = "/home/xdavidliu/Documents/temp/example.txt";
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
    std::cout << grid.size();
}

int main() {
    foo1();
}
