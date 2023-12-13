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

bool row_before_is_mirror(const std::size_t row, const std::vector<std::string> &grid) {
    for (std::size_t col = 0; col < grid.front().size(); ++col) {
        for (std::size_t left = row - 1, right = row;  // https://stackoverflow.com/a/27882587/2990344
             left != -1 && right != grid.size();
             --left, ++right) {
            if (grid[left][col] != grid[right][col]) { return false; }
        }
    }
    return true;
}

// todo: make sure not to call with empty left or right
// make sure problem statement says empty does not count
// col before is mirror

auto solve(const std::vector<std::string> &grid) {

}

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
