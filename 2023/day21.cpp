#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <exception>

constexpr char oh = 'O';
constexpr char garden = '.';

auto get_grid(const char *filename) {
    auto fs = std::ifstream(filename);
    std::vector<std::string> grid;
    std::string line;
    while (std::getline(fs, line)) {
        grid.push_back(line);
    }
    for (auto row = grid.begin(); row != grid.end(); ++row) {
        const auto col = row->find('S');
        if (col != std::string::npos) {
            row->at(col) = '.';
            return std::make_tuple(grid, row - grid.begin(), static_cast<long>(col));
        }
    }
    throw std::exception();
}

void print_grid(const std::vector<std::string> &grid, const char *filename) {
    auto ofs = std::ofstream(filename);
    for (const auto &elem : grid) {
        ofs << elem << '\n';
    }
}

void copy_in_place(std::vector<std::string> &dest, const std::vector<std::string> &src) {
    auto dit = dest.begin();
    auto sit = src.cbegin();
    for (; sit != src.cend(); ++dit, ++sit) {
        std::copy(sit->cbegin(), sit->cend(), dit->begin());
    }
}

void one_step(std::vector<std::string> &dest, const std::vector<std::string> &src) {
    for (std::size_t r = 0; r < src.size(); ++r) {
        for (std::size_t c = 0; c < src.front().size(); ++c) {
            if (src[r][c] != oh) { continue; }
            if (r != 0 && dest[r-1][c] == garden) { dest[r-1][c] = oh; }
            if (r+1 != src.size() && dest[r+1][c] == garden) { dest[r+1][c] = oh; }
            if (c != 0 && dest[r][c-1] == garden) { dest[r][c-1] = oh; }
            if (c+1 != src.front().size() && dest[r][c+1] == garden) { dest[r][c+1] = oh; }
        }
    }
}

void foo1() {
    const auto [empty_grid, start_row, start_col] = get_grid("/home/employee/Documents/temp/data.txt");
    auto cur = empty_grid, next = empty_grid;
    cur[start_row][start_col] = oh;
    constexpr int steps = 60;
    for (int i = 0; i < steps; ++i) {
        one_step(next, cur);
        cur.swap(next);
        copy_in_place(next, empty_grid);
    }
    print_grid(cur, "/home/employee/Documents/temp/out.txt");
}

int main() {
    foo1();
}
