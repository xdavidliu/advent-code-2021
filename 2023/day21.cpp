#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <exception>
#include <algorithm>
#include <sstream>

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

auto repeat_string(const std::size_t times, const std::string &str) {
    std::ostringstream oss;
    for (std::size_t i = 0; i < times; ++i) {
        oss << str;
    }
    return oss.str();
}

auto repeat_grid(const std::size_t times, const std::vector<std::string> &grid) {
    std::vector<std::string> out;
    const auto func = [times] (const auto &str) { return repeat_string(times, str); };
    std::transform(grid.cbegin(), grid.cend(), std::back_inserter(out), func);
    for (std::size_t ignore = 1; ignore < times; ++ignore) {
        for (std::size_t k = 0; k < grid.size(); ++k) {
            out.push_back(out[k]);
        }
    }
    return out;
}

auto count_oh(const std::vector<std::string> &grid) {
    std::size_t count = 0;
    for (const auto &line : grid) {
        for (const auto ch : line) {
            if (ch == oh) { ++count; }
        }
    }
    return count;
}

void foo1() {
    const auto [empty_grid, start_row, start_col] = get_grid("/home/employee/Documents/temp/data.txt");
    auto cur = empty_grid, next = empty_grid;
    cur[start_row][start_col] = oh;
    constexpr int steps = 100;
    for (int i = 0; i < steps; ++i) {
        one_step(next, cur);
        cur.swap(next);
        copy_in_place(next, empty_grid);
    }
    print_grid(cur, "/home/employee/Documents/temp/out.txt");
}

void foo2() {
    std::vector<std::string> grid{"abc", "def", "ghi"};
    const auto rep = repeat_grid(3, grid);
    print_grid(rep, "/home/employee/Documents/temp/out2.txt");
}

void foo3() {
    const auto [orig_empty_grid, orig_start_row, orig_start_col] = get_grid("/home/employee/Documents/temp/data.txt");
    constexpr std::size_t times = 5;  // has to be odd!
    const auto empty_grid = repeat_grid(times, orig_empty_grid);
    const auto start_row = (times / 2) * orig_empty_grid.size() + orig_start_row;
    const auto start_col = (times / 2) * orig_empty_grid.front().size() + orig_start_col;
    auto cur = empty_grid, next = empty_grid;
    cur[start_row][start_col] = oh;
    constexpr std::size_t steps = 195;
    for (size_t i = 0; i < steps; ++i) {
        one_step(next, cur);
        cur.swap(next);
        copy_in_place(next, empty_grid);
    }
    std::cout << count_oh(cur) << " = count oh\n";
    print_grid(cur, "/home/employee/Documents/temp/out3.txt");
}

int main() {
    foo3();
}
