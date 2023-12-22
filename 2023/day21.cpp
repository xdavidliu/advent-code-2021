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
    if (times == 1) { return grid; }
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

auto run_simulation(const std::size_t k, std::size_t steps) {
    if (k > 5) { return std::size_t{0}; }  // avoid OOMing
    const auto [orig_empty_grid, orig_start_row, orig_start_col] = get_grid("/home/employee/Documents/temp/data.txt");
    if (steps == 0) { steps = 65 + 131 * k; }
    const auto times = 2 * k + 1;
    const auto empty_grid = repeat_grid(times, orig_empty_grid);
    const auto start_row = (times / 2) * orig_empty_grid.size() + orig_start_row;
    const auto start_col = (times / 2) * orig_empty_grid.front().size() + orig_start_col;
    auto cur = empty_grid, next = empty_grid;
    cur[start_row][start_col] = oh;
    for (size_t i = 0; i < steps; ++i) {
        one_step(next, cur);
        cur.swap(next);
        copy_in_place(next, empty_grid);
    }
    return count_oh(cur);
//    print_grid(cur, "/home/employee/Documents/temp/out3.txt");
}

// see notes below
long part2() {
    long k = 0, count = 3832, diff = 30135;
    while (k != 202300) {
        count += diff;
        diff += 29954;
        ++k;
    }
    return count;
}

int main() {
    const auto part1 = run_simulation(0, 64);
    std::cout << "part 1 = " << part1 << '\n';  // 3649
    // std::cout << run_simulation(1, 0);  // for part 2
    std::cout << "part 2 = " << part2() << '\n';
}

// print out grid for various values of times and steps above
// note that there's an O at very top for steps = 65 + 131 k where k = 0, 1, 2 ...
// note that 26501365 is of this form, with k = 202300
// hence, find formula for count_oh for any given k
// k = 0 -> 3832
//   30135
// k = 1 -> 33967
//   60089
// k = 2 -> 94056
//   90043
// k = 3 -> 184099
//   119997
// k = 4 -> 304096
//   149951
// k = 5 -> 454047
//
// note the diffs of diffs are all 29954
