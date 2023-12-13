#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <iterator>
#include <algorithm>

constexpr int none = -1;

void collect(
        long &total,
        const std::string &grid,
        int i_grid,
        const std::vector<int> &sizes,
        int i_size,
        int remain)
{
    char ch;
    while (i_size != sizes.size() && i_grid != grid.size() && '?' != (ch = grid[i_grid])) {
        ++i_grid;
        if (ch == '.') {
            if (remain == none) {
                // do nothing
            } else if (remain == 0) {  // just finished a batch of #
                ++i_size;
                remain = none;
            } else {  // remain > 0
                return;  // impossible, so terminate
            }
        } else {  // ch == '#'
            if (remain == none) {
                remain = sizes[i_size] - 1;
            } else if (remain == 0) {
                return;  // impossible
            } else {  // remain > 0
                --remain;
            }
        }
    }
    if (i_size == sizes.size()) {
        for (int i = i_grid; i < grid.size(); ++i) {
            if (grid[i] == '#') { return; }
        }
        ++total;
    } else if (i_grid == grid.size()) {
        if ( remain == 0 && i_size + 1 == sizes.size()) { ++total; }
    } else {  // ch == '?' and haven't finished yet
        // try ? = #
        if (remain == none) {
            const auto new_remain = sizes[i_size] - 1;
            collect(total, grid, 1 + i_grid, sizes, i_size, new_remain);
        } else if (remain > 0) {
            collect(total, grid, 1 + i_grid, sizes, i_size, remain - 1);
        }
        // try ? = .
        if (remain == 0) {
            collect(total, grid, 1 + i_grid, sizes, 1 + i_size, none);
        } else if (remain == none) {
            collect(total, grid, 1 + i_grid, sizes, i_size, none);
        }
    }
}

long solve(const std::string &grid, const std::vector<int> &sizes) {
    long total = 0;
    collect(total, grid, 0, sizes, 0, none);
    return total;
}

struct Problem {
    std::string grid;
    std::vector<int> nums;
};

auto split_nums(const std::string &list) {
    std::size_t i = 0;
    std::vector<int> nums;
    while (true) {
        const auto k = list.find(',', i);
        if (k == list.npos) {
            nums.push_back(std::stoi(list.substr(i)));
            break;
        } else {
            nums.push_back(std::stoi(list.substr(i, k-i)));
            i = k + 1;
        }
    }
    return nums;
}

auto times_five_with_questions(const std::string &text) {
    std::string out = text;
    for (int i = 0; i < 4; ++i) {
        out.push_back('?');
        std::copy(text.cbegin(), text.cend(), std::back_inserter(out));
    }
    return out;
}

auto times_five(const std::vector<int> &sizes) {
    std::vector<int> out = sizes;
    for (int i = 0; i < 4; ++i) {
        std::copy(sizes.cbegin(), sizes.cend(), std::back_inserter(out));
    }
    return out;
}

void foo1() {
    std::vector<Problem> problems;
    if (auto fs = std::ifstream("/home/xdavidliu/Documents/temp/data.txt")) {
        long part1 = 0, part2 = 0;
        std::string grid, nums;
        while (fs >> grid) {
            fs >> nums;
            const auto sizes = split_nums(nums);
            part1 += solve(grid, sizes);
            const auto five_grid = times_five_with_questions(grid);
            const auto five_sizes = times_five(sizes);
            std::cout << "working on " << grid << '\n';
            part2 += solve(five_grid, five_sizes);
        }
        std::cout << "part 1 = " << part1 << '\n';
        std::cout << "part 2 = " << part2 << '\n';
    }
}

void foo2() {
    const std::string grid = "?###????????";
    const std::vector<int> sizes = {3, 2, 1};
    std::cout << solve(times_five_with_questions(grid), times_five(sizes)) << '\n';
}

int main() {
    foo1();
}
