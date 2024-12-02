#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <utility>

bool is_nonzero(const long x) { return x != 0; }

std::pair<long, long> first_last_value(const std::vector<long> &row) {
    std::vector<std::vector<long>> rows;
    rows.push_back(row);
    while (std::any_of(rows.back().cbegin(), rows.back().cend(), is_nonzero)) {
        std::vector<long> next;
        // https://stackoverflow.com/q/65883787/2990344
        // cannot use std::adjacent_difference
        // quite surprising behavior; it copies over first element.
        for (auto left = rows.back().cbegin(), right = left + 1;
        left != rows.back().cend() && right != rows.back().cend();
        ++left, ++right) {
            next.push_back(*right - *left);
        }
        rows.push_back(std::move(next));
    }
    long first = 0, last = 0;
    for (auto iter = rows.crbegin(); iter != rows.crend(); ++iter) {
        first = iter->front() - first;
        last += iter->back();
    }
    return {first, last};
}

int main() {
    constexpr char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::string line;
        long part1 = 0, part2 = 0;
        while (std::getline(fs, line)) {
            std::istringstream iss(line);
            long val;
            std::vector<long> row;
            while (iss >> val) {
                row.push_back(val);
            }
            const auto values = first_last_value(row);
            part1 += values.second;
            part2 += values.first;
        }
        std::cout << "part 1 = " << part1 << '\n';  // 1995001648
        std::cout << "part 2 = " << part2 << '\n';  // 988
    }
}
