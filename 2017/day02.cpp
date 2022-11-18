#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

std::vector<int> split(const std::string &s) {
    std::istringstream strm(s);
    std::vector<int> vec;
    int x;
    while (strm >> x) {
        vec.push_back(x);
    }
    return vec;
}

int divide(const std::vector<int> &vec) {
    for (auto x : vec) {
        for (auto y : vec) {
            if (x > y && x % y == 0) {
                return x / y;
            } else if (y > x && y % x == 0) {
                return y / x;
            }
        }
    }
    return 0;
}

int main() {
    if (auto fin = std::ifstream("/home/xdavidliu/Downloads/day02.txt")) {
        std::string ln;
        int part1 = 0;
        int part2 = 0;
        while (std::getline(fin, ln)) {
            auto vec = split(ln);
            part1 += *std::max_element(vec.begin(), vec.end());
            part1 -= *std::min_element(vec.begin(), vec.end());
            part2 += divide(vec);
        }
        std::cout << part1 << " = part1\n";
        std::cout << part2 << " = part2\n";
    }
}
