#include <iostream>
#include <fstream>
#include <string>
#include <string_view>
#include <cstddef>

int solve(const std::string_view s, const std::size_t d) {
    int sum = 0;
    for (std::size_t i = 0; i < s.size(); ++i) {
        const auto a = s[i], b = s[(i+d)%s.size()];
        if (a == b) sum += a - '0';
    }
    return sum;
}

int main() {
    if (auto fin = std::ifstream("/home/xdavidliu/Documents/temp/day01.txt")) {
        std::string s;
        fin >> s;
        std::cout << "part 1 = " << solve(s, 1) << '\n';
        std::cout << "part 2 = " << solve(s, s.size() / 2) << '\n';
    }
}
