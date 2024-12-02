#include <iostream>
#include <fstream>
#include <string_view>
#include <map>

int value1(std::string_view word) {
    constexpr std::string_view digits = "0123456789";
    const auto first = word[word.find_first_of(digits)] - '0';
    const auto second = word[word.find_last_of(digits)] - '0';
    return 10 * first + second;
}

const std::map<std::string_view, int> numbers = {
    {"zero", 0}, {"one", 1}, {"two", 2}, {"three", 3}, {"four", 4},
    {"five", 5}, {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9},
    {"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4},
    {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9},
};

int value2(std::string_view word) {
    std::size_t left = word.size();
    int left_val = 0;
    for (const auto &[key, value] : numbers) {
        const auto found = word.find(key);
        if (found != std::string_view::npos && left > found) {
            left = found;
            left_val = value;
        }
    }
    std::size_t right = 0;
    // horrible gotcha above: if final right is ACTUALLY 0, it
    // never updates. So need to use -1 for right_value to detect this.
    // can't use negative for right because it's a size_t
    int right_val = -1;
    for (const auto &[key, value] : numbers) {
        const auto found = word.rfind(key);
        if (found != std::string_view::npos && (right_val == -1 || right < found)) {
            right = found;
            right_val = value;
        }
    }
    return 10 * left_val + right_val;
}

int main() {
    if (auto fs = std::ifstream("/home/xdavidliu/Documents/temp/data.txt")) {
        std::string word;
        int sum1 = 0;
        int sum2 = 0;
        while (fs >> word) {
            sum1 += value1(word);
            sum2 += value2(word);
        }
        std::cout << "part 1 = " << sum1 << '\n';  // 55172
        std::cout << "part 2 = " << sum2 << '\n';  // 54925
    }
}
