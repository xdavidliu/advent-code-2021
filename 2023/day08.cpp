#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string_view>
#include <algorithm>
#include <functional>

void insert(std::string_view line, std::map<std::string, std::string> &left, std::map<std::string, std::string> &right) {
    const auto key = line.substr(0, line.find(" ="));
    const auto left_val = line.substr(1 + line.find('('), 3);
    const auto right_val = line.substr(2 + line.find(", "), 3);
    left[std::string(key)] = std::string(left_val);
    right[std::string(key)] = std::string(right_val);
}

std::size_t count_steps(
        std::string_view instructions,
        std::vector<std::string> &places,
        const std::map<std::string, std::string> &left,
        const std::map<std::string, std::string> &right,
        const std::function<bool(const std::vector<std::string> &)>& done) {
    std::size_t ind = 0, steps = 0;
    while (!done(places)) {
        for (auto &place : places) {
            switch (instructions[ind]) {
                case 'L' : { place = left.at(place); break; }
                case 'R' : { place = right.at(place); break; }
            }
        }
        ind = (ind + 1) % instructions.size();
        ++steps;
    }
    return steps;
}

std::size_t solve1(
        std::string_view instructions,
        const std::map<std::string, std::string> &left,
        const std::map<std::string, std::string> &right) {
    const auto done = [] (const auto &places) { return places.front() == "ZZZ"; };
    std::vector<std::string> places{"AAA"};
    return count_steps(instructions, places, left, right, done);
}

bool all_end_z(const std::vector<std::string> &places) {
    const auto end_z = [] (const std::string &s) { return s.back() == 'Z'; };
    return std::all_of(places.cbegin(), places.cend(), end_z);
}

std::size_t solve2(
        std::string_view instructions,
        const std::map<std::string, std::string> &left,
        const std::map<std::string, std::string> &right) {
    std::vector<std::string> places;
    for (const auto &[key, value] : left) {
        if (key.back() == 'A') { places.push_back(key); }
    }
    return count_steps(instructions, places, left, right, all_end_z);
}

int main() {
    constexpr char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::string instructions, line;
        std::getline(fs, instructions);
        std::getline(fs, line);  // empty
        std::map<std::string, std::string> left, right;
        while (std::getline(fs, line)) { insert(line, left, right); }

        std::cout << "part 1 = " << solve1(instructions, left, right) << '\n';  // 19241
        std::cout << "part 2 = " << solve2(instructions, left, right) << '\n';  // 19241
    }
}
