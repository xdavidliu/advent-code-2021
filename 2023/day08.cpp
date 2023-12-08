#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string_view>

void insert(std::string_view line, std::map<std::string, std::string> &left, std::map<std::string, std::string> &right) {
    const auto key = line.substr(0, line.find(" ="));
    const auto left_val = line.substr(1 + line.find('('), 3);
    const auto right_val = line.substr(2 + line.find(", "), 3);
    left[std::string(key)] = std::string(left_val);
    right[std::string(key)] = std::string(right_val);
}

int main() {
    constexpr char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::string instructions, line;
        std::getline(fs, instructions);
        std::getline(fs, line);  // empty
        std::map<std::string, std::string> left, right;
        while (std::getline(fs, line)) { insert(line, left, right); }
        std::size_t ind = 0, steps = 0;
        std::string place = "AAA";
        while (place != "ZZZ") {
            switch (instructions[ind]) {
                case 'L' : { place = left[place]; break; }
                case 'R' : { place = right[place]; break; }
            }
            ind = (ind + 1) % instructions.size();
            ++steps;
        }
        std::cout << "part 1 = " << steps << '\n';  // 19241
    }
}
