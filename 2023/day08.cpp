#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string_view>
#include <exception>

void
insert(std::string_view line, std::map<std::string, std::string> &left, std::map<std::string, std::string> &right) {
    const auto key = line.substr(0, line.find(" ="));
    const auto left_val = line.substr(1 + line.find('('), 3);
    const auto right_val = line.substr(2 + line.find(", "), 3);
    left[std::string(key)] = std::string(left_val);
    right[std::string(key)] = std::string(right_val);
}

class Desert {
    const std::string instructions;
    const std::map<std::string, std::string> left, right;
public:
    // https://stackoverflow.com/a/23931920/2990344
    Desert(std::string instructions,
          std::map<std::string, std::string> left,
          std::map<std::string, std::string> right)
            :
            instructions(std::move(instructions)),
            left(std::move(left)),
            right(std::move(right))
            {}
    void update(std::string &place, std::size_t &ind) const {
        switch (instructions[ind]) {
            case 'L' : {
                place = left.at(place);
                break;
            }
            case 'R' : {
                place = right.at(place);
                break;
            }
        }
        ind = (ind + 1) % instructions.size();
    }
    // clang suggested nodiscard; whatever
    [[nodiscard]] std::size_t count_steps_until(std::string_view start, std::string_view end) const {
        std::string place(start);
        std::size_t steps = 0, ind = 0;
        while (place != end) {
            update(place, ind);
            ++steps;
        }
        return steps;
    }
};

Desert read(const char *path) {
    if (auto fs = std::ifstream(path)) {
        std::string instructions, line;
        std::getline(fs, instructions);
        std::getline(fs, line);  // empty
        std::map<std::string, std::string> left, right;
        while (std::getline(fs, line)) { insert(line, left, right); }
        return {std::move(instructions), std::move(left), std::move(right)};
    } else {
        throw std::exception();
    }
}

int main() {
    auto desert = read("/home/xdavidliu/Documents/temp/data.txt");
    std::cout << "part 1 = " << desert.count_steps_until("AAA", "ZZZ") << '\n';  // 19241
}
