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

class State {
    const std::string instructions;
    std::string place;
    const std::map<std::string, std::string> left, right;
    std::size_t ind;
public:
    // https://stackoverflow.com/a/23931920/2990344
    State(std::string instructions,
          std::string place,
          std::map<std::string, std::string> left,
          std::map<std::string, std::string> right)
            :
            instructions(std::move(instructions)),
            place(std::move(place)),
            left(std::move(left)),
            right(std::move(right)),
            ind(0) {}

    std::size_t count_steps() {
        std::size_t steps = 0;
        while (place != "ZZZ") {
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
            ++steps;
        }
        return steps;
    }
};

State read(const char *path) {
    if (auto fs = std::ifstream(path)) {
        std::string instructions, line;
        std::getline(fs, instructions);
        std::getline(fs, line);  // empty
        std::map<std::string, std::string> left, right;
        while (std::getline(fs, line)) { insert(line, left, right); }
        return {std::move(instructions), "AAA", std::move(left), std::move(right)};
    } else {
        throw std::exception();
    }
}

int main() {
    auto state = read("/home/xdavidliu/Documents/temp/data.txt");
    std::cout << "part 1 = " << state.count_steps() << '\n';  // 19241
}
