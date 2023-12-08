#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string_view>
#include <exception>
#include <vector>

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
    void update(std::string &place, const std::size_t steps) const {
        switch (instructions[steps % instructions.size()]) {
            case 'L' : {
                place = left.at(place);
                break;
            }
            case 'R' : {
                place = right.at(place);
                break;
            }
        }
    }
    // clang suggested nodiscard; whatever
    [[nodiscard]] std::size_t count_steps_until(std::string_view start, std::string_view end) const {
        std::string place(start);
        std::size_t steps = 0;
        while (place != end) {
            update(place, steps);
            ++steps;
        }
        return steps;
    }
    void foo() const {
        for (const auto &[key, value] : left) {
            if (key.back() != 'A') { continue; }
            std::string place(key);
            std::map<std::pair<std::string, std::size_t>, std::size_t> seen;
            std::size_t steps = 0;
            seen[std::make_pair(place, steps % instructions.size())] = steps;
            std::vector<std::size_t> z_steps;
            while (true) {
                update(place, steps);
                ++steps;
                if (place.back() == 'Z') { z_steps.push_back(steps); }
                // https://stackoverflow.com/a/1409465/2990344
                // https://en.cppreference.com/w/cpp/container/map/insert
                auto [iter, succ] = seen.insert({{place, steps % instructions.size()}, steps});
                if (!succ) {
                    std::cout << "start " << key << ", cycle found at " << iter->second;
                    std::cout << " and " << steps << ", with z at ";
                    for (const auto &x : z_steps) {
                        std::cout << x << ' ';
                    }
                    std::cout << '\n';
                    break;
                }
            }
        }
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
    desert.foo();
}
