#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string_view>
#include <exception>
#include <vector>
#include <numeric>

void
insert(std::string_view line, std::map<std::string, std::string> &left, std::map<std::string, std::string> &right) {
    const auto key = line.substr(0, line.find(" ="));
    const auto left_val = line.substr(1 + line.find('('), 3);
    const auto right_val = line.substr(2 + line.find(", "), 3);
    left[std::string(key)] = std::string(left_val);
    right[std::string(key)] = std::string(right_val);
}

void my_assert(bool cond) {
  if (!cond) {
    throw std::exception();
  }
}

std::size_t lcm(const std::vector<std::size_t> &ns) {
  const auto op = [] (const std::size_t a, const std::size_t b) { return std::lcm(a, b); };
  return std::accumulate(ns.cbegin(), ns.cend(), 1, op);
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
    void part2() const {
        std::vector<std::size_t> periods;
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
                    my_assert(z_steps.size() == 1);  // data happens to have this
                    const auto period = steps - iter->second;
                    my_assert(z_steps.front() == period);  // data also happens to have this
                    periods.push_back(period);
                    break;
                }
            }
        }
        std::cout << "for part 2, take these and find LCM: ";
        for (const auto &p : periods) {
          std::cout << p << ' ';
        }
        std::cout << "\nthat's 9606140307013\n";
        // gives wrong result because it probably uses formula
        //   lcm(a, b) = a b / gcd(a, b)
        // and in the process it overflows even std::size_t.
        // std::cout << "part 2 = " << lcm(periods) << '\n';

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
    auto desert = read("/tmp/data.txt");
    std::cout << "part 1 = " << desert.count_steps_until("AAA", "ZZZ") << '\n';  // 19241
    desert.part2();
    //                   iter->second   steps      z_steps.front()
    // start AAA, cycle found at 2 and 19243, with z at 19241
    // start BBA, cycle found at 2 and 21411, with z at 21409
    // start BLA, cycle found at 4 and 11657, with z at 11653
    // start DRA, cycle found at 4 and 14367, with z at 14363
    // start NFA, cycle found at 2 and 12739, with z at 12737
    // start PSA, cycle found at 3 and 15992, with z at 15989
    //
    // interesting that it only found one z value for each!
    // also interesting, the z above is exactly equal to the period, which is
    // difference between first two numbers. That means there's a z at every
    // simple multiple. So just find LCM of all of the Z's here and you're done.
}
