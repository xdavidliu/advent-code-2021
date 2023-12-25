#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <fstream>
#include <boost/multiprecision/cpp_int.hpp>
#include <boost/rational.hpp>

// in fedora, sudo dnf install boost boost-devel
using ::boost::multiprecision::int128_t;
using ::boost::rational;

constexpr char filename[] = "/home/employee/Documents/temp/data.txt";
constexpr long low_bound = 200000000000000L;
constexpr long up_bound = 400000000000000L;

struct Stone {
    long x, y, z, vx, vy, vz;
};

Stone parse_line(const std::string &line) {
     std::istringstream iss(line);
     std::vector<long> vals;
     char ch;
     long val;
     for (int i = 0; i < 6; ++i) {
         iss >> val >> ch;
         vals.emplace_back(val);
     }
     return {vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]};
}

auto parse_file() {
    auto fs = std::ifstream(filename);
    std::string line;
    std::vector<Stone> things;
    while (std::getline(fs, line)) {
        things.push_back(parse_line(line));
    }
    return things;
}

template <typename T> int sign(const T &t) {
    if (t < 0) { return -1; }
    else if (t > 0) { return 1; }
    else { return 0; }
}

bool will_intersect_xy(const Stone &p1, const Stone &p2) {
    const auto m1 = rational<int128_t>(p1.vy, p1.vx);
    const auto m2 = rational<int128_t>(p2.vy, p2.vx);
    if (m1 == m2) { return false; }
    const auto b1 = p1.y - m1 * p1.x;
    const auto b2 = p2.y - m2 * p2.x;
    const auto x3 = (b2 - b1) / (m1 - m2);
    if (!(low_bound <= x3 && x3 <= up_bound)) { return false; }
    if (sign(p1.vx) != sign(x3 - p1.x)) { return false; }
    if (sign(p2.vx) != sign(x3 - p2.x)) { return false; }
    const auto y3 = m1 * x3 + b1;
    if (!(low_bound <= y3 && y3 <= up_bound)) { return false; }
    if (sign(p1.vy) != sign(y3 - p1.y)) { return false; }
    if (sign(p2.vy) != sign(y3 - p2.y)) { return false; }
    return true;
}

int main() {
    const auto things = parse_file();
    // data happens to have no verticals, i.e. vy != 0
    int part1 = 0;
    for (int i = 0; i + 1 < things.size(); ++i) {
        for (int k = i + 1; k < things.size(); ++k) {
            part1 += will_intersect_xy(things[i], things[k]);
        }
    }
    std::cout << "part 1 = " << part1 << '\n';  // 16172
}
