#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <vector>
#include <fstream>
#include <iomanip>
#include <boost/multiprecision/cpp_int.hpp>
#include <boost/multiprecision/cpp_bin_float.hpp>
#include <boost/rational.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <boost/numeric/ublas/lu.hpp>

// in fedora, sudo dnf install boost boost-devel
using ::boost::multiprecision::int128_t;
using ::boost::multiprecision::cpp_bin_float_100;
using ::boost::rational;
using ::boost::numeric::ublas::matrix;
using ::boost::numeric::ublas::vector;
using ::boost::numeric::ublas::lu_factorize;
using ::boost::numeric::ublas::lu_substitute;
using ::boost::numeric::ublas::permutation_matrix;

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

void do_part1(const std::vector<Stone> &things) {
    // data happens to have no verticals, i.e. vy != 0
    int part1 = 0;
    for (int i = 0; i + 1 < things.size(); ++i) {
        for (int k = i + 1; k < things.size(); ++k) {
            part1 += will_intersect_xy(things[i], things[k]);
        }
    }
    std::cout << "part 1 = " << part1 << '\n';  // 16172
}

class Collect {
    std::map<long, std::vector<int>> the_map;
public:
    void increment(const long x, int i) {
        auto [iter, ignore] = the_map.insert({x, std::vector<int>()});
        iter->second.push_back(i);
    }
    void show(std::ofstream &ofs) const {
        for (const auto &[key, vals]: the_map) {
            if (vals.size() > 1) {
                ofs << key << ": ";
                for (const auto &val : vals) {
                    ofs << val << ' ';
                }
                ofs << '\n';
            }
        }
        ofs << '\n';
    }
};

void foo2() {
    const auto things = parse_file();
    std::ofstream ofs("/home/employee/Documents/temp/out.txt");
    Collect c_vx, c_vy, c_vz;
    for (int i = 0; i < things.size(); ++i) {
        const auto &a = things[i];
        c_vx.increment(a.vx, i);
        c_vy.increment(a.vy, i);
        c_vz.increment(a.vz, i);
    }
    c_vx.show(ofs);
}

void foo3() {
    cpp_bin_float_100 x = 209773510765693L;
    std::cout << std::setprecision(15) << x << '\n';
    std::cout << std::numeric_limits<cpp_bin_float_100>::max() << '\n';
    std::cout << sizeof(cpp_bin_float_100) << '\n';
}

void foo4() {
    // https://www.boost.org/doc/libs/1_82_0/libs/numeric/ublas/doc/matrix.html#matrix
    matrix<cpp_bin_float_100> mat(2, 2);
    mat(0, 0) = 2;
    mat(0, 1) = 1;
    mat(1, 0) = 1;
    mat(1, 1) = 1;
    vector<cpp_bin_float_100> vec(2);
    vec(0) = -1;
    vec(1) = 1;
    permutation_matrix<std::size_t> pm(mat.size1());
    lu_factorize(mat, pm);
    lu_substitute(mat, pm, vec);
    std::cout << vec(0) << ", " << vec(1) << '\n';
    // https://stackoverflow.com/a/1297730/2990344
}

void populate_rhs(const std::vector<Stone> &stones, const vector<cpp_bin_float_100> &p, vector<cpp_bin_float_100> &f) {
    for (int i = 0; i < 3; ++i) {
        const auto &s = stones[i];
        f(i * 3) = p(0) - stones[i].x + p(6) * (p(3) - s.vx);
        f(i * 3 + 1) = p(1) - stones[i].y + p(7) * (p(4) - s.vy);
        f(i * 3 + 2) = p(2) - stones[i].z + p(8) * (p(5) - s.vz);
    }
}

void populate_jacob(const std::vector<Stone> &stones, const vector<cpp_bin_float_100> &p, matrix<cpp_bin_float_100> &j) {
    for (int i = 0; i < 9; ++i) {
        for (int k = 0; k < 9; ++k) { j(i, k) = 0; }
    }
    for (int i = 0; i < 3; ++i) {
        const auto &s = stones[i];
        const long sv[] = {s.vx, s.vy, s.vz};
        for (int k = 0; k < 3; ++k) {
            j(3 * i + k, k) = 1;  // d/dx
            j(3 * i + k, 3 + k) = p(6 + i);  // d/dv
            j(3 * i + k, 6 + i) = p(3 + k) - sv[k];  // d/dt
        }
    }
}

auto norm_sq(const vector<cpp_bin_float_100> &p) {
    cpp_bin_float_100 out = 0;
    for (const auto &x : p) {
        out += x * x;
    }
    return out;
}

void do_part2(const std::vector<Stone> &stones) {
    vector<cpp_bin_float_100> p(9), dp(9), f(9);
    matrix<cpp_bin_float_100> j(9, 9);
    // random guess
    p[0] = 224164924449606L;
    p[1] = 373280170830371L;
    p[2] = 280954002548352L;
    p[3] = 30; p[4] = 12; p[5] = -200;
    p[6] = 3; p[7] = 8; p[8] = 5;
    for (int i = 0; i < 50; ++i) {
        populate_rhs(stones, p, f);
        std::cout << norm_sq(f) << '\n';
        populate_jacob(stones, p, j);
        dp = -f;
        permutation_matrix<std::size_t> pm(j.size1());
        lu_factorize(j, pm);
        lu_substitute(j, pm, dp);
        p += dp;
    }
    // use negative for rhs
}

int main() {
    const auto things = parse_file();
    // do_part1(things);
    do_part2(things);
}
