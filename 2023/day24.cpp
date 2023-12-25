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

/*
 * x = x0 + vx t
 * y = y0 + vy t
 * z = z0 + vz t
 *
 * let a, b, and c be positions of the single one.
 * a = a0 + va t
 * b = b0 + vb t
 * c = c0 + vc t
 *
 * let ti be the collision time for i = 0, 1, 2, 3 ... N-1
 *
 * x0i + vxi ti = a0 + va ti
 * y0i + vyi ti = b0 + vb ti
 * z0i + vzi ti = c0 + vc ti
 *
 * x0i - a0 = (va - vxi) ti
 * y0i - b0 = (vb - vyi) ti
 *
 * (x0i - a0) (vb - vyi) = (y0i - b0) (va - vxi)
 *
 * x0i vb - x0i vyi - a0 vb + a0 vyi = y0i va - y0i vxi - b0 va + b0 vxi
 *
 * swap i with k, subtract; quadratic terms a0 vb and b0 va
 * drop out
 *
 * (x0i - x0k) vb - x0i vyi + x0k vyk + a0 (vyi - vyk)
 * = (y0i - y0k) va - y0i vxi + y0k vxk + b0 (vxi - vxk)
 *
 * rearrange into linear equation in a0, b0, va, vb
 *
 * a0 (vyi - vyk) + b0 (vxk - vxi) + (y0k - y0i) va + (x0i - x0k) vb
 * = y0k vxk - y0i vxi + x0i vyi - x0k vyk
 *
 * repeat for any four distinct i-k pairs to get four equations
 *
 * solve, then get ti = (x0i - a0) / (va - vxi) for any i
 * now for c, need to solve another 2 x 2 equation
 *
 * c0 + vc ti = z0i + vzi ti
 * c0 + vc tk = z0k + vzk tk
 *
 * c0 tk + vc ti tk = z0i tk + vzi ti tk
 * c0 ti + vc ti tk = z0k ti + vzk ti tk
 *
 * c0 = (z0i tk + vzi ti tk - z0k ti - vzk ti tk) / (tk - ti)
 *
 * for any two i and k. Update: make sure not to pick i = 0123 and k = 1230
 * otherwise will only have rank 3. Must have rank 4 to be solvable.
 */

void do_part2(const std::vector<Stone> &things) {
    // pick whatever, just make sure not the same
    // oh wow, if you put k_vals = 1, 2, 3, 0; it's actually linearly dependent,
    // so you get singular. Interesting, but makes sense!
    std::vector<int> i_vals = {0, 1, 2, 3}, k_vals = {1, 2, 3, 4};
    // update: probably don't even need oct; bin100 prob good enough. Maybe even double.
    matrix<cpp_bin_float_100> mat(4, 4);
    vector<cpp_bin_float_100> vec(4);
    const cpp_bin_float_100 one = 1;
    for (int r = 0; r < 4; ++r) {
        const auto &si = things[i_vals[r]], &sk = things[k_vals[r]];
        const auto x0i = si.x, y0i = si.y, x0k = sk.x, y0k = sk.y;
        const auto vxi = si.vx, vyi = si.vy, vxk = sk.vx, vyk = sk.vy;
        mat(r, 0) = vyi - vyk;
        mat(r, 1) = vxk - vxi;
        mat(r, 2) = y0k - y0i;
        mat(r, 3) = x0i - x0k;
        vec(r) = one * y0k * vxk - one * y0i * vxi + one * x0i * vyi - one * x0k * vyk;
    }
    permutation_matrix<std::size_t> pm(mat.size1());
    lu_factorize(mat, pm);
    lu_substitute(mat, pm, vec);
    const auto a0 = vec(0), b0 = vec(1), va = vec(2), vb = vec(3);
    // ti = (x0i - a0) / (va - vxi)
    const auto ti = (things[0].x - a0) / (va - things[0].vx);
    const auto tk = (things[1].x - a0) / (va - things[1].vx);
    // (z0i tk + vzi ti tk - z0k ti - vzk ti tk) / (tk - ti)
    const auto z0i = things[0].z, z0k = things[1].z;
    const auto vzi = things[0].vz, vzk = things[1].vz;
    const auto c0 = (z0i * tk + vzi * ti * tk - z0k * ti - vzk * ti * tk) / (tk - ti);
    std::cout << "part 2 = " << std::setprecision(20) << (a0 + b0 + c0) << '\n';
    // 600352360036779
}

int main() {
    const auto things = parse_file();
    do_part1(things);
    do_part2(things);
}
