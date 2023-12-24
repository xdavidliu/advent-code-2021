#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

constexpr char filepath[] ="/home/employee/Documents/temp/data.txt";
constexpr long low_bound = 200000000000000L;
constexpr long high_bound = 400000000000000L;

//constexpr char filepath[] ="/home/employee/Documents/temp/example.txt";
//constexpr long low_bound = 7;
//constexpr long high_bound = 27;


long gcd(long a, long b) {
    if (b > a) { return gcd(b, a); }
    if (a < 0 || b < 0) {
        std::cout << "gcd negative\n";
        throw std::exception();
    }
    while (b) {
        const auto c = a % b;
        a = b;
        b = c;
    }
    return a;
}


struct Ratio {
    const long num = 1, den = 1;
    // effective java: prefer static factories
    static Ratio of(const long n, const long d) {
        if (d == 0) {
            std::cout << n << " div by zero\n";
            throw std::exception();
        }
        const auto g = gcd(std::abs(n), std::abs(d));
        if (d < 0) {
            return {-n / g, -d / g};  // num stores sign info
        } else {
            return {n / g, d / g};
        }
    }
    // not explicit
    Ratio(const long x) : num(x), den(1) {}
    bool operator==(const Ratio &other) const {
        return num == other.num && den == other.den;
    }
    bool operator!=(const Ratio &other) const {
        return !operator==(other);
    }
    bool operator<(const Ratio &other) const {
        return num * other.den < other.num * den;
    }
    bool operator<=(const Ratio &other) const {
        return num * other.den <= other.num * den;
    }
    Ratio inverse() const {
        return of(den, num);
    }
    Ratio abs() const {
        return of(std::abs(num), den);
    }
    Ratio operator-() const {
        return of(-num, den);
    }
    Ratio operator+(const Ratio &other) const {
        const long new_num = num * other.den + other.num * den;
        return of(new_num, den * other.den);
    }
    Ratio operator-(const Ratio &other) const {
        return operator+(-other);
    }
    Ratio operator*(const Ratio &other) const {
        return of(num * other.num, den * other.den);
    }
    Ratio operator/(const Ratio &other) const {
        return operator*(other.inverse());
    }
private:
    Ratio(const long n, const long d): num(n), den(d) {}
};

Ratio operator-(const long x, const Ratio &r) {
    return Ratio::of(x, 1) - r;
}


struct Stone {
    long x, y, z, vx, vy, vz;
    [[nodiscard]] Ratio slope() const {
        return Ratio::of(vy, vx);
    }
};


Stone from_line(const std::string &line) {
    std::istringstream iss(line);
    long v[6];
    char ch;
    for (int i = 0; i < 6; ++i) {
        iss >> v[i];
        iss >> ch;
    }
    return {v[0], v[1], v[2], v[3], v[4], v[5]};
}

bool goes_closer(const long x0, const long v, const Ratio &xf) {
    return (xf - x0 - v).abs() < (xf - x0).abs();
}

bool intersect_inside_bound(const Stone &s1, const Stone &s2) {
    if (s1.vx == 0 || s2.vx == 0) {
        std::cout << "vertical detected\n";
        return false;
    }
    // todo: if either one is vertical, do check
    const auto m1 = s1.slope(), m2 = s2.slope();

    if (m1 == m2) { return false; }
    const auto b1 = s1.y - m1 * s1.x, b2 = s2.y - m2 * s2.x;
    const auto x_inter = (b2 - b1) / (m1 - m2);
    if (!(Ratio::of(low_bound, 1) <= x_inter && x_inter <= high_bound) ||
            !goes_closer(s1.x, s1.vx, x_inter) ||
            !goes_closer(s2.x, s2.vx, x_inter)) {
        return false;
    }
    const auto y_inter = m1 * x_inter + b1;
    if (!(Ratio::of(low_bound, 1) <= y_inter && y_inter <= high_bound) ||
        !goes_closer(s1.y, s1.vy, y_inter) ||
        !goes_closer(s2.y, s2.vy, y_inter)) {
        return false;
    }
    return true;
}


void foo1() {
    auto fs = std::ifstream(filepath);
    std::string line;
    std::vector<Stone> stones;
    while (std::getline(fs, line)) {
        stones.push_back(from_line(line));
    }
    int part1 = 0;
    for (int i = 0; i + 1 < stones.size(); ++i) {
        for (int k = i + 1; k < stones.size(); ++k) {
            part1 += intersect_inside_bound(stones[i], stones[k]);
        }
    }
    std::cout << "part 1 = " << part1 << '\n';  // 6081 too low
}

void foo2() {
    const auto a = Ratio::of(2, 3), b = Ratio::of(3, 4);
    const auto c = a + b;
    std::cout << c.num << ' ' << c.den << '\n';
}

int main() {
    foo1();
}
