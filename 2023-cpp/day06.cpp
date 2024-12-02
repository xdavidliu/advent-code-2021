#include <iostream>

long distance(const long time, const long hold) {
    return hold * (time - hold);
}

long ways(const long time, const long record) {
    long count = 0;
    for (long hold = 0; hold <= time; ++hold) {
        if (record < distance(time, hold)) { ++count; }
    }
    return count;
}

// d == h t - h^2
// 0 == t - 2 h   (derivative)
// from h = 0 to h = t / 2 it's increasing
// from h = t / 2 to t = T it's decreasing
long first_positive_in_increasing(const long time, const long record) {
    long left = 0, right = time / 2;
    while (right - left > 1000) {
        long mid = (left + right) / 2;
        if (record >= distance(time, mid)) { left = mid; }
        else { right = mid; }
    }
    for (auto t = left; t <= right; ++t) {
        if (record < distance(time, t)) { return t; }
    }
    std::cout << "failed\n";
    return 0;
}

long last_positive_in_decreasing(const long time, const long record) {
    long left = time / 2, right = time;
    while (right - left > 1000) {
        long mid = (left + right) / 2;
        if (record < distance(time, mid)) { left = mid; }
        else { right = mid; }
    }
    for (auto t = right; t >= left; --t) {
        if (record < distance(time, t)) { return t; }
    }
    std::cout << "failed\n";
    return 0;
}

struct Race {
    long time, record;
};

int main() {
//    constexpr Race races[] = {{7, 9}, {15, 40}, {30, 200}};  // example
    constexpr Race races[] = {{47, 282}, {70, 1079}, {75, 1147}, {66, 1062}};
    long part1 = 1;
    for (const auto &race : races) {
        part1 *= ways(race.time, race.record);
    }
    std::cout << "part 1 = " << part1 << '\n';  // 281600
//    constexpr long time = 71530;
//    constexpr long record = 940200;
    constexpr long time = 47707566;
    constexpr long record = 282107911471062;
    const auto part2 = 1 + last_positive_in_decreasing(time, record) - first_positive_in_increasing(time, record);
    std::cout << "part 2 = " << part2 << '\n';  // 33875953
}
