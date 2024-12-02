#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <limits>
#include <vector>
#include <algorithm>

struct Row {
    long destination, source, range;
    bool operator<(const Row &other) const {
        return source < other.source;
    }
    [[nodiscard]] long right() const {
        return source + range - 1;
    }
};

void my_assert(const bool cond) {
    if (!cond) { throw std::exception(); }
}

std::vector<Row> read_rows(std::ifstream &fs) {
    std::string line;
    std::getline(fs, line);  // skip first "seed-to-soil" line
    std::vector<Row> rows;
    while (std::getline(fs, line) && !line.empty()) {
        std::istringstream iss(line);
        Row row = {0, 0, 0};
        iss >> row.destination;
        iss >> row.source;
        iss >> row.range;
        rows.push_back(row);
    }
    std::sort(rows.begin(), rows.end());
    return rows;
}

long lookup(const long source_value, const std::vector<Row> &map) {
    for (const auto &row : map) {
        if (row.source <= source_value && source_value <= row.source + row.range) {
            return row.destination + source_value - row.source;
        }
    }
    return source_value;
}

long location(const long seed, const std::vector<std::vector<Row>> &maps) {
    long value = seed;
    for (const auto &map : maps) {
        value = lookup(value, map);
    }
    return value;
}

void part1(const std::vector<long> &seeds, const std::vector<std::vector<Row>> &maps) {
    auto small = std::numeric_limits<long>::max();
    for (const auto &seed : seeds) {
        small = std::min(small, location(seed, maps));
    }
    std::cout << "part 1 = " << small << '\n';  // 84470622
}

long right_side(const long source, const long range) {
    return source + range - 1;
}

// seeds mutable for convenience in chopping off right part of seed
void match_map(std::vector<std::pair<long, long>> &out, std::vector<std::pair<long, long>> &seeds, const std::vector<Row> &map) {
    auto row = map.cbegin();
    auto seed = seeds.begin();
    while (row != map.cend() && seed != seeds.end()) {
        if (row->right() < seed->first) { ++row; continue; }
        if (seed->second < row->source) {
            out.emplace_back(seed->first, seed->second);
            ++seed;
            continue;
        }
        if (seed->first < row->source) {
            out.emplace_back(seed->first, row->source - 1);
        }
        my_assert(seed->second >= row->source);
        const auto left = std::max(seed->first, row->source);
        const auto right = std::min(seed->second, row->right());
        if (left <= right) {
            const auto out_left = left + row->destination - row->source;
            const auto out_right = right + row->destination - row->source;
            out.emplace_back(out_left, out_right);
        }
        if (right == seed->second) {
            ++seed;
        } else {
            seed->first = right + 1;  // hack
            ++row;
        }
    }
    while (seed != seeds.end()) {
        out.emplace_back(seed->first, seed->second);
        ++seed;
    }
}

void part2(const std::vector<long> &seeds, const std::vector<std::vector<Row>> &maps) {
    std::vector<std::pair<long, long>> pairs_from, pairs_to;
    for (std::size_t i = 0; i < seeds.size(); i += 2) {
        pairs_from.emplace_back(seeds[i], seeds[i] + seeds[i+1] - 1);
    }
    for (const auto &map : maps) {
        if (pairs_from.empty()) { std::cout << "empty\n"; break; }
        pairs_to.clear();
        std::sort(pairs_from.begin(), pairs_from.end());
        match_map(pairs_to, pairs_from, map);
        pairs_to.swap(pairs_from);
        // todo for performance: merge pairs_from
    }
    const auto ans = std::min_element(pairs_from.cbegin(), pairs_from.cend());
    std::cout << "part 2 = " << ans->first << '\n';  // 26714516
}

int main() {
    const char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::string line;
        std::getline(fs, line);  // line.substr(2 + line.find(": "))
        std::istringstream iss(line.substr(2 + line.find(": ")));
        std::vector<long> seeds;
        long num;
        while (iss >> num) { seeds.push_back(num); }
        std::getline(fs, line);  // empty
        std::vector<std::vector<Row>> maps;
        while (fs) { maps.push_back(read_rows(fs)); }
        part1(seeds, maps);
        part2(seeds, maps);
    }
}
