#include <iostream>
#include <fstream>
#include <exception>
#include <string>
#include <sstream>
#include <limits>
#include <vector>
#include <algorithm>

struct Row {
    long destination, source, range;
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
        Row row;
        iss >> row.destination;
        iss >> row.source;
        iss >> row.range;
        rows.push_back(row);
    }
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

void solve(const std::vector<long> &seeds, const std::vector<std::vector<Row>> &maps) {
    auto small = std::numeric_limits<long>::max();
    for (const auto &seed : seeds) {
        small = std::min(small, location(seed, maps));
    }
    std::cout << "part 1 = " << small << '\n';  // 84470622
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
        std::cout << '\n';
        std::getline(fs, line);  // empty
        std::vector<std::vector<Row>> maps;
        while (fs) { maps.push_back(read_rows(fs)); }
        solve(seeds, maps);
    }
}
