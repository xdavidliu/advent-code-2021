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
    bool operator<(const Row &other) const {
      return source < other.source;
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
        Row row;
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

void match_row(std::vector<std::pair<long, long>> &out, const long source, const long range, const Row &row) {
  const auto left = std::max(source, row.source);
  const auto right = std::min(source + range, row.source + row.range);
  if (left <= right) {
    const auto out_left = left + row.destination - row.source;
    const auto out_right = right + row.destination - row.source;
    out.emplace_back(out_left, out_right);
  }
}

void match_map(std::vector<std::pair<long, long>> &out, const long source, const long range, const std::vector<Row> &map) {
  for (const auto &row : map) {
    match_row(out, source, range, row);
  }
}

/*
 * wait this is wrong
 * must match all parts
 * */

void part2(const std::vector<long> &seeds, const std::vector<std::vector<Row>> &maps) {
  std::vector<std::pair<long, long>> pairs_from, pairs_to;
  for (std::size_t i = 0; i < seeds.size(); i += 2) {
    pairs_from.emplace_back(seeds[i], seeds[i+1]);
  }
  for (const auto &map : maps) {
    pairs_to.clear();
    for (const auto &[source, range] : pairs_from) {
      match_map(pairs_to, source, range, map);
    }
    pairs_to.swap(pairs_from);
  }
  const auto ans = std::min_element(pairs_from.cbegin(), pairs_from.cend())->first;
  std::cout << "part 2 = " << ans << '\n';  // too high
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
        part1(seeds, maps);
        part2(seeds, maps);
    }
}
