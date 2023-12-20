#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <exception>
#include <numeric>

void my_assert(const bool cond, const std::string_view msg) {
    if (!cond) {
        std::cout << msg << '\n';
        throw std::exception();
    }
}

auto split_comma(const std::string &word) {
    std::vector<std::string> out;
    std::size_t left = 0, right = word.find(',');
    while (right != std::string::npos) {
        out.push_back(word.substr(left, right - left));
        left = right + 1;
        right = word.find(',', left);
    }
    // last one has npos, so
    out.push_back(word.substr(left));
    return out;
}

void add_to_map(const std::string &line, std::map<std::string, std::vector<std::string>> &ins_map) {
    const auto left_brace = line.find('{');
    const auto ins_str = line.substr(left_brace + 1, line.size() - left_brace - 2);
    ins_map.insert({line.substr(0, left_brace), split_comma(ins_str)});
}

// {x=787,m=2655,a=1222,s=2876}
auto get_values(const std::string &word) {
    std::vector<long> out;
    const auto items = split_comma(word.substr(1, word.size() - 2));
    for (const auto &item : items) {
        out.push_back(std::stol(item.substr(2)));
    }
    return out;
}

std::size_t index(const char ch) {
    switch (ch) {
        case 'x': return 0;
        case 'm': return 1;
        case 'a': return 2;
        case 's': return 3;
        default: {
            std::cout << "index: unexpected char\n";
            throw std::exception();
        }
    }
}

std::string perform_one(const std::vector<long> &values, const std::vector<std::string> &instructs) {
    for (const auto &ins : instructs) {
        const auto colon = ins.find(':');
        if (colon != std::string::npos) {
            const auto lhs = values[index(ins[0])];
            const auto rhs = std::stol(ins.substr(2, colon - 2));
            if (ins[1] == '<' && lhs < rhs || ins[1] == '>' && lhs > rhs) {
                return ins.substr(colon + 1);  // action
            }
            // otherwise, continue to next ins
        } else {
            return ins;  // action is entire instruction
        }
    }
    std::cout << "instructions exhausted\n";
    throw std::exception();
}

std::string perform_all(const std::vector<long> &values, const std::map<std::string, std::vector<std::string>> &ins_map) {
    std::string cur = "in";
    while (cur != "A" && cur != "R") {
        cur = perform_one(values, ins_map.at(cur));
    }
    return cur;
}

using Range = std::pair<int, int>;
using FourRange = std::tuple<Range, Range, Range, Range>;

// update single range should take MUTABLE reference
void update_single_range(const std::string &comparison, Range &range) {
    const auto rhs = std::stoi(comparison.substr(2));
    if (comparison[1] == '<') {
        range.second = std::min(range.second, rhs - 1);
    } else {  // >
        range.first = std::max(range.first, rhs + 1);
    }
}

void update_single_range_complement(const std::string &comparison, Range &range) {
    const auto rhs = std::stoi(comparison.substr(2));
    if (comparison[1] == '<') {  // becomes >=
        range.first = std::max(range.first, rhs);
    } else {  // > becomes <=
        range.second = std::min(range.second, rhs);
    }
}

// pass by mutable copy so can return
FourRange update_ranges(const std::string &comparison, FourRange ranges) {
    switch (comparison[0]) {
        case 'x': {
            update_single_range(comparison, std::get<0>(ranges));
            break;
        }
        case 'm': {
            update_single_range(comparison, std::get<1>(ranges));
            break;
        }
        case 'a': {
            update_single_range(comparison, std::get<2>(ranges));
            break;
        }
        case 's': {
            update_single_range(comparison, std::get<3>(ranges));
            break;
        }
        default: {
            std::cout << comparison << " update range\n";
            throw std::exception();
        }
    }
    return ranges;
}

FourRange update_ranges_complement(const std::string &comparison, FourRange ranges) {
    switch (comparison[0]) {
        case 'x': {
            update_single_range_complement(comparison, std::get<0>(ranges));
            break;
        }
        case 'm': {
            update_single_range_complement(comparison, std::get<1>(ranges));
            break;
        }
        case 'a': {
            update_single_range_complement(comparison, std::get<2>(ranges));
            break;
        }
        case 's': {
            update_single_range_complement(comparison, std::get<3>(ranges));
            break;
        }
        default: {
            std::cout << comparison << " update range complement\n";
            throw std::exception();
        }
    }
    return ranges;
}

void recurse(
        const std::map<std::string, std::vector<std::string>> &ins_map,
        const std::string &cur,
        const std::size_t ind,
        const FourRange ranges,
        std::vector<FourRange> &accepted)
{
    const auto &ins_list = ins_map.at(cur);
    const auto &ins = ins_list.at(ind);
    if (ins == "R") {
        return;
    } else if (ins == "A") {
        accepted.push_back(ranges);
        // todo: maybe do some merging to dedupe and avoid blowup
    } else {
        const auto colon = ins.find(':');
        if (colon == std::string::npos) {
            // neither R, A, nor comparison, hence is a register name
            my_assert(ind + 1 == ins_list.size(), "expected register to be last");
            recurse(ins_map, ins, 0, ranges, accepted);
            return;
        }
        // a<2006:qkq
        const auto new_ranges = update_ranges(ins.substr(0, colon), ranges);
        const auto next = ins.substr(colon + 1);
        if (next == "A") {
            accepted.push_back(new_ranges);
            // don't return, because need to recurse ind+1
        } else if (next != "R") {  // register
            recurse(ins_map, next, 0, new_ranges, accepted);
        }
        const auto complement_ranges = update_ranges_complement(ins.substr(0, colon), ranges);
        if (ind+1 < ins_list.size()) {
            recurse(ins_map, cur, ind+1, complement_ranges, accepted);
        }
    }
}

int main() {
    constexpr char filepath[] = "/tmp/data.txt";
    if (auto fs = std::ifstream(filepath)) {
        std::map<std::string, std::vector<std::string>> ins_map;
        std::string word;
        while ((fs >> word) && word.front() != '{') {
            add_to_map(word, ins_map);
        }
        decltype(get_values(word)) values;
        long part1 = 0;
        do {
            values = get_values(word);
            if ("A" == perform_all(values, ins_map)) {
                part1 += std::accumulate(values.cbegin(), values.cend(), 0L);
            }
        } while (fs >> word);
        std::cout << "part 1 = " << part1 << '\n';  // 402185
        //
        FourRange ranges{{1, 4000}, {1, 4000}, {1, 4000}, {1, 4000}};
        std::vector<FourRange> accepted;
        recurse(ins_map, "in", 0, ranges, accepted);
        long part2 = 0;
        // data just happens to cause these sets to be disjoint, so no inclusion-exclusion
        // shenanigans needed
        for (const auto &[x, m, a, s] : accepted) {
            part2 += 1L * (x.second - x.first + 1) * (m.second - m.first + 1) * (a.second - a.first + 1) * (s.second - s.first + 1);
        }
        std::cout << "part 2 = " << part2 << '\n';  // 130291480568730
    } else {
        my_assert(false, "wrong filepath");
    }
}
