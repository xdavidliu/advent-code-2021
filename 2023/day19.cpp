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

void foo4() {
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
    } else {
        my_assert(false, "wrong filepath");
    }
}

int main() {
    foo4();
}
