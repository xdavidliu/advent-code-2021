#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <string>
#include <exception>
#include <fstream>
#include <sstream>
#include <string_view>
#include <tuple>
#include <deque>

struct Input {
    char type = 0;
    std::string lhs;
    std::vector<std::string> rhs;
};

void push_comma_separated(std::istringstream& iss,
                          std::vector<std::string>& out) {
    std::string word;
    while (iss >> word) {
        if (word.back() == ',') {
            out.push_back(word.substr(0, word.size() - 1));
        } else {
            out.push_back(word);
        }
    }
}

Input from_line(const std::string& line) {
    if (line.find("broadcaster") == 0) {
        std::cout << "from line called on broadcaster\n";
        throw std::exception();
    }
    std::istringstream iss(line);
    Input input;
    iss >> input.type;
    iss >> input.lhs;
    std::string ignore;
    iss >> ignore;  // ->
    push_comma_separated(iss, input.rhs);
    return input;
}

constexpr char flip_ch = '%';
constexpr char conv_ch = '&';
const char *broadcaster = "broadcaster";

auto read_file(const char* filepath) {
    if (auto fs = std::ifstream(filepath)) {
        std::map<std::string, char> type_of;
        std::map<std::string, std::vector<std::string>> neighbors;
        std::string line;
        while (std::getline(fs, line)) {
            if (0 == line.find(broadcaster)) {
                std::vector<std::string> br_ns;
                constexpr auto start = std::string_view("broadcaster -> ").size();
                std::istringstream iss(line.substr(start));
                push_comma_separated(iss, br_ns);
                neighbors[broadcaster] = std::move(br_ns);
            } else {
                auto [type, lhs, ns] = from_line(line);
                type_of[lhs] = type;
                neighbors[lhs] = std::move(ns);
            }
        }
        return std::make_tuple(type_of, neighbors);
    } else {
        std::cout << "invalid filepath\n";
        throw std::exception();
    }
}

struct Pulse {
    std::string src, dest;
    bool high;
};

// todo when summing, don't forget to count the initial pulse from button to
// broadcast

auto get_converge_neighbors(const std::map<std::string, char> &type_of, const std::map<std::string, std::vector<std::string>> &neighbors) {
    std::map<std::string, std::vector<std::string>> out;
    for (const auto &[key, val] : neighbors) {
        for (const auto &dest : val) {
            const auto found_type = type_of.find(dest);
            if (found_type == type_of.cend() || found_type->second != conv_ch) { continue; }
            auto [iter, worked] = out.insert({dest, std::vector<std::string>()});
            iter->second.push_back(key);
        }
    }
    return out;
}

auto get_flip_on(const std::map<std::string, char> &type_of) {
    std::map<std::string, bool> flip_on;
    for (const auto &[key, val] : type_of) {
        if (val == flip_ch) {
            flip_on[key] = false;
        }
    }
    return flip_on;
}

auto get_last_sent_to_from(const std::map<std::string, char> &type_of, const std::map<std::string, std::vector<std::string>> &neighbors) {
    // key is dest, only convergence, inner key is src
    std::map<std::string, std::map<std::string, bool>> out;
    for (const auto &[src, dests] : neighbors) {
        for (const auto &dest : dests) {
            const auto found = type_of.find(dest);
            if (found == type_of.cend() || found->second != '&') { continue; }
            auto [iter, worked] = out.insert({dest, std::map<std::string, bool>()});
            // Conjunction modules ... initially default to remembering a low pulse for each input.
            iter->second[src] = false;
        }
    }
    return out;
}

void foo2() {
    constexpr char filepath[] ="/home/employee/Documents/temp/data.txt";
    const auto [type_of, neighbors] = read_file(filepath);
    const std::string target = "bx";
    for (const auto &[key, val] : neighbors) {
        const auto found = std::find(val.cbegin(), val.cend(), target);
        if (found != val.cend()) {
            const auto type_found = type_of.find(key);
            if (type_found != type_of.cend()) {
                std::cout << type_found->second;
            }
            std::cout << key << ' ';
        }
    }
}

void foo1() {
    constexpr char filepath[] ="/home/employee/Documents/temp/data.txt";
    const auto [type_of, neighbors] = read_file(filepath);
    const auto converge_neighbors = get_converge_neighbors(type_of, neighbors);
    auto last_sent_to_from = get_last_sent_to_from(type_of, neighbors);
    // "Flip-flop modules ... are initially off."
    auto flip_on = get_flip_on(type_of);
    std::deque<Pulse> que;
    long low_count = 0, high_count = 0;
    for (int presses = 1; presses < 20000; ++presses) {
        // push button
        ++low_count;
        for (const auto& dest: neighbors.at(broadcaster)) {
            ++low_count;
            que.push_back({broadcaster, dest, false});
        }
        while (!que.empty()) {
            const auto [src, dest, high] = que.front();
            if (src == "zt" && !high) {
                std::cout << presses << " had low\n";
                // this being low means at this button_ind, all the % parents are 1
                // hence on NEXT button_ind, they are back to zero again.
                // for gt:
                //3797 had low
                //7594 had low
                //11391 had low
                std::cout << "part 2 = " << 211712400442661L << '\n';
                return;
                // gt, xd, ms, zt  <- grandparents of bb, which is parent of rx
                // LCM(3797, 3733, 3907, 3823)
            }
            que.pop_front();
            last_sent_to_from[dest][src] = high;
            const auto found = type_of.find(dest);
            if (found != type_of.cend()) {
                switch (found->second) {
                    case '%': {  // flip
                        // "If a flip-flop module receives a high pulse, it is ignored
                        // and nothing happens."
                        if (high) { continue; }
                        // "However, if a flip-flop module receives a low pulse, it flips between on and off."
                        auto &found_flip = flip_on[dest];
                        const auto old_val = found_flip;
                        found_flip = !found_flip;
                        for (const auto &neigh : neighbors.at(dest)) {
                            // "If it was off, it turns on and sends a high pulse. If it was on,
                            // it turns off and sends a low pulse."
                            que.push_back({dest, neigh, !old_val});
                            if (old_val) { ++low_count; } else { ++high_count; }
                        }
                        break;
                    }
                    case '&': {  // converge
                        bool all_high = true;
                        for (const auto &[ignore, last_high] : last_sent_to_from.at(dest)) {
                            if (!last_high) {
                                all_high = false;
                                break;
                            }
                        }
                        for (const auto &neigh : neighbors.at(dest)) {
                            // if it remembers high pulses for all inputs, it sends a low pulse;
                            // otherwise, it sends a high pulse.
                            que.push_back({dest, neigh, !all_high});
                            if (all_high) { ++low_count; } else { ++high_count; }
                        }
                        break;
                    }
                    default: {
                        std::cout << found->second << " invalid found->second\n";
                        throw std::exception();
                    }
                }
            } else {  // dest no type; maybe output or something
                const auto signal = high ? "high" : "low";
                // std::cout << dest << " received " << signal << '\n';
            }
        }
        if (presses == 1000) {
            std::cout << "part 1 = " << low_count * high_count << '\n';  // 883726240
        }
    }
}
