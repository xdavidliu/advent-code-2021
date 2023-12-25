#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <set>
#include <deque>
#include <map>

// alternate proper way to do this:
// https://stackoverflow.com/questions/4482986/how-can-i-find-the-minimum-cut-on-a-graph-using-a-maximum-flow-algorithm
// also, see ch 26 min cut max flow section in CLRS

void adj_insert(std::map<std::string, std::vector<std::string>> &adj,
                const std::string &key, const std::string &val) {
    auto [iter, ignore] = adj.insert({key, std::vector<std::string>()});
    // "Each connection between two components is represented only once"
    iter->second.push_back(val);
}

auto read_adj() {
    const char *filepath = "/home/employee/Documents/temp/data.txt";
    auto fs = std::ifstream(filepath);
    std::map<std::string, std::vector<std::string>> adj;
    std::string line, key, val;
    while (std::getline(fs, line)) {
        std::istringstream iss(line);
        iss >> key;
        key = key.substr(0, key.size() - 1);  // omit :
        while (iss >> val) {
            adj_insert(adj, key, val);
            adj_insert(adj, val, key);
        }
    }
    return adj;
}

std::pair<std::string, std::string> ordered(const std::string &one, const std::string &two) {
    if (one < two) { return {one, two}; }
    else { return {two, one}; }
}

void increment(std::map<std::pair<std::string, std::string>, int> &count,
               const std::string &one,
               const std::string &two)
{
    const auto key = ordered(one, two);
    auto [iter, ignore] = count.insert({key, 0});
    ++iter->second;
}

void bfs(const std::map<std::string, std::vector<std::string>> &adj,
         const std::string &start,
         std::set<std::string> &seen,
         std::map<std::pair<std::string, std::string>, int> &count)
{
    std::deque<std::string> que;
    que.push_back(start);
    seen.insert(start);
    while (!que.empty()) {
        const auto front = que.front();
        que.pop_front();
        for (const auto &other : adj.at(front)) {
            if (!seen.count(other)) {
                seen.insert(other);
                increment(count, front, other);
                que.push_back(other);
            }
        }
    }
}

void remove_one_edge(std::map<std::string, std::vector<std::string>> &adj,
                 const std::string &key,
                 const std::string &elem)
{
    auto &v = adj.at(key);
    v.erase(std::find(v.begin(), v.end(), elem));
}

void remove_both_edge(std::map<std::string, std::vector<std::string>> &adj,
                      const std::string &one,
                      const std::string &two)
{
    remove_one_edge(adj, one, two);
    remove_one_edge(adj, two, one);
}

auto top_key(const std::map<std::pair<std::string, std::string>, int> &count) {
    int best = 0;
    std::string one, two;
    for (const auto &[key, c] : count) {
        if (c > best) {
            best = c;
            one = key.first;
            two = key.second;
        }
    }
    return std::make_pair(one, two);
}

void foo1() {
    auto adj = read_adj();
    std::map<std::pair<std::string, std::string>, int> count;
    std::set<std::string> seen;
    for (int i = 0; i < 3; ++i) {
        for (const auto &[key, ignore] : adj) {
            bfs(adj, key, seen, count);
            seen.clear();
        }
        const auto [one, two] = top_key(count);
        remove_both_edge(adj, one, two);
        count.clear();
    }
    int seen_size = 0;
    std::vector<int> diffs;
    for (const auto &[key, ignore] : adj) {
        bfs(adj, key, seen, count);
        if (seen.size() > seen_size) {
            diffs.push_back(seen.size() - seen_size);
            seen_size = seen.size();
        }
    }
    std::cout << "part 1 = " << (diffs[0] * diffs[1]) << '\n';
    // 547410
    /*
     * start BFS from each visited edge
     * update counter, after done, sort by count, get top 5
     * if yes, then cut those and do bfs from each component and multiply
     *
     * update: do it three times, taking top each time
     *
     */
}

int main() {
    foo1();
}
