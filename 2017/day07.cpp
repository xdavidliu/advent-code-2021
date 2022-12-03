#include <iostream>
#include <regex>
#include <string>
#include <fstream>
#include <set>
#include <map>
#include <vector>
#include <cstddef>
#include <algorithm>
#include <stdexcept>

std::vector<std::string> split(const std::string &text, std::string delim) {
    std::vector<std::string> out;
    std::size_t pos = 0;
    while (true) {
        auto next = text.find(delim, pos);
        if (next == std::string::npos) {
            out.push_back(text.substr(pos, text.size() - pos));
            break;
        } else {
            out.push_back(text.substr(pos, next - pos));
            pos = next + delim.size();
        }
    }
    return out;
}

struct Node {
    long tree_weight;
    long weight;
    std::vector<Node> children;
};

Node make_tree(std::string root,
               const std::map<std::string, long> &weights,
               const std::map<std::string, std::vector<std::string>> &adj)
{
    Node out;
    // note cannot say weights[root] because weights is const and map::operator[] has no
    // const qualifier! ugh!
    // https://stackoverflow.com/a/42095692/2990344
    out.weight = weights.at(root);
    out.tree_weight = out.weight;
    if (adj.count(root)) {  // leaves are not in adj
        for (const auto &c : adj.at(root)) {
            auto subtree = make_tree(c, weights, adj);
            out.tree_weight += subtree.tree_weight;
            out.children.push_back(std::move(subtree));
        }
    }
    return out;
}

long repeat(const std::vector<Node> &children) {
    long out;
    std::vector<long> seen;
    for (const auto &c : children) {
        if (seen.end() != std::find(seen.begin(), seen.end(), c.tree_weight)) {
            return c.tree_weight;
        }
        seen.push_back(c.tree_weight);
    }
    throw std::runtime_error("did not find any repeat");
}

long recurse(const Node &root, long delta) {
    long correct = repeat(root.children);
    for (const auto &c : root.children) {
        if (c.tree_weight != correct) {
            return recurse(c, delta);
        }
    }
    return root.weight + delta;
}

long part2(const Node &root) {
    if (root.children.size() == 2) {
        std::cout << "skipping part 2 because two-child case unimplemented\n";
        return -1;
        // would need to recurse into both because not sure which one is wrong.
    }
    long correct = repeat(root.children);
    for (const auto &c : root.children) {
        if (c.tree_weight != correct) {
            return recurse(c, correct - c.tree_weight);
        }
    }
    /*
     * if children were all equal, the imbalance cannot be in
     * any subtree, since by changing one node in the subtree, you would
     * destroy the balance. Hence the wrong node MUST be in subtree with root
     * that is mismatched.
     * Once you have goal, don't need to worry about two children case.
     */
}

int main() {
    if (auto fin = std::ifstream("/home/xdavidliu/Documents/data.txt")) {
        std::set<std::string> leaves, dests;
        std::map<std::string, long> weights;
        std::map<std::string, std::vector<std::string>> adj;
        // https://en.cppreference.com/w/cpp/regex/ecmascript
        // \\((\\d+)\\) means "literal left paren, then capture the digits, then
        // literal right paren
        const std::string pat_leaf("(\\w+) \\((\\d+)\\)");
        std::regex re_leaf(pat_leaf);
        std::regex re_edge(pat_leaf + " -> (.+)");
        std::string ln;
        while (std::getline(fin, ln)) {
            std::smatch sm;
            if (std::regex_match(ln, sm, re_edge)) {
                auto tokens = split(sm[3].str(), ", ");
                auto name = sm[1].str();
                leaves.insert(name);
                weights[name] = std::stol(sm[2].str().c_str());
                dests.insert(tokens.begin(), tokens.end());
                adj[name] = std::move(tokens);
            } else if (std::regex_match(ln, sm, re_leaf)) {
                auto name = sm[1].str();
                leaves.insert(name);
                weights[name] = std::stol(sm[2].str().c_str());
            }
        }
        std::vector<std::string> diff;
        std::set_difference(leaves.begin(), leaves.end(),
                            dests.begin(), dests.end(),
                            std::back_inserter(diff));
        auto rootname = diff[0];
        std::cout << "part 1 = " << rootname << '\n';
        auto root = make_tree(rootname, weights, adj);
        std::cout << "part 2 = " << part2(root) << '\n';
    }
}
