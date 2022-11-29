#include <iostream>
#include <string>
#include <set>
#include <sstream>
#include <fstream>
#include <algorithm>

bool correct(const std::string &str, bool sort) {
    std::istringstream strm(str);
    std::set<std::string> set;
    std::string word;
    while (strm >> word) {
        if (sort) {
            std::sort(word.begin(), word.end());
        }
        if (set.count(word)) {
            return false;
        }
        set.insert(word);
    }
    return true;
}

int main(void) {
    if (auto fin = std::ifstream("/home/xdavidliu/Documents/data.txt")) {
        std::string ln;
        int part1 = 0, part2 = 0;
        while (std::getline(fin, ln)) {
            part1 += correct(ln, false);
            part2 += correct(ln, true);
        }
        std::cout << "part 1 = " << part1 << '\n';
        std::cout << "part 2 = " << part2 << '\n';
    }
}
