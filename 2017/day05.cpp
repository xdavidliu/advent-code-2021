#include <iostream>
#include <fstream>
#include <vector>

// deliberately pass by copy
int part1(std::vector<int> vec) {
    int i = 0, steps = 0;
    while (0 <= i && i < vec.size()) {
        i += vec[i]++;
        ++steps;
    }
    return steps;
}

// by mutable reference to safe a copy
// assumes not needed afterwards
int part2(std::vector<int> &vec) {
    int i = 0, steps = 0;
    while (0 <= i && i < vec.size()) {
        int old = vec[i];
        if (old >= 3) {
            --vec[i];
        } else {
            ++vec[i];
        }
        i += old;
        ++steps;
    }
    return steps;
}

int main(void) {
    if (auto fin = std::ifstream("/home/xdavidliu/Documents/data.txt")) {
        int x;
        std::vector<int> vec;
        while (fin >> x) {
            vec.push_back(x);
        }
        std::cout << "part1 = " << part1(vec) << '\n';
        std::cout << "part2 = " << part2(vec) << '\n';
    }
}
