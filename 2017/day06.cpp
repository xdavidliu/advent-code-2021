#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <iterator>
#include <algorithm>
#include <map>
#include <utility>

std::string vec2str(const std::vector<int> &vec) {
    std::ostringstream oss;
    std::ostream_iterator<int> oit(oss, " ");
    std::copy(vec.begin(), vec.end(), oit);
    return oss.str();
}

int ind_max(const std::vector<int> &vec) {
    int i = 0;
    for (int k = 1; k < vec.size(); ++k) {
        if (vec[k] > vec[i]) {
            i = k;
        }
    }
    return i;
}

void run_step(std::vector<int> &vec) {
    int i = ind_max(vec);
    int val = vec[i];
    vec[i] = 0;
    while (val) {
        i = (i + 1) % vec.size();
        --val;
        ++vec[i];
    }
}

std::pair<int, int> solve(std::vector<int> &vec) {
    std::map<std::string, int> seen;
    seen[vec2str(vec)] = 0;
    int steps = 0;
    while (true) {
        run_step(vec);
        ++steps;
        auto s = vec2str(vec);
        auto f = seen.find(s);
        if (f != seen.end()) {
            return std::make_pair(steps, f->second);
        }
        seen[s] = steps;
    }
}

int main(void) {
    std::vector<int> vec{14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4};
    auto sol = solve(vec);
    std::cout << "part 1 = " << sol.first << "\npart 2 = "
    << (sol.first - sol.second) << '\n';
}
