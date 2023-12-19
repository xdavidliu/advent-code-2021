#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <algorithm>
#include <queue>
#include <set>

constexpr char filepath[] = "/home/employee/Documents/temp/example.txt";

struct Dig {
    char dir;
    long steps;
};

Dig dig_from(const std::string& rgb) {
    // 2 because of (#
    const auto steps = std::stoi(rgb.substr(2, 5), nullptr, 16);
    const auto second_to_last = *(rgb.crbegin() + 1);
    switch (second_to_last) {
        case '0':return {'R', steps};
        case '1':return {'D', steps};
        case '2':return {'L', steps};
        case '3':return {'U', steps};
        default: throw std::exception();
    }
}

std::pair<std::vector<Dig>, std::vector<Dig>> read_digs() {
    if (auto fs = std::fstream(filepath)) {
        std::vector<Dig> out1, out2;
        long steps = 0;
        char dir;
        std::string rgb;
        while (fs >> dir) {
            fs >> steps >> rgb;
            out1.push_back({dir, steps});
            out2.push_back(dig_from(rgb));
        }
        return {out1, out2};
    } else {
        throw std::exception();
    }
}

// x, top, bottom, is_up
// top means low number of row, bottom means high number
// i.e. in a grid
using Vertical = std::tuple<long, long, long, bool>;

auto get_x(const Vertical &vert) {
    return std::get<0>(vert);
}

auto get_top(const Vertical &vert) {
    return std::get<1>(vert);
}

auto get_bottom(const Vertical &vert) {
    return std::get<2>(vert);
}

auto is_up(const Vertical &vert) {
    return std::get<3>(vert);
}


bool less_top(const Vertical &a, const Vertical &b) {
    return get_top(a) < get_top(b);
}

class GreaterBottom {
public:
    bool operator() (const Vertical &a, const Vertical &b) {
        return get_bottom(a) > get_bottom(b);
    }
};

auto verticals_by_top(const std::vector<Dig> &digs) {
    std::vector<Vertical> out;
    long x = 0, row = 0;
    for (const auto &dig : digs) {
        switch (dig.dir) {
            case 'L': {
                x -= dig.steps;
                break;
            }
            case 'R': {
                x += dig.steps;
                break;
            }
            case 'U': {
                const auto old_row = row;
                row -= dig.steps;
                out.emplace_back(x, row, old_row, true);
                break;
            }
            case 'D': {
                const auto old_row = row;
                row += dig.steps;
                out.emplace_back(x, old_row, row, false);
            }
        }
    }
    std::sort(out.begin(), out.end(), less_top);
    return out;
}

#include <limits>

long solve(const std::vector<Vertical> &verts) {
    long area = 0;
    // verts sorted by x, so this def of clockwise is correct.
    const auto clockwise = is_up(verts.front());
    // if clockwise, all up verts have area to right, and all down have
    // area to left
    // min_heap, peek has smallest bottom
    std::priority_queue<Vertical, std::vector<Vertical>, GreaterBottom> heap;
    // keep matched so can quickly iterate through by x
    std::set<Vertical> heap_set;
    auto iter = verts.cbegin();
    constexpr auto inf = std::numeric_limits<long>::max();
    long cross_section = 0, last_row = std::numeric_limits<long>::min();
    while (!heap.empty() || iter != verts.cend()) {
        const auto heap_next = heap.empty() ? inf : get_bottom(heap.top());
        const auto iter_next = iter == verts.cend() ? inf : get_top(*iter);
        const auto next = std::min(heap_next, iter_next);
        area += cross_section * (next - last_row + 1);
        last_row = next;
        if (next == heap_next) {
            while (!heap.empty() && next == get_bottom(heap.top())) {
                heap_set.erase(heap.top());
                heap.pop();
            }
            // exiting verticals don't participate in next cross-section
        }
        // not else-if, since both can be true
        if (next == iter_next) {
            while (iter != verts.cend() && next == get_top(*iter)) {
                heap_set.insert(*iter);
                heap.push(*iter);
                ++iter;
            }
        }
        // expect heap size to be even now
        // expect from left to right all unique x
        // expect turn on off on off in that order, don't even care about clockwise
        bool entering = true;
        cross_section = 0;
        long last_x = 0; // unused on first iteration
        for (const auto &vert : heap_set) {
            const auto this_x = get_x(vert);
            if (!entering) {
                cross_section += this_x - last_x + 1;
            }
            entering = !entering;
            last_x = this_x;
        }
        // now use heap_set to re-calculate cross-section
        /* actually, verts about to be removed or about to be added
         * do NOT contribute to cross section!
         * multiply last cross-section by distance since last row
         * account for boundary effects w/ +1, -1, etc
         * very first iteration establishes cross-section; or just hack
         *   to have cross section 0 before first iter
         */
    }
    return area;
}

int main() {
    const auto [digs1, digs2] = read_digs();
    const auto verts1 = verticals_by_top(digs1);
    const auto verts2 = verticals_by_top(digs2);
    std::cout << "part 1 = " << solve(verts1) << '\n';  // 49897
    std::cout << "part 2 = " << solve(verts2) << '\n';
}
