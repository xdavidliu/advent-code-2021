#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <set>
#include <string_view>
#include <deque>

constexpr char filepath[] = "/home/employee/Documents/temp/data.txt";
bool left_handed = false;
std::size_t queue_limit = 123456;

struct Dig {
    char dir;
    int steps;
};

using Position = std::pair<long, long>;

Dig dig_from(const std::string &rgb) {
    // 2 because of (#
    const auto steps = std::stoi(rgb.substr(2, 5), nullptr, 16);
    const auto second_to_last = *(rgb.crbegin()+1);
    switch (second_to_last) {
        case '0':
            return {'R', steps};
        case '1':
            return {'D', steps};
        case '2':
            return {'L', steps};
        case '3':
            return {'U', steps};
    }
    throw std::exception();
}

std::pair<std::vector<Dig>, std::vector<Dig>> read_digs() {
     if (auto fs = std::fstream(filepath)) {
         std::vector<Dig> out1, out2;
         int steps = 0;
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

Position move_increment(const Position &pos, const char dir) {
    const auto [row, col] = pos;
    switch (dir) {
        case 'U': return {row-1, col};
        case 'D': return {row+1, col};
        case 'L': return {row, col-1};
        case 'R': return {row, col+1};
        default: throw std::exception();
    }
}

// to minus from
char diff_dir(const Position &to, const Position &from) {
    const auto [to_row, to_col] = to;
    const auto [from_row, from_col] = from;
    switch (to_row - from_row) {
        case -1: return 'U';
        case 1: return 'D';
    }
    switch (to_col - from_col) {
        case -1: return 'L';
        case 1: return 'R';
    }
    throw std::exception();
}

char dir_left(const char dir) {
    switch (dir) {
        case 'U': return 'L';
        case 'D': return 'R';
        case 'L': return 'D';
        case 'R': return 'U';
        default: throw std::exception();
    }
}

char dir_right(const char dir) {
    switch (dir) {
        case 'U': return 'R';
        case 'D': return 'L';
        case 'L': return 'U';
        case 'R': return 'D';
        default: throw std::exception();
    }
}

auto visit_loop(const std::vector<Dig> &digs) {
    std::vector<Position> out;
    Position pos = {0, 0};
    out.push_back(pos);
    for (const auto &[dir, steps] : digs) {
        for (int i = 0; i < steps; ++i) {
            pos = move_increment(pos, dir);
            out.push_back(pos);
        }
    }
    return out;
}

void add_to_bfs(std::set<Position> &seen, std::deque<Position> &que, const Position &pos) {
    if (seen.count(pos)) { return; }
    seen.insert(pos);
    que.push_back(pos);
    if (seen.size() > queue_limit) {
        // endl to guarantee flush cout
        std::cout << "bfs queue too big; invert left_handed" << std::endl;
        throw std::exception();
    }
}

void bfs(std::set<Position> &seen, const Position &start) {
    if (seen.count(start)) { return; }
    seen.insert(start);
    std::deque<Position> que;
    que.push_back(start);
    while (!que.empty()) {
        const auto pos = que.front();
        que.pop_front();
        for (const auto dir : std::string_view("UDLR")) {
            add_to_bfs(seen, que, move_increment(pos, dir));
        }
    }
}

// note BFS with handed-ness similar to earlier problem this year
auto fill_interior(const std::vector<Position> &loop) {
    std::set<Position> seen(loop.cbegin(), loop.cend());
    for (auto iter = loop.cbegin(); iter+1 != loop.cend(); ++iter) {
        const auto dir = diff_dir(*(iter+1), *iter);
        const auto in_dir = left_handed ? dir_left(dir) : dir_right(dir);
        // similar to other problem this year, need two bfs not one here
        bfs(seen, move_increment(*iter, in_dir));
        bfs(seen, move_increment(*(iter+1), in_dir));
    }
    return seen.size();
}

int main() {
    left_handed = left_handed;  // hack: hide compiler warnings about "always true"
    const auto [digs1, digs2] = read_digs();
    const auto loop1 = visit_loop(digs1);
    const auto part1 = fill_interior(loop1);
    std::cout << "part 1 = " << part1 << '\n';  // 49897
    for (const auto &[dir, steps] : digs2) {
        std::cout << dir << ' ' << steps << '\n';
    }
}
