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
    std::string rgb;
};

using Position = std::pair<long, long>;

std::vector<Dig> read_digs() {
     if (auto fs = std::fstream(filepath)) {
         std::vector<Dig> out;
         Dig dig;
         while (fs >> dig.dir) {
             fs >> dig.steps >> dig.rgb;
             out.push_back(dig);
         }
         return out;
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
    }
    throw std::exception();
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
    for (const auto &[dir, steps, rgb] : digs) {
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
    const auto digs = read_digs();
    const auto loop = visit_loop(digs);
    const auto part1 = fill_interior(loop);
    std::cout << "part 1 = " << part1 << '\n';
}
