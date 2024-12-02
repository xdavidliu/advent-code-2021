#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <algorithm>
#include <queue>
#include <utility>
#include <map>
#include <tuple>
#include <exception>
#include <limits>

enum class Direction {
    Up, Down, Left, Right
};

Direction rotate_left(const Direction dir) {
    switch(dir) {
        case Direction::Up : return Direction::Left;
        case Direction::Down : return Direction::Right;
        case Direction::Left : return Direction::Down;
        case Direction::Right : return Direction::Up;
    }
}

Direction rotate_right(const Direction dir) {
    switch(dir) {
        case Direction::Up : return Direction::Right;
        case Direction::Down : return Direction::Left;
        case Direction::Left : return Direction::Up;
        case Direction::Right : return Direction::Down;
    }
}

// row, col, dir
// need negative to handle off grid by more than -1
using Position = std::tuple<long, long, Direction>;

// interestingly, move and forward are both related to r-value refs
// in C++ terminology
Position move_forward(const Position pos) {
    auto [row, col, dir] = pos;
    switch (dir) {
        case Direction::Up : {
            --row; break;
        }
        case Direction::Down : {
            ++row; break;
        }
        case Direction::Left : {
            --col; break;
        }
        case Direction::Right : {
            ++col; break;
        }
    }
    return {row, col, dir};
}

Position left_and_forward(const Position pos) {
    auto [row, col, dir] = pos;
    return move_forward({row, col, rotate_left(dir)});
}

Position right_and_forward(const Position pos) {
    auto [row, col, dir] = pos;
    return move_forward({row, col, rotate_right(dir)});
}

using Heat = std::size_t;
constexpr auto inf_heat = std::numeric_limits<Heat>::max();

// heat, pos
using Item = std::pair<Heat, Position>;

auto read_grid(const char *filepath) {
    if (auto fs = std::ifstream(filepath)) {
        std::string line;
        std::vector<std::string> grid;
        while (std::getline(fs, line)) {
            grid.push_back(line);
        }
        return grid;
    } else {
        throw std::exception();
    }
}

class Solver {
    const std::vector<std::string> grid;
    // greater gives min heap
    // interesting you can say greater<>, known as "transparent functor"
    std::priority_queue<Item, std::vector<Item>, std::greater<>> heap;
    std::map<Position, Heat> best_heat;
    Heat best_non_heap_end;
    Heat grid_value(const std::size_t row, const std::size_t col) {
        return grid[row][col] - '0';
    }
    void add(const Item &item) {
        const auto [heat, pos] = item;
        const auto [row, col, dir] = pos;
        // assumes Position has signed indices
        if (row < 0 || col < 0 || row >= grid.size() || col >= grid.front().size()) { return; }
        auto found = best_heat.find(pos);
        const auto new_heat = heat + grid_value(row, col);
        auto [iter, worked] = best_heat.insert({pos, new_heat});
        // if !worked, iter already points to value
        // https://en.cppreference.com/w/cpp/container/map/insert
        if (worked || new_heat < iter->second) {
            iter->second = new_heat;  // does nothing if !worked
            heap.emplace(new_heat, pos);
        }
    }
    bool is_at_end(const std::size_t row, const std::size_t col) {
        return row + 1 == grid.size() && col + 1 == grid.front().size();
    }
public:
    explicit Solver(const char *filepath) : grid(read_grid(filepath)), best_non_heap_end(inf_heat) {}
    std::size_t solve(const int min_move, const int max_move) {
        best_heat.clear();
        // no clear for priority queue, so do it manually
        // https://stackoverflow.com/a/2852183/2990344
        heap = decltype(heap)();
        best_non_heap_end = inf_heat;
        // because heap pop loop below assumes one turn already made
        // 0 instead of grid_value(0, 0) because problems says first doesn't count
        add({0, {0, 1, Direction::Right}});
        add({0, {1, 0, Direction::Down}});
        while (!heap.empty()) {
            auto [heat, pos] = heap.top();
            heap.pop();
            // check best, if best is lower heat, skip. Hmm, may be optimizable here
            // since may be provable that's not possible. But this is suboptimality that I discussed
            // last year in central park.
            if (heat > best_heat.at(pos)) { continue; }
            const auto [row, col, dir] = pos;
            if (is_at_end(row, col)) {
                return std::min(heat, best_non_heap_end);
            }
            // after a turn, there's already one move
            for (int moves = 1; moves <= max_move; ++moves) {
                // heat is valid here because just turned, so pos is in heap
                if (min_move <= moves) {
                    add({heat, left_and_forward(pos)});
                    add({heat, right_and_forward(pos)});
                }
                // because don't want to check best_non_heap_end after 1 + max_move
                if (moves == max_move) { break; }
                pos = move_forward(pos);
                const auto [row, col, dir] = pos;
                if (row < 0 || col < 0 || row >= grid.size() || col >= grid.front().size()) { break; }
                // need to manually add heat because forward-moves never added to heap
                heat += grid_value(row, col);
                // idea: best_heat djikstra map missing a lot of these intermediate
                // ones. But that's okay because max of 3 means there's lots of turns,
                // so okay to keep track of just the "just turned" ones
                if (is_at_end(row, col)) {
                    best_non_heap_end = std::min(best_non_heap_end, heat);
                    break;
                    // can't turn or move forward from end
                }
            }
        }
        std::cout << "failed to reach end\n";
        throw std::exception();
    }
};

int main() {
    constexpr char filepath[] = "/home/xdavidliu/Documents/temp/data.txt";
    Solver solver(filepath);
    const auto part1 = solver.solve(0, 3);
    std::cout << "part 1 = " << part1 << '\n';  // 785
    const auto part2 = solver.solve(4, 10);
    std::cout << "part 2 = " << part2 << '\n';  // 922
}

/*
 * djikstra, except for neighbors, do left and right, then forward (1, 2, 3) LR
 * state needs direction so that you know not to go backwards
 * for initial state, have 0,0 right and also 0 0 down.
 *
 */
