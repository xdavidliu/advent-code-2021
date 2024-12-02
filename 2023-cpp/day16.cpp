#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <deque>
#include <tuple>
#include <set>
#include <exception>

enum class Direction {
    Up, Down, Left, Right
};

std::pair<std::size_t, std::size_t> move_to(const Direction dir, const std::size_t row, const std::size_t col) {
    switch (dir) {
        case Direction::Up :
            return {row - 1, col};
        case Direction::Down :
            return {row + 1, col};
        case Direction::Left :
            return {row, col - 1};
        case Direction::Right :
            return {row, col + 1};
    }
}

using Position = std::tuple<std::size_t, std::size_t, Direction>;

std::size_t dedupe_dir(const std::set<Position> &seen) {
    std::set<std::pair<std::size_t, std::size_t>> without_dir;
    for (const auto &[row, col, dir]: seen) {
        without_dir.emplace(row, col);
    }
    return without_dir.size();
}

auto read_from(const char *filepath) {
    std::vector<std::string> grid;
    if (auto fs = std::ifstream(filepath)) {
        std::string line;
        while (std::getline(fs, line)) {
            grid.push_back(line);
        }
    }
    return grid;
}

class Solver {
    std::vector<std::string> grid;
    std::deque<Position> que;
    std::set<Position> seen;

    void add_to_both(const std::size_t row, const std::size_t col, const Direction dir) {
        if (row != -1 && col != -1 && row != grid.size() && col != grid.front().size() &&
            0 == seen.count({row, col, dir})) {
            que.emplace_back(row, col, dir);
            seen.emplace(row, col, dir);
        }
    }

    void
    split_left_right(const std::size_t row, const std::size_t col) {
        const auto [left_row, left_col] = move_to(Direction::Left, row, col);
        const auto [right_row, right_col] = move_to(Direction::Right, row, col);
        add_to_both(left_row, left_col, Direction::Left);
        add_to_both(right_row, right_col, Direction::Right);
    }

    void split_up_down(const std::size_t row, const std::size_t col) {
        const auto [up_row, up_col] = move_to(Direction::Up, row, col);
        const auto [down_row, down_col] = move_to(Direction::Down, row, col);
        add_to_both(up_row, up_col, Direction::Up);
        add_to_both(down_row, down_col, Direction::Down);
    }

    void reflect_slash(const std::size_t row, const std::size_t col, const Direction dir) {
        Direction new_dir;
        switch (dir) {
            case Direction::Up: {
                new_dir = Direction::Right;
                break;
            }
            case Direction::Down: {
                new_dir = Direction::Left;
                break;
            }
            case Direction::Left: {
                new_dir = Direction::Down;
                break;
            }
            case Direction::Right: {
                new_dir = Direction::Up;
                break;
            }
            default: throw std::exception();
        }
        const auto [new_row, new_col] = move_to(new_dir, row, col);
        add_to_both(new_row, new_col, new_dir);
    }

    void reflect_backslash(const std::size_t row, const std::size_t col, const Direction dir) {
        Direction new_dir;
        switch (dir) {
            case Direction::Up: {
                new_dir = Direction::Left;
                break;
            }
            case Direction::Down: {
                new_dir = Direction::Right;
                break;
            }
            case Direction::Left: {
                new_dir = Direction::Up;
                break;
            }
            case Direction::Right: {
                new_dir = Direction::Down;
                break;
            }
            default: throw std::exception();
        }
        const auto [new_row, new_col] = move_to(new_dir, row, col);
        add_to_both(new_row, new_col, new_dir);
    }

public:
    explicit Solver(const char *filepath) : grid(read_from(filepath)) {}
    std::size_t solve(const std::size_t init_row, const std::size_t init_col, const Direction init_dir) {
        que.clear();
        seen.clear();
        add_to_both(init_row, init_col, init_dir);
        while (!que.empty()) {
            const auto [row, col, dir] = que.front();
            que.pop_front();
            const auto [new_row, new_col] = move_to(dir, row, col);
            switch (grid[row][col]) {
                // https://stackoverflow.com/a/34830061/2990344
                case '.': {
                    add_to_both(new_row, new_col, dir);
                    break;
                }
                case '|': {
                    if (dir == Direction::Up || dir == Direction::Down) {
                        add_to_both(new_row, new_col, dir);
                    } else {
                        split_up_down(row, col);
                    }
                    break;
                }
                case '-': {
                    if (dir == Direction::Left || dir == Direction::Right) {
                        add_to_both(new_row, new_col, dir);
                    } else {
                        split_left_right(row, col);
                    }
                    break;
                }
                case '\\': {
                    reflect_backslash(row, col, dir);
                    break;
                }
                case '/': {
                    reflect_slash(row, col, dir);
                    break;
                }
                default:
                    throw std::exception();
            }
        }
        return dedupe_dir(seen);
    }
    auto solve_best() {
        std::size_t best = 0;
        for (std::size_t row = 0; row < grid.size(); ++row) {
            best = std::max(best, solve(row, 0, Direction::Right));
            best = std::max(best, solve(row, grid.front().size() - 1, Direction::Left));
        }
        for (std::size_t col = 0; col < grid.size(); ++col) {
            best = std::max(best, solve(0, col, Direction::Down));
            best = std::max(best, solve(grid.size() - 1, col, Direction::Up));
        }
        return best;
    }
};

int main() {
    Solver solver("/home/xdavidliu/Documents/temp/data.txt");
    const auto part1 = solver.solve(0, 0, Direction::Right);
    std::cout << "part 1 = " << part1 << '\n';  // 8389
    std::cout << "part 2 = " << solver.solve_best() << '\n';  // 8564
}
