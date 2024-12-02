#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cctype>  // isdigit
#include <map>
#include <set>
#include <numeric>  // accumulate

bool is_symbol(const char ch) {
    return ch != '.' && !std::isdigit(ch);
}

void my_assert(const bool cond) {
    if (!cond) { throw std::exception(); }
}

// oh, part number, so I should actually put part numbers in a map
// actually, have map where key is every grid point that is part
// of a part number, and value is the leftmost grid point of that
// part number (not the number itself). This way you don't double
// count part numbers. Finally, have second map that maps leftmost
// grid point to acc value.

// but how to maintain first map if I don't know if I have part
// number until end? Ah just go back through and add-em.

// alternate approach: while checking symbol, if symbol is *
// then increment counter. Ah wait, doesn't work for my approach
// below because I short-circuit: If I find non-* symbol, I ignore
// future symbols.

void solve(const std::vector<std::string> &grid) {
    using Point = std::pair<std::size_t, std::size_t>;
    std::map<Point, Point> point_to_left;
    std::map<Point, int> left_to_acc;
    for (std::size_t row = 0; row < grid.size(); ++row) {
        bool symbol_found = false;
        bool in_middle_number = false;
        std::size_t left_col = 0;
        const bool at_bottom = row + 1 == grid.size();
        // https://stackoverflow.com/a/12391887/2990344
        decltype(left_to_acc)::mapped_type acc = 0;
        for (std::size_t col = 0; col < grid.front().size(); ++col) {
            const auto ch = grid[row][col];
            const auto is_digit = std::isdigit(ch);
            if (is_digit) {
                acc = acc * 10 + (ch - '0');
                if (!in_middle_number && col != 0) {
                    my_assert(!symbol_found);
                    // check west, northwest, and southwest for symbol
                    symbol_found = is_symbol(grid[row][col-1]);  // west
                    if (!symbol_found && row > 0) {
                        symbol_found = is_symbol(grid[row-1][col-1]);  // northwest
                    }
                    if (!symbol_found && !at_bottom) {
                        symbol_found = is_symbol(grid[row+1][col-1]);  // southwest
                    }
                }
                if (!in_middle_number) {
                    left_col = col;
                    in_middle_number = true;
                }
                if (!symbol_found && row > 0) {
                    symbol_found = is_symbol(grid[row-1][col]);  // north
                }
                if (!symbol_found && !at_bottom) {
                    symbol_found = is_symbol(grid[row+1][col]);  // south
                }
            } else {  // !is_digit
                if (!in_middle_number) { continue; }
                if (!symbol_found) {
                    symbol_found = is_symbol(ch);
                    // east of last digit
                }
                if (!symbol_found && row > 0) {
                    symbol_found = is_symbol(grid[row-1][col]);
                    // north here means northeast of last digit
                }
                if (!symbol_found && !at_bottom) {
                    symbol_found = is_symbol(grid[row+1][col]);
                    // south here means southeast of last digit
                }
                if (symbol_found) {
                    for (std::size_t k = left_col; k < col; ++k) {
                        point_to_left[{row, k}] = {row, left_col};
                    }
                    left_to_acc[{row, left_col}] = acc;
                }
                acc = 0;
                symbol_found = false;
                in_middle_number = false;
                // VVV technically not necessary, and technically doesn't help
                // ideally I would use an optional.
                left_col = 0;
            }
        }
        // same logic as above, just need to do it outside
        // only difference is k < ...
        if (in_middle_number) {
            if (symbol_found) {
                for (std::size_t k = left_col; k < grid.front().size(); ++k) {
                    point_to_left[{row, k}] = {row, left_col};
                }
                left_to_acc[{row, left_col}] = acc;
            }
            acc = 0;
            symbol_found = false;
            in_middle_number = false;
            left_col = 0;
        }
    }
    decltype(left_to_acc)::mapped_type part1 = 0;
    for (const auto & [key, value] : left_to_acc) {
        part1 += value;
    }
    std::cout << "part 1 = " << part1 << '\n';  // 553079
    unsigned long part2 = 0;
    for (std::size_t row = 0; row < grid.size(); ++row) {
        for (std::size_t col = 0; col < grid.front().size(); ++col) {
            if ('*' != grid[row][col]) { continue; }
            std::set<Point> part_left;
            // look at neighbors
            for (int dr = -1; dr <= 1; ++dr) {
                for (int dc = -1; dc <= 1; ++dc) {
                    if (dr == 0 && dc == 0) { continue; }
                    if (dr == -1 && row == 0) { continue; }
                    if (dc == -1 && col == 0) { continue; }
                    if (dr == 1 && row + 1 == grid.size()) { continue; }
                    if (dc == 1 && col + 1 == grid.front().size()) { continue; }
                    const auto found = point_to_left.find({row + dr, col + dc});
                    if (found != point_to_left.cend()) {
                        part_left.insert(found->second);
                    }
                }
            }
            if (part_left.size() == 2) {
                decltype(part2) prod = 1;
                for (const auto &point : part_left) {
                    prod *= left_to_acc[point];
                }
                part2 += prod;
            }
        }
    }
    std::cout << "part 2 = " << part2 << '\n';  // 84363105
}

int main() {
    constexpr char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::vector<std::string> grid;
        std::string line;
        while (std::getline(fs, line)) {
            grid.push_back(std::move(line));
            // avoid copy. Pretty sure can getline on it again.
            // https://stackoverflow.com/a/7028318/2990344
        }
        solve(grid);
    }
}
