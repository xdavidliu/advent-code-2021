#include <iostream>
#include <fstream>
#include <vector>
#include <string>

constexpr char wall = '#';
constexpr char dot = '.';
constexpr char oh = 'O';
constexpr char must[] = "^<v>";

int dir_index(const char ch) {
    for (int i = 0; i < 4; ++i) {
        if (ch == must[i]) { return i; }
    }
    throw std::exception();
}

// pass grid by mutable copy
void recurse(std::vector<std::string> grid, int row_from, int col_from, int steps, int &best) {
    // U L D R -> ^ < v >
    constexpr int diff[4][2] = {{-1, 0}, {0, -1}, {1, 0}, {0, 1}};
    constexpr char cannot[] = "v>^<";  // effectively # because opposite direction
    while (true) {
        if (row_from == grid.size()-1 && col_from == grid.front().size()-2) {
            // std::cout << "found path " << (steps + 1) << '\n';
//            if (steps + 1 > best) {
//                best = steps + 1;
//                std::cout <<
//            }
            if (steps + 1 > best) {
                best = steps + 1;
                std::cout << best << ' ';
                std::cout.flush();
                // 4678 4714 4802 5134 5170 5238 5302 5634 5670 5742 5778 5782 5934 6042 6074 6106 6226 6230 6294 6314 6434 6450 6490 6498 6514 6554
            }
            // std::cout << best << ' ';
            return;
        }
        const auto this_ch = grid[row_from][col_from];
        if (this_ch == oh) {
            std::cout << "failed\n";
            return;
        }
        grid[row_from][col_from] = oh;
        if (this_ch != wall && this_ch != dot) {  // it must be in "must"
            const auto ind = dir_index(this_ch);
            row_from += diff[ind][0];
            col_from += diff[ind][1];
            ++steps;
            continue;
        }
        int count = 0, next_row[4], next_col[4];
        for (int i = 0; i < 4; ++i) {
            const auto [dr, dc] = diff[i];
            const auto row = row_from + dr, col = col_from + dc;
            // no bound check because all walls around
            const auto next_ch = grid[row][col];
            if (next_ch != wall && next_ch != oh && next_ch != cannot[i]) {
                next_row[count] = row;
                next_col[count] = col;
                ++count;
            }
        }
        if (count == 0) { return; }  // dead end
        else if (count == 1) {
            row_from = next_row[0];
            col_from = next_col[0];
            ++steps;
            continue;
        } else {  // count > 1
            for (int i = 0; i < count; ++i) {
                recurse(grid, next_row[i], next_col[i], steps + 1, best);
            }
            return;
        }
    }
}

void delete_slopes(std::vector<std::string> &grid) {
    for (auto &row : grid) {
        for (auto &ch : row) {
            if (ch != wall && ch != dot) { ch = dot; }
        }
    }
}

void foo1() {
    const char *filepath = "/home/employee/Documents/temp/data.txt";
    auto fs = std::ifstream(filepath);
    if (!fs) { return; }
    std::vector<std::string> grid;
    std::string line;
    while (std::getline(fs, line)) {
        grid.push_back(line);
    }
    grid[0][1] = wall;  // hack so no need to bound check
    auto grid_part2 = grid;
    delete_slopes(grid_part2);
    int best = -1;
    // 0 steps because (1,1) not shaded yet
//    recurse(grid, 1, 1, 0, best);
    std::cout << "part 1 = " << best << '\n';  // 2394

    int best2 = -1;
    recurse(grid_part2, 1, 1, 0, best2);
    std::cout << "part 2 = " << best2 << '\n';  // 6554 but doesn't actually complete running, but already good enough

}

int main() {
    foo1();
}
