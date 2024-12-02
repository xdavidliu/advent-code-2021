#include <iostream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>
#include <fstream>
#include <set>
#include <map>
#include <algorithm>
#include <optional>

struct Brick {
    std::size_t x0, y0, z0, x1, y1, z1;
    static Brick from_line(const std::string &line) {
        // 1,1,288~4,1,288
        std::istringstream iss(line);
        std::size_t val;
        char ch;
        std::vector<std::size_t> vals;
        for (std::size_t i = 0; i < 6; ++i) {
            iss >> val;
            vals.push_back(val);
            iss >> ch;
        }
        return {vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]};
    }
    // https://stackoverflow.com/q/217911/2990344
    bool operator==(const Brick &other) const {
        return x0 == other.x0 && x1 == other.x1 && y0 == other.y0 && y1 == other.y1 && z0 == other.z0 && z1 == other.z1;
    }
    bool operator!=(const Brick &other) const {
        return !operator==(other);
    }
    bool operator<(const Brick &rhs) const {
        const auto tup_lhs = std::make_tuple(x0, y0, z0, x1, y1, z1);
        const auto tup_rhs = std::make_tuple(rhs.x0, rhs.y0, rhs.z0, rhs.x1, rhs.y1, rhs.z1);
        return tup_lhs < tup_rhs;
    }
    operator std::string() const {
        std::ostringstream oss;
        oss << '(' << x0 << ',' << y0 << ',' << z0 << '~';
        oss << x1 << ',' << y1 << ',' << z1 << ')';
        return oss.str();
    }
};

bool bottom_below(const Brick &lhs, const Brick &rhs) {
    return lhs.z0 < rhs.z0;
}

bool is_overlap(const std::size_t a0, const std::size_t a1, const std::size_t b0, const std::size_t b1) {
    return !(a1 < b0 || b1 < a0);
}

bool is_overlap(const Brick &a, const Brick &b) {
    return is_overlap(a.x0, a.x1, b.x0, b.x1) && is_overlap(a.y0, a.y1, b.y0, b.y1);
}

auto compute_by_top_and_bottom(const std::vector<Brick> &bricks_at_snap_by_bottom) {
    std::vector<std::vector<Brick>> bricks_by_top, bricks_by_bottom;
    for (const auto &brick : bricks_at_snap_by_bottom) {
        const auto start = std::min(brick.z0 - 1, bricks_by_top.size() - 1);
        auto brick_copy = brick;
        brick_copy.z0 = 1;  // assume no overlaps, then correct if overlap found
        brick_copy.z1 = 1 + brick.z1 - brick.z0;
        for (std::size_t z1 = start; z1 != 0; --z1) {
            const auto overlaps_with_brick = [&brick] (const auto &other) { return is_overlap(brick, other); };
            if (std::any_of(bricks_by_top[z1].cbegin(), bricks_by_top[z1].cend(), overlaps_with_brick)) {
                brick_copy.z0 = z1 + 1;
                brick_copy.z1 = z1 + 1 + brick.z1 - brick.z0;
                break;
            }
        }
        while (bricks_by_top.size() <= brick_copy.z1) {
            bricks_by_top.emplace_back();
        }
        bricks_by_top[brick_copy.z1].push_back(brick_copy);
        while (bricks_by_bottom.size() <= brick_copy.z0) {
            bricks_by_bottom.emplace_back();
        }
        bricks_by_bottom[brick_copy.z0].push_back(brick_copy);
    }
    return std::make_pair(bricks_by_top, bricks_by_bottom);
}

void count_freq(const std::vector<Brick> &bricks) {
    std::map<std::size_t, std::size_t> freq;
    for (const auto &brick : bricks) {
        // const auto metric = (brick.x1 - brick.x0 + 1) * (brick.y1 - brick.y0 + 1);
        const auto metric = brick.z1 - brick.z0 + 1;
        auto [iter, worked] = freq.insert({metric, 0});
        ++iter->second;
    }
    for (const auto &[metric, count] : freq) {
        std::cout << metric << ": " << count << " times\n";
    }
    // all x1 >= x0, y1 >= y0, z1 >= z0
}

std::size_t count_sole_overlap(const std::vector<Brick> &tops, const std::vector<Brick> &bottoms) {
    std::vector<Brick> found;
    for (const auto &bot : bottoms) {
        std::optional<Brick> top_below;
        bool found_two = false;
        for (const auto &top : tops) {
            if (is_overlap(top, bot)) {
                if (top_below.has_value()) {
                    found_two = true;
                    break;
                } else {
                    top_below.emplace(top);
                }
            }
        }
        if (!found_two && top_below.has_value()) {
            // alternative to implementing comparator for Brick
            const auto already = std::find(found.cbegin(), found.cend(), top_below.value());
            if (already == found.cend()) {
                found.push_back(top_below.value());
            }
        }
    }
    return found.size();
}

auto how_many_fall_without(const std::size_t z1_init, const Brick &to_remove,
                           const std::vector<std::vector<Brick>> &bricks_by_top,
                           const std::vector<std::vector<Brick>> &bricks_by_bottom)
{
    // oh, you can't overlap last; those are BOTTOMS. You need to do TOPS.
    std::set<Brick> removed;
    removed.insert(to_remove);
    for (std::size_t z0 = z1_init + 1; z0 < bricks_by_bottom.size(); ++z0) {
        const auto &below = bricks_by_top[z0 - 1];
        for (const auto &brick : bricks_by_bottom[z0]) {
            const auto overlaps_non_removed = [&brick, &removed] (const Brick &other) {
                return 0 == removed.count(other) && is_overlap(brick, other);
            };
            if (std::none_of(below.cbegin(), below.cend(), overlaps_non_removed)) {
                removed.insert(brick);
            }
        }
    }
    return removed.size() - 1;  // don't count first one
}

auto do_part2(const std::vector<std::vector<Brick>> &bricks_by_top,
              const std::vector<std::vector<Brick>> &bricks_by_bottom)
{
    std::size_t count = 0;
    // z1 != 0 because lowest z is 1
    for (std::size_t z1 = bricks_by_bottom.size() - 2; z1 != 0; --z1) {
        for (const auto &to_remove : bricks_by_top[z1]) {
            count += how_many_fall_without(z1, to_remove, bricks_by_top, bricks_by_bottom);
        }
    }
    return count;
}

int main() {
    const char *filepath = "/home/employee/Documents/temp/data.txt";
    auto fs = std::ifstream(filepath);
    std::string line;
    std::vector<Brick> bricks_at_snap;
    while (std::getline(fs, line)) {
        bricks_at_snap.push_back(Brick::from_line(line));
    }
    std::sort(bricks_at_snap.begin(), bricks_at_snap.end(), bottom_below);
    const auto [bricks_by_top, bricks_by_bottom] = compute_by_top_and_bottom(bricks_at_snap);
    std::size_t sole = 0;
    for (std::size_t bot = 2; bot < bricks_by_bottom.size(); ++bot) {
        const auto top = bot - 1;
        sole += count_sole_overlap(bricks_by_top[top], bricks_by_bottom[bot]);
    }
    // for (long z0 = 1; z0 < bricks_by_bottom.size()
    std::cout << "part 1 = " << (bricks_at_snap.size() - sole) << '\n';  // 393
    const auto part2 = do_part2(bricks_by_top, bricks_by_bottom);
    std::cout << "part 2 = " << part2 << '\n';  // 58440
}

/*
 * 1000 bricks, most one or two thick
 * most less than 3 x 3 across.
 * for each z, a list of bricks that stop there
 *
 */

// area
//1: 128 times
//2: 269 times
//3: 527 times
//4: 268 times
//5: 18 times
//
// height
//1: 1099 times
//2: 23 times
//3: 50 times
//4: 35 times
//5: 3 times
