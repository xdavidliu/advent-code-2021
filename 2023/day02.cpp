#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <vector>
#include <charconv>

// in hindsight, doing the below without regex is way too tedious
// also would've been way more convenient in python / java / kotlin
// maybe even rust

// improved from my 2017 day07.cpp
std::vector<std::string_view> split(std::string_view text, std::string_view delim) {
    std::vector<std::string_view> out;
    std::size_t pos = 0;
    while (true) {
        auto next = text.find(delim, pos);
        if (next == std::string::npos) {
            out.emplace_back(text.substr(pos, text.size() - pos));
            break;
        } else {
            out.emplace_back(text.substr(pos, next - pos));
            pos = next + delim.size();
        }
    }
    return out;
}

enum class Color {
    BLUE, RED, GREEN
};

Color from_color_str(std::string_view s) {
    if (s == "blue") { return Color::BLUE; }
    else if (s == "green") { return Color::GREEN; }
    else if (s == "red") { return Color::RED; }
    else { throw std::exception(); }
}

struct Draw {
    int count;
    Color color;
};

struct Set {
    std::vector<Draw> draws;
};

struct Game {
    int id{};  // not sure why compiler suggested {} here
    std::vector<Set> sets;
};

Set read_set(std::string_view draw) {
    const auto by_comma = split(draw, ", ");
    Set set;
    for (const auto &part : by_comma) {
        const auto space = part.find(" ");
        int count;
        const auto [ptr, err]  = std::from_chars(part.data(), part.data() + space, count);
        if (err != std::errc()) { throw std::exception(); }
        const auto color_str = part.substr(space + 1);
        // can't use switch case with string_view
        set.draws.push_back({count, from_color_str(color_str)});
    }
    return set;
}

Game read_game(const std::string &line) {
    Game game;
    std::istringstream iss(line);
    iss.ignore(5);  // "Game "
    iss >> game.id;

    constexpr std::string_view colon = ": ";
    const auto after_colon = line.find(colon) + colon.size();
    std::string_view view(line);  // to avoid accidental allocations from substr later
    const auto after = view.substr(after_colon);
    const auto parts = split(after, "; ");
    for (const auto &part : parts) {
        game.sets.push_back(read_set(part));
    }
    return game;  // how the heck does it still compile without this line?
    // CLion doesn't even warn me at all.
    // https://stackoverflow.com/a/21870350/2990344
    // you gotta be kidding me
}

bool works(const Set &set) {
    for (const auto &draw : set.draws) {
        switch (draw.color) {
            case Color::BLUE : { if (draw.count > 14) return false; break; }
            case Color::RED : { if (draw.count > 12) return false; break; }
            case Color::GREEN : { if (draw.count > 13) return false; break; }
        }
    }
    return true;
}

long power(const Game &game) {
    int red = 0, blue = 0, green = 0;
    for (const auto &set : game.sets) {
        for (const auto &draw : set.draws) {
            switch (draw.color) {
                case Color::RED :
                    red = std::max(red, draw.count);
                    break;
                case Color::BLUE :
                    blue = std::max(blue, draw.count);
                    break;
                case Color::GREEN :
                    green = std::max(green, draw.count);
                    break;
            }
        }
    }
    return static_cast<long>(red) * blue * green;
}

int main() {
    if (auto fs = std::ifstream("/home/xdavidliu/Documents/temp/data.txt")) {
        std::string line;
        int part1 = 0;
        long part2 = 0;
        while (std::getline(fs, line)) {
            const auto game = read_game(line);
            if (std::all_of(game.sets.cbegin(), game.sets.cend(), works)) {
                part1 += game.id;
            }
            part2 += power(game);
        }
        std::cout << "part 1 = " << part1 << '\n';  // 2265
        std::cout << "part 2 = " << part2 << '\n';  // 64097 too low
    }
}
