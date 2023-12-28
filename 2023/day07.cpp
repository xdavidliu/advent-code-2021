#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <exception>
#include <string_view>
#include <functional>

struct Hand {
    std::string cards;
    int bid, type, type2;
};

int get_type(std::string_view hand) {
    char cards[5];
    int counts[5];
    int top = 0;
    for (const auto &card: hand) {
        const auto found = std::find(std::cbegin(cards), top + std::cbegin(cards), card);
        if (top + std::cbegin(cards) == found) {
            cards[top] = card;
            counts[top] = 1;
            ++top;
        } else {
            ++counts[found - std::cbegin(cards)];
        }
    }
    switch (top) {
        case 5:
            return 0;  // high card
        case 4:
            return 1;  // one pair
        case 3: {
            for (std::size_t i = 0; i < top; ++i) {
                if (counts[i] == 3) { return 3; }  // three kind
            }
            return 2;  // two pair
        }
        case 2: {
            for (std::size_t i = 0; i < top; ++i) {
                if (counts[i] == 4) { return 5; }  // four kind
            }
            return 4;  // full house
        }
        case 1:
            return 6;  // five of a kind
        default:
            throw std::exception();  // not possible
    }
}

int get_type2(std::string_view hand) {
    if (hand.find('J') == std::string_view::npos) { return get_type(hand); }
    int best = 0;
    constexpr std::string_view cards = "23456789TQKA";
    for (const auto &possible: cards) {
        std::string other;
        for (const auto &ch: hand) {
            other.push_back(ch == 'J' ? possible : ch);
        }
        best = std::max(best, get_type(other));
    }
    return best;
}

int card_value(const char card) {
    switch (card) {
        case 'T':
            return 10;
        case 'J' :
            return 11;
        case 'Q' :
            return 12;
        case 'K' :
            return 13;
        case 'A' :
            return 14;
        default:
            return card - '0';
    }
}

int card_value2(const char card) {
    if (card == 'J') { return 1; }
    else { return card_value(card); }
}

bool hand_compare_less_with_funcs(
        const Hand &left, const Hand &right,
        std::function<int(const char)> value_func,
        std::function<int(const Hand &)> type_func) {
    if (type_func(left) == type_func(right)) {
        int left_arr[5], right_arr[5];
        std::transform(left.cards.cbegin(), left.cards.cend(),
                       std::begin(left_arr), value_func);
        std::transform(right.cards.cbegin(), right.cards.cend(),
                       std::begin(right_arr), value_func);
        return std::lexicographical_compare(
                std::cbegin(left_arr), std::cend(left_arr),
                std::cbegin(right_arr), std::cend(right_arr));
    }
    return type_func(left) < type_func(right);
}

bool hand_compare_less(const Hand &left, const Hand &right) {
    const auto type_func = [](const Hand &hand) { return hand.type; };
    return hand_compare_less_with_funcs(left, right, card_value, type_func);
}

bool hand_compare_less2(const Hand &left, const Hand &right) {
    const auto type_func2 = [](const Hand &hand) { return hand.type2; };
    return hand_compare_less_with_funcs(left, right, card_value2, type_func2);
}

int main() {
    constexpr char path[] = "/home/xdavidliu/Documents/temp/data.txt";
    if (auto fs = std::ifstream(path)) {
        std::string cards;
        std::vector<Hand> hands;
        while (fs >> cards) {
            Hand hand{cards, 0};
            fs >> hand.bid;
            hand.type = get_type(hand.cards);
            hand.type2 = get_type2(hand.cards);
            hands.push_back(hand);
        }
        std::sort(hands.begin(), hands.end(), hand_compare_less);
        std::size_t part1 = 0;
        for (std::size_t i = 0; i < hands.size(); ++i) {
            // example data tricks you! Still works if inverted.
            part1 += (i + 1) * hands[i].bid;
        }
        std::cout << "part 1 = " << part1 << '\n';  // 249638405

        std::sort(hands.begin(), hands.end(), hand_compare_less2);
        std::size_t part2 = 0;
        for (std::size_t i = 0; i < hands.size(); ++i) {
            part2 += (i + 1) * hands[i].bid;
        }
        std::cout << "part 2 = " << part2 << '\n';  // 249776650
    }
}
