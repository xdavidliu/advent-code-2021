#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <exception>
#include <fstream>
#include <sstream>
#include <string_view>
#include <memory>

struct Input {
    char type = 0;
    std::string lhs;
    std::vector<std::string> rhs;
};

void push_comma_separated(std::istringstream &iss, std::vector<std::string> &out) {
    std::string word;
    while (iss >> word) {
        if (word.back() == ',') {
            out.push_back(word.substr(0, word.size()-1));
        } else {
            out.push_back(word);
        }
    }
}

Input from_line(const std::string &line) {
    if (line.find("broadcaster") == 0) {
        std::cout << "from line called on broadcaster\n";
        throw std::exception();
    }
    std::istringstream iss(line);
    Input input;
    iss >> input.type;
    iss >> input.lhs;
    std::string ignore;
    iss >> ignore;  // ->
    push_comma_separated(iss, input.rhs);
    return input;
}

std::pair<std::vector<std::string>, std::vector<Input>> read_file(const char *filepath) {
    std::vector<Input> inputs;
    if (auto fs = std::ifstream(filepath)) {
        std::string line;
        std::vector<std::string> broadcast;
        while (std::getline(fs, line)) {
            if (0 == line.find("broadcaster")) {
                constexpr auto start = std::string_view("broadcaster -> ").size();
                std::istringstream iss(line.substr(start));
                push_comma_separated(iss, broadcast);
            } else {
                inputs.push_back(from_line(line));
            }
        }
        return {broadcast, inputs};
    } else {
        std::cout << "invalid filepath\n";
        throw std::exception();
    }
}

// todo: if RHS has output, that's not a module

class Module {
    const std::vector<std::string> dests;
public:
    explicit Module(std::vector<std::string> dests): dests(std::move(dests)) {}
    virtual void receive() = 0;
    virtual void send() = 0;
    virtual ~Module() = default;
};

/*
 * have inputs, create map of all lhs, key lhs to val shared_ptr<Module>
 * Module can be Flip or Conjunct
 * for each input, for each elem in rhs, insert into lhs's module's dests field
 * also do inputs. For conjuncts, also have srcs field, populated during above
 * for each module, have last_sent so conjuncts can easily see
 */

void foo1() {
    constexpr char filepath[] = "/home/employee/Documents/temp/example1.txt";
    const auto [broadcast, inputs] = read_file(filepath);
    std::cout << "broadcast: \n";
    for (const auto &elem : broadcast) {
        std::cout << elem << ' ';
    }
    std::cout << "\n =========== \n";
    for (const auto &input : inputs) {
        std::cout << input.type << ' ' << input.lhs << " -> ";
        for (const auto &elem : input.rhs) {
            std::cout << elem << ' ';
        }
        std::cout << '\n';
    }
}

int main() {
    foo1();
}
