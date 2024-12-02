#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <string_view>
#include <algorithm>

int hash(const std::string_view instruction) {
  int current = 0;
  for (const auto ch : instruction) {
    current = ((current + ch) * 17) % 256;
  }
  return current;
}

auto split_commas(const std::string &line) {
  std::vector<std::string> out;
  auto comma_pos = line.find(',');
  std::size_t beg = 0;
  while (comma_pos != std::string::npos) {
    out.push_back(line.substr(beg, comma_pos - beg));
    beg = comma_pos + 1;
    comma_pos = line.find(',', beg);
  }
  out.push_back(line.substr(beg, line.size() - beg));
  return out;
}

struct Lens {
  std::string label;
  int length;
};

void perform(const std::string ins, std::vector<std::vector<Lens>> &boxes) {
  const bool is_minus = ins.back() == '-';
  const auto op_pos = ins.find(is_minus ? '-' : '=');
  const auto label = ins.substr(0, op_pos);
  const auto label_hsh = hash(label);
  auto &box = boxes[label_hsh];
  const auto matches = [&label] (const Lens &lens) { return lens.label == label; };
  if (ins.back() == '-') {
    auto rem = std::remove_if(box.begin(), box.end(), matches);
    box.erase(rem, box.end());
    // ugh, erase remove idiom. Horrible.
    // https://stackoverflow.com/a/347478
  } else {  // =
    const auto focal_str = ins.substr(1 + op_pos, ins.size() - 1 - op_pos);
    const auto focal = std::stoi(focal_str);
    auto found = std::find_if(box.begin(), box.end(), matches);
    if (found != box.end()) {
      found->length = focal;
    } else {
      box.push_back({label, focal});
    }
  }
}

int main() {
  if (auto fs = std::ifstream("/tmp/data.txt")) {
    std::string line;
    std::getline(fs, line);
    const auto tokens = split_commas(line);
    std::vector<std::vector<Lens>> boxes(256);
    long part1 = 0, part2 = 0;
    for (const auto &token : tokens) {
      const auto hsh = hash(token);
      part1 += hsh;
      perform(token, boxes);
    }
    std::cout << "part 1 = " << part1 << '\n';  // 521341
    for (std::size_t i_box = 0; i_box < boxes.size(); ++i_box) {
      const auto &box = boxes[i_box];
      for (std::size_t i_lens = 0; i_lens < box.size(); ++i_lens) {
        part2 += (1 + i_box) * (1 + i_lens) * box[i_lens].length;
      }
    }
    std::cout << "part 2 = " << part2 << '\n';  // 252782
  }
}
