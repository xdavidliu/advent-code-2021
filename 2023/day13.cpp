#include <iostream>
#include <fstream>
#include <string>
#include <vector>

auto question_positions(const std::string &line) {
  std::vector<int> pos;
  for (int i = 0; i < line.size(); ++i) {
    if (line[i] == '?') { pos.push_back(i); }
  }
  return pos;
}

auto split_nums(const std::string &list) {
  std::size_t i = 0;
  std::vector<int> nums;
  while (true) {
    const auto k = list.find(',', i);
    if (k == list.npos) {
      nums.push_back(std::stoi(list.substr(i)));
      break;
    } else {
      nums.push_back(std::stoi(list.substr(i, k-i)));
      i = k + 1;
    }
  }
  return nums;
}

struct Problem {
  std::string grid;
  std::vector<int> nums;
};

auto read_problems(const char *filepath) {
  std::vector<Problem> problems;
  if (auto fs = std::ifstream(filepath)) {
    std::string grid, nums;
    while (fs >> grid) {
      fs >> nums;
      problems.push_back({grid, split_nums(nums)});
    }
  }
  return problems;
}

void apply_mask(std::vector<bool> &bits, int mask) {
  for (int k = 0; k < bits.size(); ++k) {
    bits[k] = 1 == (mask & 1);
    mask >>= 1;
  }
}

void apply_fix(
    std::string &fixed, const std::string &grid,
    const std::vector<int> &pos, const std::vector<bool> &bits) {
  fixed = grid;
  for (int i = 0; i < pos.size(); ++i) {
    fixed[pos[i]] = bits[i] ? '#' : '.';
  }
}

bool check_works(const std::string &fixed, const std::vector<int> nums) {
  int pos = 0;
  for (const auto n : nums) {
    if (pos == fixed.size()) { return false; }
    const int left = fixed.find('#', pos);
    if (left == fixed.npos) { return false; }
    int right = fixed.find('.', left + 1);
    if (right == fixed.npos) { right = fixed.size(); }
    if (right - left != n) { return false; }
    pos = right;
  }
  while (pos < fixed.size()) {
    if (fixed[pos] == '#') { return false; }
    ++pos;
  }
  return true;
}

int solve(const Problem &problem) {
  const auto qpos = question_positions(problem.grid);
  std::vector<bool> bits(qpos.size());
  std::string fixed(problem.grid);
  const int power = 1 << qpos.size();
  int count = 0;
  for (int i = 0; i < power; ++i) {
    apply_mask(bits, i);
    apply_fix(fixed, problem.grid, qpos, bits);
    if (check_works(fixed, problem.nums)) {
      ++count;
    }
  }
  return count;
}

int main() {
  const auto problems = read_problems("/tmp/data.txt");
  int part1 = 0;
  for (const auto &prob : problems) {
    int s = solve(prob);
    part1 += s;
  }
  std::cout << "part 1 = " << part1 << '\n';  // 7017
}
