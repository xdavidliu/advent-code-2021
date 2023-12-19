#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <tuple>
#include <exception>

void my_assert(const bool cond, const std::string_view msg) {
  if (!cond) {
    std::cout << msg << '\n';
    throw std::exception();
  }
}

auto split_comma(const std::string &word) {
  std::vector<std::string> out;
  std::size_t left = 0, right = word.find(',');
  while (right != std::string::npos) {
    out.push_back(word.substr(left, right - left));
    left = right + 1;
    right = word.find(',', left);
  }
  // last one has npos, so
  out.push_back(word.substr(left));
  return out;
}

void add_to_map(const std::string &line, std::map<std::string, std::vector<std::string>> &ins_map) {
  const auto left_brace = line.find('{');
  const auto ins_str = line.substr(left_brace + 1, line.size() - left_brace - 2);
  ins_map.insert({line.substr(0, left_brace), split_comma(ins_str)});
}

// {x=787,m=2655,a=1222,s=2876}
auto get_values(const std::string &word) {
  std::vector<long> out;
  const auto items = split_comma(word.substr(1, word.size() - 2));
  for (const auto &item : items) {
    out.push_back(std::stol(item.substr(2)));
  }
  return out;
}

void foo1() {
  constexpr char filepath[] = "/tmp/example.txt";
  if (auto fs = std::ifstream(filepath)) {
    std::map<std::string, std::vector<std::string>> ins_map;
    std::string word;
    while ((fs >> word) && word.front() != '{') {
      std::cout << word << '\n';
    }
    std::cout << "=====\n";
    std::cout << word << '\n';  // first one already read
    while (fs >> word) {
      std::cout << word << '\n';
    }
  } else {
    my_assert(false, "wrong filepath");
  }
}

void foo2() {
  const std::string word = "px{a<2006:qkq,m>2090:A,rfg}";
  std::map<std::string, std::vector<std::string>> ins_map;
  add_to_map(word, ins_map);
  for (const auto &[key, val] : ins_map) {
    std::cout << key << '\n';
    for (const auto &elem : val) {
      std::cout << elem << '\n';
    }
    std::cout << '\n';
  }

}


int main() {
  foo2();
}

/*
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
 */
