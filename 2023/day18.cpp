#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <exception>
#include <limits>

constexpr char filepath[] = "/tmp/data.txt";

struct Dig {
  char dir;
  long steps;
};

using Position = std::pair<long, long>;

Dig dig_from(const std::string& rgb) {
  // 2 because of (#
  const auto steps = std::stoi(rgb.substr(2, 5), nullptr, 16);
  const auto second_to_last = *(rgb.crbegin() + 1);
  switch (second_to_last) {
    case '0':return {'R', steps};
    case '1':return {'D', steps};
    case '2':return {'L', steps};
    case '3':return {'U', steps};
  }
  throw std::exception();
}

std::pair<std::vector<Dig>, std::vector<Dig>> read_digs() {
  if (auto fs = std::fstream(filepath)) {
    std::vector<Dig> out1, out2;
    long steps = 0;
    char dir;
    std::string rgb;
    while (fs >> dir) {
      fs >> steps >> rgb;
      out1.push_back({dir, steps});
      out2.push_back(dig_from(rgb));
    }
    return {out1, out2};
  } else {
    throw std::exception();
  }
}

struct Edge {
  long r0, c0, r1, c1;
};

Edge make_edge(const Position &pos, const Dig &dig) {
  const auto [row, col] = pos;
  const auto [dir, steps] = dig;
  switch (dir) {
    case 'U': return {row, col, row-steps, col};
    case 'D': return {row, col, row+steps, col};
    case 'L': return {row, col, row, col-steps};
    case 'R': return {row, col, row, col+steps};
  }
  throw std::exception();
}

auto convert_to_edges(const std::vector<Dig> &digs) {
  std::vector<Edge> out;
  Position pos{0, 0};
  for (const auto &dig : digs) {
    const auto edge = make_edge(pos, dig);
    pos = {edge.r1, edge.c1};
    out.push_back(edge);
  }
  return out;
}

void solve(const std::vector<Edge> &edges) {
  constexpr auto inf = std::numeric_limits<long>::max();
  constexpr auto neg_inf = std::numeric_limits<long>::min();
  // must be opposite because will use min and max
  long far_left = inf, far_right = neg_inf, far_up = inf, far_down = neg_inf;
  for (const auto &[r0, c0, r1, c1] : edges) {
    far_left = std::min(far_left, std::min(c0, c1));
    far_up = std::min(far_up, std::min(r0, r1));
    far_right = std::max(far_right, std::max(c0, c1));
    far_down = std::max(far_down, std::max(r0, r1));
  }
  std::cout << far_left << " left\n";
  std::cout << far_right << " right\n";
  std::cout << far_up << " up\n";
  std::cout << far_down << " down\n";
}

int main() {
  const auto [digs1, digs2] = read_digs();
  const auto edges1 = convert_to_edges(digs1);
  const auto edges2 = convert_to_edges(digs2);
  std::cout << "row = [";
  for (const auto &e : edges2) {
    std::cout << e.r0 << ',' << e.r1 << ',';
  }
  std::cout << "];\n";
  std::cout << "col = [";
  for (const auto &e : edges2) {
    std::cout << e.c0 << ',' << e.c1 << ',';
  }
  std::cout << "];\n";
  solve(edges1);
  // part 1 = 49897

  // find lowest and highest of row and col
  // find first and last time it's at each of them
  // integrate around
  // do example and data for part 1, confirm

}
