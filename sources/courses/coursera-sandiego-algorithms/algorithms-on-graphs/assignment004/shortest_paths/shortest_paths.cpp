#include <iostream>
#include <limits>
#include <vector>
#include <queue>

using std::vector;
using std::queue;
using std::pair;
using std::priority_queue;

void shortest_paths(vector<vector<int> > &adj, vector<vector<int> > &cost, int s, vector<long long> &distance, vector<int> &reachable, vector<int> &shortest) {
  //write your code here
}

void run (std::istream& in, std::ostream& out)
{
  int n, m, s;
  in >> n >> m;
  vector<vector<int> > adj(n, vector<int>());
  vector<vector<int> > cost(n, vector<int>());
  for (int i = 0; i < m; i++) {
    int x, y, w;
    in >> x >> y >> w;
    adj[x - 1].push_back(y - 1);
    cost[x - 1].push_back(w);
  }
  in >> s;
  s--;
  vector<long long> distance(n, std::numeric_limits<long long>::max());
  vector<int> reachable(n, 0);
  vector<int> shortest(n, 1);
  shortest_paths(adj, cost, s, distance, reachable, shortest);
  for (int i = 0; i < n; i++) {
    if (!reachable[i]) {
      out << "*\n";
    } else if (!shortest[i]) {
      out << "-\n";
    } else {
      out << distance[i] << "\n";
    }
  }
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string &instr, const std::string& expectedOut)
{
  auto in = std::stringstream{instr};
  auto actualOut = std::stringstream();

  run(in, actualOut);

  REQUIRE(expectedOut == actualOut.str());
}

TEST_CASE("","")
{
  test(R"(
    4 4
    1 2 1
    4 1 2
    2 3 2
    1 3 5
    1 3
  )", "3");
}

#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
