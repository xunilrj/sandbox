#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_set>

using std::vector;
using std::pair;

class DepthFirstSearch
{
public:
  int reach(vector<vector<int> > &adj, int x, int y) {
    explore(adj, x);

    return visited.find(y) != visited.end();
  }
private:
  void explore(vector<vector<int> > &adj, int x)
  {
    if(visited.find(x) != visited.end())
      return;

    visited.insert(x);
    for(auto edge_end : adj[x])
    {
      explore(adj, edge_end);
    }
  }
  std::unordered_set<int> visited;
};

int reach(vector<vector<int> > &adj, int x, int y) {
  return DepthFirstSearch().reach(adj, x, y);
}

void run(std::istream& in, std::ostream& out)
{
  size_t n, m;
  in >> n >> m;

  vector<vector<int> > adj(n, vector<int>());
  for (size_t i = 0; i < m; i++) {
    int x, y;
    in >> x >> y;
    adj[x - 1].push_back(y - 1);
    adj[y - 1].push_back(x - 1);
  }

  int x, y;
  in >> x >> y;
  out << reach(adj, x - 1, y - 1);
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& strin, const std::string& expectedOut)
{
  auto in = std::stringstream{strin};
  auto actualOut = std::stringstream();

  run(in, actualOut);

  REQUIRE(expectedOut == actualOut.str());
}

TEST_CASE("must find solution for simple cases","maze tests")
{
  test(R"(4 4 
  1 2
  3 2
  4 3
  1 4
  1 4)","1");
  test(R"(4 2
    1 2
    3 2
    1 4)","0");
}

#else

int main() {
  run(std::cin, std::cout);
}

#endif