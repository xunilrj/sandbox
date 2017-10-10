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
  int countComponents(vector<vector<int> > &adj)
  {
    nextComponent = -1;
    for(int i = 0;i < adj.size(); ++i)
    {
      if(visited.find(i) != visited.end())
        continue;

      nextComponent++;
      explore(adj, i);
    }

    return nextComponent + 1;
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

  int nextComponent;
  std::unordered_set<int> visited;
};

int number_of_components(vector<vector<int> > &adj) {
  auto search = DepthFirstSearch();
  return search.countComponents(adj);
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
  out << number_of_components(adj);
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
    1 4)","2");
}

#else

int main() {
  run(std::cin, std::cout);
}

#endif
