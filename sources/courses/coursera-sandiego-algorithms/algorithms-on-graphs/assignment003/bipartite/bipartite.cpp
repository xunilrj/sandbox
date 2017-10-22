#include <iostream>
#include <vector>
#include <queue>
#include <limits>
#include <functional>

using std::vector;
using std::queue;

const int NOTVISITED = std::numeric_limits<int>::max();

int bipartite(vector<vector<int> > &adj) {
  int s = 0;

  std::vector<int> color(adj.size(), NOTVISITED);
  std::queue<int> q;
  
  q.push(s);
  color[s] = 0;
  
  while(q.size() > 0)
  {
    auto current = q.front();
    q.pop();
    auto currentColor = color[current];

    auto edges = adj[current];
    for(auto target : edges)
    {
      if(color[target] == NOTVISITED)
      {
        q.push(target);
        auto targetColor = currentColor == 0 ? 1 : 0;          
        color[target] = targetColor;
      }
      else
      {
        auto targetColor = color[target];
        if(currentColor == targetColor) return 0;
      }
    }
  }

  return 1;
}

void run(std::istream& in, std::ostream& out)
{
  int n, m;
  in >> n >> m;
  vector<vector<int> > adj(n, vector<int>());
  for (int i = 0; i < m; i++) {
    int x, y;
    in >> x >> y;
    adj[x - 1].push_back(y - 1);
    adj[y - 1].push_back(x - 1);
  }
  out << bipartite(adj);
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
  test(R"(4 4
    1 2
    4 1
    2 3
    3 1)", "0");
  test(R"(5 4
    5 2
    4 2
    3 4
    1 4)", "1");    
}

#else 

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
