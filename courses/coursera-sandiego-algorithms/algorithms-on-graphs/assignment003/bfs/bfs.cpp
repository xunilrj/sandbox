#include <iostream>
#include <vector>
#include <queue>
#include <limits>

using std::vector;
using std::queue;

const int NOTVISITED = std::numeric_limits<int>::max();
int distance(vector<vector<int> > &adj, int s, int t) {
  std::vector<int> previous(adj.size(), NOTVISITED);

  std::queue<int> q;
  q.push(s);

  while(q.size() > 0)
  {
    auto current = q.front();
    q.pop();

    if(current == t)
    {
      break;
    }

    auto edges = adj[current];
    for(auto target : edges)
    {
      if(previous[target] == NOTVISITED)
      {
        q.push(target);
        previous[target] = current;
      }
    }
  }

  auto current = t;
  if(previous[current] == NOTVISITED) return -1;

  std::deque<int> path;
  while(current != s)
  {
    path.push_front(current);
    current = previous[current];
  }
  
  return path.size();
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
  int s, t;
  in >> s >> t;
  s--, t--;
  out << distance(adj, s, t);
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
    3 1
    2 4)", "2");
  test(R"(5 4
    5 2
    1 3
    3 4
    1 4
    3 5)", "-1");    
}

#else 

int main() {
 run(std::cin, std::cout);
 return 0;
}

#endif