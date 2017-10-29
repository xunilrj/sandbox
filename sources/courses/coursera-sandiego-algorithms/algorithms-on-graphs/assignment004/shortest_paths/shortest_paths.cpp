#include <iostream>
#include <limits>
#include <vector>
#include <queue>
#include <set>

using std::vector;
using std::queue;
using std::pair;
using std::priority_queue;

const int INF = std::numeric_limits<int>::max();
const long long INFL = std::numeric_limits<long long>::max();

void bellmanford(vector<vector<int> > &adj,
  vector<vector<int> > &cost,
  std::vector<long long> &dist,
  vector<int> &reachable,
  bool fillreachable,
  std::vector<int> &prev,
  std::queue<int> &onnegcycle,
  int source) {
    
  dist[source] = 0;
  if(fillreachable) reachable[source] = 1;

  int V = adj.size();
  for(int i = 0;i < (V-1);++i)
  {
    bool anychange = false;

    for(int u = 0;u < adj.size(); ++u)
    {
      if(dist[u] == INFL) continue;
      auto edges = adj[u];
      for(int vi = 0;vi < edges.size();++vi)
      {
        int v = edges[vi];
        int wuv = cost[u][vi];
        if(dist[v] > dist[u] + wuv)
        {
          anychange = true;
          if(fillreachable) reachable[v] = 1;
          dist[v] = dist[u] + wuv;
          prev[v] = u;
        }
      }
    }

    if(anychange == false) break;
  }

  
  for(int u = 0;u < adj.size(); ++u)
  {
    if(dist[u] == INFL) continue;
    auto edges = adj[u];
    for(int vi = 0;vi < edges.size();++vi)
    {
      int v = edges[vi];
      int wuv = cost[u][vi];
      if(dist[v] > dist[u] + wuv)
      {
        onnegcycle.push(u);
        dist[v] = dist[u] + wuv;
      }
    }
  }
}

void bfs(
  vector<vector<int> > &adj, 
  std::queue<int> &findreachables,
  vector<int> &shortest) {
  
  std::set<int> visited;
  while(findreachables.size() > 0)
  {
    auto u = findreachables.front();
    findreachables.pop();

    auto it = visited.find(u);
    if(it != visited.end()) continue;
    visited.insert(u);

    shortest[u] = 0;

    auto uedges = adj[u];
    for(int vi = 0; vi < uedges.size(); ++vi)    
    {
      auto v = uedges[vi];
      findreachables.push(v);
    }
  }
}

void shortest_paths(vector<vector<int> > &adj,
  vector<vector<int> > &cost,
  int s,
  vector<long long> &dist,
  vector<int> &reachable,
  vector<int> &shortest) {
  
  std::vector<int> prev(adj.size(), INF);  
  std::queue<int> onnegcycle; 

  bellmanford(adj, cost, dist, reachable, true, prev, onnegcycle, s);
  bfs(adj, onnegcycle, shortest);
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
  vector<long long> distance(n, INFL);
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
    6 7
    1 2 10
    2 3 5
    1 3 100
    3 5 7
    5 4 10
    4 3 -18
    6 1 -1
    1
  )", R"(0
10
-
-
-
*
)");
  test(R"(5 4
  1 2 1
  4 1 2
  2 3 2
  3 1 -5
  4)",R"(-
-
-
0
*
)");
}

#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
