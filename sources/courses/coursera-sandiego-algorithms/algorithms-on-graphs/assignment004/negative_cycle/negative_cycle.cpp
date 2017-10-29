#include <iostream>
#include <vector>
#include <limits>

using std::vector;

const int INF = std::numeric_limits<int>::max();
const long long INFL = std::numeric_limits<long long>::max();

//could be optimized taking into consideration that an iteration have
//not changed anything.
void bellmanford(vector<vector<int> > &adj,
    vector<vector<int> > &cost,
    std::vector<long long> &dist,
    std::vector<int> &prev,
    int source) {
      
    dist[source] = 0;
    int V = adj.size();
    for(int i = 0;i <= (V-1);++i)
    {
      // for(auto x : dist) std::cout << x << ",";
      // std::cout << std::endl;
  
      // std::cout << "iteration " << i << std::endl;
      for(int u = 0;u < adj.size(); ++u)
      {
        // std::cout << "at " << u + 1 << " current cost " << dist[u] << std::endl;
        if(dist[u] == INFL) continue;
        auto edges = adj[u];
        for(int vi = 0;vi < edges.size();++vi)
        {
          int v = edges[vi];
          int wuv = cost[u][vi];
          // std::cout << "    analysing " << v + 1 << " cost " << wuv << std::endl;  
          if(dist[v] > dist[u] + wuv)
          {
            // std::cout << "      updating dist " << v + 1 << std::endl;
            // std::cout << "      before " << dist[v] << std::endl;
            // std::cout << "      after " << dist[u] + wuv << std::endl;
            dist[v] = dist[u] + wuv;
            prev[v] = u;
          }
        }
      }
    }
}

int negative_cycle(vector<vector<int> > &adj, vector<vector<int> > &cost) {  
  std::vector<long long> dist(adj.size(), INFL);
  std::vector<int> prev(adj.size(), INF);

  //search negative cycles
  //even on disconnected graphs
  for(int u = 0;u < adj.size(); ++u)
  {
    if(dist[u] == INFL)
    {
      bellmanford(adj, cost, dist, prev, u);
    }
    auto edges = adj[u];
    for(int vi = 0;vi < edges.size();++vi)
    {
      int v = edges[vi];
      int wuv = cost[u][vi];
      if(dist[v] > dist[u] + wuv) return 1;
    }
  }

  return 0;
}

void run(std::istream& in, std::ostream& out)
{
  int n, m;
  in >> n >> m;
  vector<vector<int> > adj(n, vector<int>());
  vector<vector<int> > cost(n, vector<int>());
  for (int i = 0; i < m; i++) {
    int x, y, w;
    in >> x >> y >> w;
    adj[x - 1].push_back(y - 1);
    cost[x - 1].push_back(w);
  }
  out << negative_cycle(adj, cost);
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
    1 2 -5
    4 1 2
    2 3 2
    3 1 1)", "1");
  test(R"(
      4 0
      )", "0");
  test(R"(
        1 0
        )", "0");
}

#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif