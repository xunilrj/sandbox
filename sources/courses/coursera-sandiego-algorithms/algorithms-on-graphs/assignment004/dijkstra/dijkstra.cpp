#include <iostream>
#include <vector>
#include <queue>
#include <limits>
#include <set>

using std::vector;
using std::queue;
using std::pair;
using std::priority_queue;

const int INF = std::numeric_limits<int>::max();
const long long INFL = std::numeric_limits<long long>::max();
long long distance(vector<vector<int> > &adj, vector<vector<int> > &cost, int s, int t) {
  std::vector<long long> dist(adj.size(), INFL);
  std::vector<int> prev(adj.size(), INF);

  dist[s] = 0;
  
  auto cmp = [&](const int& l, const int& r) {
      return dist[l] < dist[r];
  };
  auto H = std::multiset<int, decltype(cmp)>(cmp);
  //for(int i = 0; i < adj.size(); ++i) H.insert(i);
  H.insert(s);
  
  while(H.size() > 0)
  {
    auto hbegin = H.begin();
    auto u = *hbegin;
    H.erase(hbegin);

    // std::cout << "at " << u + 1 << std::endl;

    auto uedges = adj[u];
    for(int vi = 0; vi < uedges.size(); ++vi)    
    {
      auto v = uedges[vi];
      auto wuv = cost[u][vi];
      // std::cout << "    analysing " << v + 1 << " cost " << wuv << std::endl;      
      if(dist[v] > dist[u] + wuv)
      {
        // std::cout << "      updating dist " << v + 1 << std::endl;
        // std::cout << "      before " << dist[v] << std::endl;
        // std::cout << "      after " << dist[u] + wuv << std::endl;

        //H.erase(v); //?

        dist[v] = dist[u] + wuv;
        prev[v] = u;
        
        H.insert(v);        
      }      
    }
  }

  // for(auto x : dist) std::cout << x << ",";
  // std::cout << "found solution at " << dist[t] << std::endl;
  auto d = dist[t];  
  return d != INFL ? d : -1;
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
  int s, t;
  in >> s >> t;
  s--, t--;
  out << distance(adj, cost, s, t);
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
    1 2 1
    4 1 2
    2 3 2
    1 3 5
    1 3
  )", "3");
  test(R"(4 4
    1 2 1
    4 1 2
    2 3 2
    1 3 0
    1 3
  )", "0");
  test(R"(5 9
    1 2 4
    1 3 2
    2 3 2
    3 2 1
    2 4 2
    3 5 4
    5 4 1
    2 5 3
    3 4 4
    1 5)","6");
    test(R"(3 3
      1 2 7
      1 3 5
      2 3 2
      3 2)", "-1");
    test(R"(3 3
        1 2 7
        1 3 5
        2 3 2
        1 1)", "0");
    test(R"(1 0
          1 1)", "0");
}


#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
