#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_set>
#include <vector>
#include <functional>

using std::vector;
using std::pair;

template <typename T, typename TValue>
class map_greater
{
public:
  map_greater(std::function<TValue(const T&)> f) : getValue(f)
  {
  }

  bool operator () (const T& l, const T& r) const
  {
    auto lvalue = getValue(l);
    auto rvalue = getValue(r);
    return lvalue > rvalue;
  }
private:
  std::function<TValue(const T&)> getValue;
};

struct VertexPrePos
{
  int Vertex;
  int Pre;
  int Pos;
};

template <typename T, typename TValue>
map_greater<T,TValue> make_greater(std::function<TValue(const T&)> f)
{
  return {f};
}

class Graph
{
public:
  Graph(vector<vector<int> > adjList) : adj(adjList), r(false)
  {
  }

  std::vector< VertexPrePos > DFS() const
  {
    int pos = 0;
    auto prepos = std::vector<VertexPrePos>(adj.size()); 

    auto visited = std::unordered_set<int>();
    std::function<void(const int&)> visit;
    visit = [&](const int& vi){
      if (visited.find(vi) != visited.end())
      {
        return;
      }

      visited.insert(vi);
      prepos[vi] = {vi, pos, 0};
      ++pos;

      auto vadj = adj[vi];
      for(auto other_vertex : vadj)
      {
        visit(other_vertex);
      }
      
      prepos[vi].Pos = pos;
      ++pos;
    };
    
    for(int i = 0; i < adj.size();++i) visit(i);
    return prepos;
  }
private:
  vector<vector<int> > adj;
  bool r;
};

vector<int> toposort(vector<vector<int> > adj) {
  vector<int> used(adj.size(), 0);
  vector<int> order;
  
  auto g = Graph{adj};
  auto prepos = g.DFS();   
  auto greater = make_greater<VertexPrePos,int>([](const VertexPrePos& v) -> int {return v.Pos;});
  std::sort(prepos.begin(), prepos.end(), greater);

  for(auto p : prepos)
  {
    order.push_back(p.Vertex);
  }

  return order;
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
  }
  vector<int> order = toposort(adj);
  for (size_t i = 0; i < order.size(); i++) {
    out << order[i] + 1 << " ";
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
  //     3 ---\
  //          |
  //          v
  //      2-->0
  //          |
  //          |
  //      1<--/
  test(R"(4 3
    1 2
    4 1
    3 1)", "4 3 1 2 ");
  test(R"(4 1
    3 1)", "2 3 1 4 ");
}

#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
