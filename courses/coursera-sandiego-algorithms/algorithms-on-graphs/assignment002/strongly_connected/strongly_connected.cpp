#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_set>
#include <vector>
#include <functional>

using std::vector;
using std::pair;

std::ostream& operator << (std::ostream& out, std::vector<int> v)
{
  out << "[";
  for(auto x : v)
  {
    out << x << ",";
  }
  return out << "]";
}

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

  Graph(vector<vector<int> > adjList, bool reverse) : adj(adjList), r(reverse)
  {
    if(reverse == true)
    {
      adj = vector<vector<int>>(adjList.size());
      for(int source = 0; source < adjList.size(); ++source)
      {
        for(auto target : adjList[source])
        {
          adj[target].push_back(source);
        }
      }
    }
  }

  Graph getReverse() const
  {
    return {adj, true};
  }

  std::vector<int> reachable_from(int v, std::unordered_set<int>& visited) const
  {
    // std::cout << "reachable_from" << v << std::endl;

    auto result = std::vector<int>();

    // auto visited = std::unordered_set<int>();
    std::function<void(const int&)> visit;
    visit = [&](const int& vi){
      // std::cout << "    visit " << vi << std::endl;
      
      if (visited.find(vi) != visited.end())
      {
        return;
      }

      visited.insert(vi);
      result.push_back(vi);

      auto vadj = adj[vi];
      // std::cout << "    adjacents " << vadj << std::endl;
      for(auto other_vertex : vadj)
      {
        visit(other_vertex);
      }
    };

    visit(v);
    return result;
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

class StronglyConnectedComponents
{
public:
  std::vector< std::vector<int> > calculateComponents(vector<vector<int> > &adj)
  {
    auto g = Graph{adj};
    auto gr = g.getReverse();

    auto components = std::vector< std::vector<int> >();

    auto prepos = gr.DFS();   

    auto greater = make_greater<VertexPrePos,int>([](const VertexPrePos& v) -> int {return v.Pos;});
    std::sort(prepos.begin(), prepos.end(), greater);
    auto visited = std::unordered_set<int>();

    for(auto vprepos : prepos)
    {
      auto x = vprepos.Vertex;
      if (visited.find(x) != visited.end()) continue;

      auto reachable = g.reachable_from(x, visited);
      for(auto r : reachable) visited.insert(r);

      components.push_back(reachable);
    }
    
    return components;
  }
};


int number_of_strongly_connected_components(vector<vector<int> > adj) {
  auto scc = StronglyConnectedComponents();
  auto components = scc.calculateComponents(adj);
  return components.size();
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
  out << number_of_strongly_connected_components(adj);
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

#else

int main() {
  run(std::cin, std::cout);
  return 0;
}

#endif
