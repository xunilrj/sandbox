#include <algorithm>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <string>
#include <cmath>

using std::vector;
using std::string;
using std::pair;
using std::min;

std::ostream & tab(int size)
{
    return (std::cout << std::string(size * 3, ' '));
}

 std::ostream & operator<<(std::ostream & o, const std::tuple<int,int> & m)
  {
      o << "(" << std::get<0>(m) << "," << std::get<1>(m) << ")";
      return o;
  }

double dist(std::tuple<int,int> a, std::tuple<int,int> b)
{
  auto x1x2 = std::get<0>(b)-std::get<0>(a);
  auto y1y2 = std::get<1>(b)-std::get<1>(a);
  return std::sqrt((x1x2*x1x2) + (y1y2*y1y2));
}

double naive_minimal_distance(vector<int> xs, vector<int> ys)
{
  double distance = std::numeric_limits<double>::max();
  for(int i = 0; i < xs.size();++i)
  {
    for(int j = 0; j < xs.size();++j)
    {
        if(i!=j){
          auto d = dist(std::make_tuple(xs[i],ys[i]),std::make_tuple(xs[j],ys[j]));
          if(distance > d)
          {
            distance = d;
          }
        }
    }
  }
  return distance;
}

double naive_minimal_distance(vector<int>& order, vector<int>& xs, vector<int>& ys, int lstart, int lend, int rstart, int rend)
{
  double distance = std::numeric_limits<double>::max();
  for(int i = lstart; i < lend;++i)
  {
    for(int j = rstart; j < rend;++j)
    {
        if(i!=j){
          auto d = dist(std::make_tuple(xs[i],ys[i]),std::make_tuple(xs[j],ys[j]));
          if(distance > d)
          {
            distance = d;
          }
        }
    }
  }
  return distance;
}

double minimal_distance_internal(vector<int>& order, vector<int>& xs, vector<int>& ys, int l, int r, int depth)
{
  if(l >= r - 1)
  {
      tab(depth) << "[" << l << "," << r << "] (" << xs[order[l]] << "," << ys[order[l]] << ") = ignoring" << std::endl;
      return std::numeric_limits<double>::max();
  }
  else if(l >= r - 2)
  {
    auto result = dist(std::make_tuple(xs[order[l]],ys[order[l]]),std::make_tuple(xs[order[r-1]],ys[order[r-1]]));
    tab(depth) << "[" << l << "," << r << "] = <("<< xs[order[l]] <<"," << ys[order[l]] << "),(" << xs[order[r-1]] << "," << ys[order[r-1]] << ")> = " << result << std::endl;
    return result;
  }
  else
  {
    auto pivot = l + ((r-l)/2);
    tab(depth) << "[" << l << "," << pivot << "," << r << "]" << std::endl;
    
    auto minl = minimal_distance_internal(order,xs,ys,l,pivot, depth+1);
    auto minr = minimal_distance_internal(order,xs,ys,pivot,r, depth+1);

    auto most_right_on_left = std::make_tuple(xs[order[pivot-1]],ys[order[pivot-1]]);
    auto most_left_on_right = std::make_tuple(xs[order[pivot]],ys[order[pivot]]);
    auto between = dist(most_right_on_left,most_left_on_right);

    tab(depth) << "calc " << most_right_on_left << " " << most_left_on_right << " = " << between << std::endl;
    tab(depth) << "comparing: " << minl << " " << minr << " " << between << std::endl;
    return std::min(between, std::min(minl, minr));
  }  
}

double minimal_distance(vector<int> x, vector<int> y) {
  auto order = std::vector<int>(x.size());
  std::iota(order.begin(), order.end(), 0);
  std::sort(order.begin(), order.end(), [&](const int& l, const int& r){
    return std::make_tuple(x[l],y[l]) < std::make_tuple(x[r],y[r]);
  });
  return minimal_distance_internal(order, x,y,0,x.size(), 0);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void compare(std::vector<int> xs, std::vector<int> ys)
{
  REQUIRE(naive_minimal_distance(xs,ys) == minimal_distance(xs,ys));
}

TEST_CASE("minimal_distance must work", "[minimal_distance]")
{
  compare({0,3},{0,4});
  compare({7,1,4,7},{7,100,8,7});
  compare({4,-2,-3,-1,2,-4,1,-1,3,-4,-2},{4,-2,-4,3,3,0,1,-1,-1,2,4});
}

void printProg(int x, int total){
    int progress = ((double)x / total) * 100;
    std::cout << "[";
    for (int i=1;i<=100;i++){
        if (i<progress || progress==100)
            std::cout << "=";
        else if (i==progress)
            std::cout << ">";
        else
            std::cout << " ";
    }

    std::cout << "] " << x << "/" << total << "\r" << std::flush;
}

void printVector(const std::vector<int> &a)
{
  std::cout << "[";
  for(auto i : a)
  {
    std::cout << i << ",";
  }
  std::cout << "]" << std::endl;
}

TEST_CASE("minimal_distance random cases", "[minimal_distance]")
{
  for(int i = 0; i < 100; ++i){
    printProg(i, 100);
    auto xs = std::vector<int>(std::rand() % 20);
    std::generate(xs.begin(),xs.end(), []() -> int {return ((std::rand() % 1000) - 500);});
    auto ys = std::vector<int>(xs.size());
    std::generate(ys.begin(),ys.end(), []() -> int {return ((std::rand() % 1000) - 500);});
    printVector(xs);
    printVector(ys);
    compare(xs,ys);
  }
}

#else

int main() {
  size_t n;
  std::cin >> n;
  vector<int> x(n);
  vector<int> y(n);
  for (size_t i = 0; i < n; i++) {
    std::cin >> x[i] >> y[i];
  }
  std::cout << std::fixed;
  std::cout << std::setprecision(9) << minimal_distance(x, y) << "\n";
}

#endif