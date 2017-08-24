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
  long long x1x2 = std::get<0>(b)-std::get<0>(a);
  long long y1y2 = std::get<1>(b)-std::get<1>(a);
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
   return (distance == std::numeric_limits<double>().max())?(0):(distance);
}

//To understand why we only need to go 8 more points in this list
//see the algo_proff.odg on the same folder as this file.
double minimal_distance_split(vector<int>& order, vector<int>& xs, vector<int>& ys, int depth)
{
  double distance = std::numeric_limits<double>::max();
  for(int i = 0; i < order.size();++i)
  {
    for(int j = i+1; j < std::min(i+8,(int)order.size());++j)
    {
        auto d = dist(std::make_tuple(xs[order[i]],ys[order[i]]),std::make_tuple(xs[order[j]],ys[order[j]]));
        // tab(depth) << "[" << i << "," << j << "] = <("<< xs[order[i]] <<"," << ys[order[i]] << "),(" << xs[order[j]] << "," << ys[order[j]] << ")> = " << d << std::endl;
        if(distance > d)
        {
          distance = d;
        }
    }
  }
  return distance;
}

double minimal_distance_internal(vector<int>& orderx, vector<int>& ordery, vector<int>& subordery, vector<int>& xs, vector<int>& ys, int l, int r, int depth)
{
  if(l >= r - 1)
  {
      // tab(depth) << "[" << l << "," << r << "] (" << xs[orderx[l]] << "," << ys[orderx[l]] << ") = ignoring" << std::endl;
      return std::numeric_limits<double>::max();
  }
  else if(l >= r - 2)
  {
    auto result = dist(std::make_tuple(xs[orderx[l]],ys[orderx[l]]),std::make_tuple(xs[orderx[r-1]],ys[orderx[r-1]]));
    // tab(depth) << "[" << l << "," << r << "] = <("<< xs[orderx[l]] <<"," << ys[orderx[l]] << "),(" << xs[orderx[r-1]] << "," << ys[orderx[r-1]] << ")> = " << result << std::endl;
    return result;
  }
  else
  {
    auto pivot = l + ((r-l)/2);
    // tab(depth) << "[" << l << "," << pivot << "," << r << "]" << std::endl;

    auto lnewordery = std::vector<int>(pivot-l);
    for(int i = 0,index = l; index < pivot; ++index,++i){lnewordery[i] = orderx[index];}
    std::sort(lnewordery.begin(),lnewordery.end(),[&](const int& ll, const int & rr){return ys[ll] < ys[rr];});
    auto rnewordery = std::vector<int>(r-pivot);
    for(int i = 0,index = pivot; index < r; ++index,++i){rnewordery[i] = orderx[index];}
    std::sort(rnewordery.begin(),rnewordery.end(),[&](const int& ll, const int & rr){return ys[ll] < ys[rr];});
    
    auto minl = minimal_distance_internal(orderx,ordery,lnewordery,xs,ys,l,pivot, depth+1);
    auto minr = minimal_distance_internal(orderx,ordery,rnewordery,xs,ys,pivot,r, depth+1);
    auto best = std::min(minl,minr);

    auto xpivot = xs[orderx[pivot-1]];
    auto stripleft = xpivot - best;
    auto stripright = xpivot + best;
    auto relevant = std::vector<int>();
    for(auto yi : subordery)
    {
        auto x = xs[yi];
        if((x >= stripleft)&&(x <= stripright)){relevant.push_back(yi);}
    }

    // tab(depth) << "testing split" << std::endl;
    auto between = minimal_distance_split(relevant,xs,ys, depth);
    
    // tab(depth) << "comparing: " << minl << " " << minr << " " << between << std::endl;
    return std::min(between, best);
  }  
}

double minimal_distance(vector<int>& x, vector<int>& y) {
  auto orderx = std::vector<int>(x.size());
  std::iota(orderx.begin(), orderx.end(), 0);
  std::sort(orderx.begin(), orderx.end(), [&](const int& l, const int& r){return x[l] < x[r];});
  auto ordery = std::vector<int>(x.size());
  std::iota(ordery.begin(), ordery.end(), 0);
  std::sort(ordery.begin(), ordery.end(), [&](const int& l, const int& r){return y[l] < y[r];});
  auto r = minimal_distance_internal(orderx, ordery, ordery, x,y,0,x.size(), 0);
  return (r == std::numeric_limits<double>().max())?(0):(r);
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
  compare({1,2,3},{1,2,3});
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

// TEST_CASE("minimal_distance random cases", "[minimal_distance]")
// {
//   for(int i = 0; i <= 100000; ++i){
//     printProg(i, 100000);
//     auto xs = std::vector<int>(std::rand() % 1000 + 2);
//     std::generate(xs.begin(),xs.end(), []() -> int {return ((std::rand() % 1000) - 500);});
//     auto ys = std::vector<int>(xs.size());
//     std::generate(ys.begin(),ys.end(), []() -> int {return ((std::rand() % 1000) - 500);});
//     // printVector(xs);
//     // printVector(ys);
//     compare(xs,ys);
//   }
// }

TEST_CASE("minimal_distance corner cases", "[minimal_distance]")
{
  compare({0},{0});
  compare({-1000000000,1000000000},{-1000000000,1000000000});
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