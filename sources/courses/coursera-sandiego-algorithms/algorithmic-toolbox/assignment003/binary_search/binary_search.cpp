#include <iostream>
#include <cassert>
#include <vector>
#include <functional>

using std::vector;

int binary_search(const vector<int> &a, int x) {
  std::function<int(int,const vector<int> &,int,int)> internal_search;
  internal_search = [&](int x, const vector<int> &a, int l, int r) -> int
  {
    if(r <= l)
    {
      return -1;
    }
    // std::cout << l << " " << r << std::endl;
    int pivot = l+((r-l)/2);
    if(a[pivot] == x)
    {
      return pivot;
    }
    else if (a[pivot] > x)
    {
      return internal_search(x, a, l, pivot);
    }
    else
    {
      return internal_search(x, a, pivot+1, r);
    }
  };
  return internal_search(x, a, 0, a.size());
}

int binary_search_(vector<int> a, int x){
  return binary_search(a,x);
}

int linear_search(const vector<int> &a, int x) {
  for (size_t i = 0; i < a.size(); ++i) {
    if (a[i] == x) return i;
  }
  return -1;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("binary_search must work", "[binary_search]")
{
  auto numbers = std::vector<int>{1,5,8,12,13};
  REQUIRE(binary_search_(numbers,8) == 2);
  REQUIRE(binary_search_(numbers,1) == 0);
  REQUIRE(binary_search_(numbers,23) == -1);
  REQUIRE(binary_search_(numbers,1) == 0);
  REQUIRE(binary_search_(numbers,11) == -1);
}

TEST_CASE("binary_search corner cases", "[binary_search]")
{
  auto numbers = std::vector<int>{1,1000000000};
  REQUIRE(binary_search_(numbers,1000000000) == 1);
  REQUIRE(binary_search_(numbers,1000000001) == -1);


  numbers = std::vector<int>(100000);
  REQUIRE(binary_search_(numbers,0) == 50000);
  REQUIRE(binary_search_(numbers,1) == -1);
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<int> a(n);
  for (size_t i = 0; i < a.size(); i++) {
    std::cin >> a[i];
  }
  int m;
  std::cin >> m;
  vector<int> b(m);
  for (int i = 0; i < m; ++i) {
    std::cin >> b[i];
  }
  for (int i = 0; i < m; ++i) {
    //replace with the call to binary_search when implemented
    std::cout << binary_search(a, b[i]) << ' ';
  }
}

#endif