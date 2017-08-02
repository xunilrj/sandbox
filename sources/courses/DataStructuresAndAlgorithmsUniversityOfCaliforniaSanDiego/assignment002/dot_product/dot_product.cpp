#include <algorithm>
#include <iostream>
#include <vector>

using std::vector;

int sortDescending(const int & l, const int & r){
    return l > r;
}

long long max_dot_product(vector<int> a, vector<int> b) {
  std::sort(a.begin(),a.end(),sortDescending);
  std::sort(b.begin(),b.end(),sortDescending);

  // write your code here
  long long result = 0;
  for (size_t i = 0; i < a.size(); i++) {
    result += ((long long) a[i]) * b[i];
  }

  return result;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("dot_product must work", "[dot_product]")
{
  REQUIRE(max_dot_product({23},{39}) == 897);
  REQUIRE(max_dot_product({1,3,-5},{-2,4,1}) == 23);
}

TEST_CASE("dot_product corner case", "[dot_product]")
{
  REQUIRE(max_dot_product({-100000,100000},{-100000,100000}) == 20000000000);

  auto ads = std::vector<int>(1000);
  REQUIRE(max_dot_product(ads,ads) == 0);
}

#else

int main() {
  size_t n;
  std::cin >> n;
  vector<int> a(n), b(n);
  for (size_t i = 0; i < n; i++) {
    std::cin >> a[i];
  }
  for (size_t i = 0; i < n; i++) {
    std::cin >> b[i];
  }
  std::cout << max_dot_product(a, b) << std::endl;
}

#endif