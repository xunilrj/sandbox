#include <iostream>

int gcd_naive(int a, int b) {
  int current_gcd = 1;
  for (int d = 2; d <= a && d <= b; d++) {
    if (a % d == 0 && b % d == 0) {
      if (d > current_gcd) {
        current_gcd = d;
      }
    }
  }
  return current_gcd;
}

int gcd(int a, int b)
{
  if(b == 0) return a;
  if(b > a) return gcd(b,a);
  return gcd(b, a % b);
}


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("GCD must work", "[gcd]")
{
    REQUIRE(gcd(2,1) == 1);
    REQUIRE(gcd(18,35) == 1);
    REQUIRE(gcd(1344,217) == 7);
    REQUIRE(gcd(28851538, 1183019) == 17657);
}

TEST_CASE("GCD corner cases", "[gcd]")
{
    REQUIRE(gcd(1,1) == 1);
    REQUIRE(gcd(2*1000000000, 1) == 1);
    REQUIRE(gcd(std::numeric_limits<int>::max(), 1) == 1);
}

#else

int main() {
  int a, b;
  std::cin >> a >> b;
  std::cout << gcd(a, b) << std::endl;
  return 0;
}

#endif