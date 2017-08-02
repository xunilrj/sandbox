#include <iostream>

long long lcm_naive(int a, int b)
{
  for (long l = 1; l <= (long long)a * b; ++l)
    if (l % a == 0 && l % b == 0)
      return l;

  return (long long)a * b;
}

int gcd(int a, int b)
{
  if(b == 0) return a;
  if(b > a) return gcd(b,a);
  return gcd(b, a % b);
}

long long lcm(int a, int b)
{
  //keep the biggest number as a
  //because it is the one divided
  //in the calculation.
  //avoiding overflows
  if(b > a) return lcm(b,a);
  long long lla = a;
  long long llb = b;
  long long llgcd = gcd(a,b);
  return (a/llgcd)*(b);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("GCD must work", "[gcd]")
{
  REQUIRE(lcm(6, 8) == 24);
  REQUIRE(lcm(28851538, 1183019) == 1933053046);
}

TEST_CASE("GCD corner cases", "[gcd]")
{
  REQUIRE(lcm(1, 1) == 1);
  REQUIRE(lcm(1, 2*1000000000) == 2*1000000000);
  REQUIRE(lcm(2*1000000000,1) == 2*1000000000);
  REQUIRE(lcm(2*1000000000,2*1000000000) == 2*1000000000);
}

#else

int main()
{
  int a, b;
  std::cin >> a >> b;
  std::cout << lcm(a, b) << std::endl;
  return 0;
}

#endif
