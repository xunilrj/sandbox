#include <iostream>
#include <array>

int get_change(int m) {
  int count = 0;
  auto coins = std::array<int,3>{10,5,1};
  for(auto coin : coins)
  {
    auto q = m / coin;
    m = m % coin;
    count += q;
  }
  return count;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("get_change must work", "[change]")
{
    REQUIRE(get_change(1) == 1);
    REQUIRE(get_change(2) == 2);
    REQUIRE(get_change(3) == 3);
    REQUIRE(get_change(4) == 4);

    REQUIRE(get_change(5) == 1);
    REQUIRE(get_change(6) == 2);

    REQUIRE(get_change(9) == 5);
    REQUIRE(get_change(10) == 1);
    REQUIRE(get_change(11) == 2);

    REQUIRE(get_change(15) == 2);
    REQUIRE(get_change(16) == 3);
}

TEST_CASE("get_change corner case", "[change]")
{
    REQUIRE(get_change(1000) == 100);
    REQUIRE(get_change(999) == 99+1+4);
}

#else

int main() {
  int m;
  std::cin >> m;
  std::cout << get_change(m) << '\n';
}

#endif