#include <iostream>
#include <vector>

using std::vector;

vector<int> optimal_summands(int n) {
  vector<int> summands;

  int current = 1;
  while(n >= current)
  {
    // auto rest = n - current;
    // if(rest > 0 && rest < (current+1))
    if(n != current && n <= (2*current))
    {
        // std::cout << "NOK " << n << " " << current << std::endl;
        current++;
        continue;
    }
    else
    {
        // std::cout << "OK " << n << " " << current << std::endl;
        summands.push_back(current);
        n -= current;
        current++;
    }
  }
  return summands;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("dot_product must work", "[dot_product]")
{
  auto result = optimal_summands(6);
  REQUIRE(result.size() == 3);
  REQUIRE(result[0] == 1);
  REQUIRE(result[1] == 2);
  REQUIRE(result[2] == 3);

  result = optimal_summands(8);
  REQUIRE(result.size() == 3);
  REQUIRE(result[0] == 1);
  REQUIRE(result[1] == 2);
  REQUIRE(result[2] == 5);

  result = optimal_summands(2);
  REQUIRE(result.size() == 1);
  REQUIRE(result[0] == 2);
}

TEST_CASE("dot_product corner case", "[dot_product]")
{
  auto result = optimal_summands(1000000000);
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<int> summands = optimal_summands(n);
  std::cout << summands.size() << '\n';
  for (size_t i = 0; i < summands.size(); ++i) {
    std::cout << summands[i] << ' ';
  }
}

#endif