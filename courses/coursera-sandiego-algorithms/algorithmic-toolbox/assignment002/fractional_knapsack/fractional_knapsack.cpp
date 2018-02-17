#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using std::vector;

double unitValue(int w, int v)
{
  return (double)v/(double)w;
}

double get_optimal_value(int capacity, vector<int> weights, vector<int> values) {
  double value = 0.0;
  auto order = std::vector<int>(weights.size());
  std::iota(order.begin(), order.end(), 0);
  std::sort(order.begin(), order.end(), [&](const int& l, const int& r){
    return unitValue(weights[r],values[r]) < unitValue(weights[l],values[l]);
  });
  for(auto i: order)
  {
    if(capacity <= 0)
    {
      break;
    }
    auto w = weights[i];
    auto v = values[i];
    if(capacity >= w)
    {
      capacity -= w;
      value += v;
    }
    else
    {
      auto partial = (double)capacity / (double)w;
      value += v*partial;
      capacity = 0;
    }
  }
  return value;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("fractional_knapsack must work", "[knap]")
{
    REQUIRE(get_optimal_value(50, {20,50,30}, {60,100,120}) == 180.0);
    REQUIRE(get_optimal_value(10, {30}, {500}) == Approx(166.66666));
}

TEST_CASE("fractional_knapsack corner cases", "[knap]")
{
    REQUIRE(get_optimal_value(1000, {2*1000000}, {2*1000000}) == 1000);

    auto weigths = std::vector<int>(2*1000000);
    auto values = std::vector<int>(2*1000000);
    REQUIRE(get_optimal_value(1000, weigths, values) == 0);
}

#else

int main() {
  int n;
  int capacity;
  std::cin >> n >> capacity;
  vector<int> values(n);
  vector<int> weights(n);
  for (int i = 0; i < n; i++) {
    std::cin >> values[i] >> weights[i];
  }

  double optimal_value = get_optimal_value(capacity, weights, values);

  std::cout.precision(10);
  std::cout << optimal_value << std::endl;
  return 0;
}

#endif
