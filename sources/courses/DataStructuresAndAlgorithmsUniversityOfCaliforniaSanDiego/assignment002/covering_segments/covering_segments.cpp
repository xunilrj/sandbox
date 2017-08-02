#include <algorithm>
#include <iostream>
#include <climits>
#include <vector>

using std::vector;

struct Segment {
  int start, end;
};

int sortAscending(const Segment & l, const Segment & r){
    return l.end < r.end;
}

vector<int> optimal_points(vector<Segment>& segments) {
  std::sort(segments.begin(), segments.end(), sortAscending);

  vector<int> points;
  //write your code here
  for (auto s : segments) {
    if(points.size() == 0)
    {
      points.push_back(s.end);
    }
    else
    {
      auto lastpoint = points[points.size()-1];
      if(lastpoint < s.start || lastpoint > s.end)
      {
        points.push_back(s.end);
      }
    }
  }
  return points;
}

vector<int> optimal_points_(vector<Segment> segments) {
  return optimal_points(segments);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("optimal_points must work", "[optimal_points]")
{
  auto result = optimal_points_({{1,3},{2,5},{3,6}});
  REQUIRE(result.size() == 1);
  REQUIRE(result[0] == 3);

  result = optimal_points_({{4,7},{1,3},{2,5},{5,6}});
  REQUIRE(result.size() == 2);
  REQUIRE(result[0] == 3);
  REQUIRE(result[1] == 6);
}

TEST_CASE("optimal_points corner case", "[optimal_points]")
{
  auto segments = std::vector<Segment>(100);
  auto result = optimal_points_(segments);
  REQUIRE(result.size() == 1);

  result = optimal_points_({{0,1000000000},{1000000000,1000000000}});
  REQUIRE(result.size() == 1);
  REQUIRE(result[0] == 1000000000);
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<Segment> segments(n);
  for (size_t i = 0; i < segments.size(); ++i) {
    std::cin >> segments[i].start >> segments[i].end;
  }
  vector<int> points = optimal_points(segments);
  std::cout << points.size() << "\n";
  for (size_t i = 0; i < points.size(); ++i) {
    std::cout << points[i] << " ";
  }
}

#endif