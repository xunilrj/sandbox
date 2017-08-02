#include <algorithm>
#include <sstream>
#include <iostream>
#include <vector>
#include <string>

using std::vector;
using std::string;

string largest_number(vector<string> a) {
  std::sort(a.begin(), a.end(), [](const string & l, const string & r){
      long long lint;
      long long riint;
      std::stringstream lstr;
      lstr << l << r;
      lstr >> lint;
      lstr.clear();
      lstr << r << l;
      lstr >> riint;

      return lint > riint;
  });
  //write your code here
  std::stringstream ret;
  for (size_t i = 0; i < a.size(); i++) {
    ret << a[i];
  }
  string result;
  ret >> result;
  return result;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("largest_number must work", "[largest_number]")
{
  REQUIRE(largest_number({"21","2"}) == "221");
  REQUIRE(largest_number({"9","4","6","1","9"}) == "99641");
  REQUIRE(largest_number({"23","39","92"}) == "923923");
}

TEST_CASE("largest_number corner cases", "[largest_number]")
{
  REQUIRE(largest_number({"1000","1"}) == "11000");
  
  auto numbers = std::vector<string>(100);
  for(int i = 0; i < numbers.size(); ++i)
  {
    numbers[i] = "1";
  }
  auto expected = std::string(100,'1');
  REQUIRE(largest_number(numbers) == expected);
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<string> a(n);
  for (size_t i = 0; i < a.size(); i++) {
    std::cin >> a[i];
  }
  std::cout << largest_number(a);
}

#endif