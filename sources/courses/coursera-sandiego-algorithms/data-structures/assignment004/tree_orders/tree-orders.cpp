#include <iostream>
#include <vector>
#include <algorithm>
#include <stack>
#include <functional>

using std::vector;

class TreeOrders {
  int n;
  vector <int> key;
  vector <int> left;
  vector <int> right;

public:
  void read(std::istream& in) {
    in >> n;
    key.resize(n);
    left.resize(n);
    right.resize(n);
    for (int i = 0; i < n; i++) {
      in >> key[i] >> left[i] >> right[i];
    }
  }

  vector <int> in_order() {
    std::function<void(std::vector<int>&, int)> recursive_in_order;
    recursive_in_order = [&](std::vector<int>& result, int current) {
      auto l = left[current];
      auto r = right[current];
  
      if(l != -1) recursive_in_order(result, l);
  
      result.push_back(key[current]);
  
      if(r != -1) recursive_in_order(result, r);
    };

    std::vector<int> result;
    recursive_in_order(result, 0);
    return result;    
  }

  vector <int> pre_order() {
    std::function<void(std::vector<int>&, int)> recursive_pre_order;
    recursive_pre_order = [&](std::vector<int>& result, int current) {
      auto l = left[current];
      auto r = right[current];
  
      result.push_back(key[current]);
      if(l != -1) recursive_pre_order(result, l);
      if(r != -1) recursive_pre_order(result, r);
    };

    std::vector<int> result;
    recursive_pre_order(result, 0);
    return result; 
  }

  vector <int> post_order() {
    std::function<void(std::vector<int>&, int)> recursive_post_order;
    recursive_post_order = [&](std::vector<int>& result, int current) {
      auto l = left[current];
      auto r = right[current];
      
      if(l != -1) recursive_post_order(result, l);
      if(r != -1) recursive_post_order(result, r);
      result.push_back(key[current]);
    };

    std::vector<int> result;
    recursive_post_order(result, 0);
    return result; 
  }
};

void print(std::ostream& out, vector <int> a) {
  for (size_t i = 0; i < a.size(); i++) {
    if (i > 0) {
      out << ' ';
    }
    out << a[i];
  }
  out << '\n';
}

void run(std::istream& in, std::ostream& out)
{
  TreeOrders t;
  t.read(in);
  print(out, t.in_order());
  print(out, t.pre_order());
  print(out, t.post_order());
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& instr, const std::string& expectedOutStr)
{
  auto instream = std::stringstream{instr};
  auto expectedstream = std::stringstream{expectedOutStr};

  auto outstream = std::stringstream{};

  run(instream, outstream);
  outstream.seekg(0);

  // std::cout << outstream.str() << std::endl;
  // outstream.seekg(0);

  std::string actual, expected;
  while(!expectedstream.eof())
  {
    expectedstream >> expected;
    outstream >> actual;

    REQUIRE(expected == actual);    
  }
}

TEST_CASE("","")
{
  test(R"(5
  4 1 2
  2 3 4
  5 -1 -1
  1 -1 -1
  3 -1 -1)",R"(1 2 3 4 5
  4 2 1 3 5
  1 3 2 5 4)");
  test(R"(10
    0 7 2
    10 -1 -1
    20 -1 6
    30 8 9
    40 3 -1
    50 -1 -1
    60 1 -1
    70 5 4
    80 -1 -1
    90 -1 -1)",R"(50 70 80 30 90 40 0 20 10 60
      0 70 50 40 30 80 90 20 60 10
      50 80 90 30 40 70 10 60 20 0)");
}

#else

int main (int argc, char **argv)
{
  run(std::cin, std::cout);
  return 0;
}

#endif
