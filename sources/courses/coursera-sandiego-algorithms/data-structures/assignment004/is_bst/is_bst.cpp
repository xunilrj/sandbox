#include <algorithm>
#include <iostream>
#include <vector>
#include <tuple>

using std::endl;
using std::vector;

struct Node {
  int key;
  int left;
  int right;

  Node() : key(0), left(-1), right(-1) {}
  Node(int key_, int left_, int right_) : key(key_), left(left_), right(right_) {}
};

bool IsNodeOk(const vector<Node>& tree, int current, std::tuple<int,int> boundary){
  auto l = tree[current].left;
  auto r = tree[current].right;
  auto currentValue = tree[current].key;

  // std::cout << current << " " << currentValue << " " << std::get<0>(boundary) << " " << std::get<1>(boundary) << std::endl;

  if(currentValue <= std::get<0>(boundary)) return false;
  if(currentValue >= std::get<1>(boundary)) return false;

  if(l >= 0)
  {
    auto lValue = tree[l].key;
    if(lValue >= currentValue) return false;    
    auto ok = IsNodeOk(tree, l, std::make_tuple(std::get<0>(boundary),currentValue));
    if(ok == false) return false;
  }

  if(r >= 0)
  {
    auto rValue = tree[r].key;
    if(rValue <= currentValue) return false;
    auto ok  = IsNodeOk(tree, r, std::make_tuple(currentValue,std::get<1>(boundary)));
    if(ok == false) return false;
  }

  return true;
}

bool IsBinarySearchTree(const vector<Node>& tree) {
  if(tree.size() == 0) return true;
  return IsNodeOk(tree, 0, std::make_tuple(std::numeric_limits<int>::min(),std::numeric_limits<int>::max()));
}

void run(std::istream& in, std::ostream& out)
{
  int nodes;
  in >> nodes;
  vector<Node> tree;
  for (int i = 0; i < nodes; ++i) {
    int key, left, right;
    in >> key >> left >> right;
    tree.push_back(Node(key, left, right));
  }
  if (IsBinarySearchTree(tree)) {
    out << "CORRECT" << endl;
  } else {
    out << "INCORRECT" << endl;
  }
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
  // test(R"(3
  //   2 1 2
  //   1 -1 -1
  //   3 -1 -1)", R"(CORRECT)");
  // test("0","CORRECT");
  test(R"(4
    4 1 -1
    2 2 3
    1 -1 -1
    5 -1 -1)","INCORRECT");
}

#else

int main() 
{
  run(std::cin, std::cout);
  return 0;
}

#endif
