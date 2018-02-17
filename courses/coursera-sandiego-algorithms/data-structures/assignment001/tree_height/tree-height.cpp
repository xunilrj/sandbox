#include <algorithm>
#include <iostream>
#include <vector>
#include <stack>

class Node 
{
public:
    friend std::ostream& operator<<(std::ostream &, const Node &);
  
    Node() : parent(nullptr)
    {
    }

    void setKey(int x)
    {
      key = x;
    }

    void setParent(Node *theParent) 
    {
      parent = theParent;
      parent->children.push_back(this);
    }

    template <typename F>
    void forEachChildren(F f)
    {
      for(auto c : children)
      {
        f(c);
      }
    }

  private:
    int key;
    Node *parent;
    std::vector<Node *> children;
};

std::ostream& operator<<(std::ostream &os, const Node &n)
{
    return os << n.key;
}

void printTree(Node * root, int depth)
{
  // std::cout << std::string(depth*2, ' ') << *root << std::endl;
  root->forEachChildren([=](Node * c){
    printTree(c, depth+1);
  });
}

struct NodeDepth
{
  Node * node;
  int depth;

  NodeDepth(Node * node, int depth) : node(node), depth(depth)
  {
  }
};

int getHeight(Node * root)
{
  int maxh = std::numeric_limits<int>::min();;
  auto stack = std::stack<NodeDepth>();
  stack.emplace(root, 1);

  while(stack.size() > 0)
  {
    auto current = stack.top();
    stack.pop();

    if(current.depth > maxh) maxh = current.depth;

    current.node->forEachChildren([&](Node * c){
      stack.emplace(c, current.depth+1);      
    });
  }

  return maxh;
}

Node * buildTree(std::istream &in)
{
  int count = 0;
  in >> count;

  // std::cout << "building " << count << " nodes." << std::endl;;
  
  Node * root = nullptr;
  auto nodes = std::vector<Node*>(count);
  for(auto i = 0; i < count; ++i) nodes[i] = new Node();

  for(auto i = 0; i < count; ++i)
  {
    auto currentParent = 0;
    in >> currentParent;

    if(currentParent == -1)
    {
      // std::cout << "root found!" << std::endl;
      root = nodes[i];
      root->setKey(i);
    }
    else
    {
      auto parent = nodes[currentParent];
      nodes[i]->setParent(parent);
      nodes[i]->setKey(i);
    }
  }

  // printTree(root, 0);

  return root;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string &input, int expectedHeight)
{
  auto str = std::stringstream{input, std::ios_base::in | std::ios_base::out};
  auto root = buildTree(str);
  auto height = getHeight(root);
  REQUIRE(height == expectedHeight);
}

TEST_CASE("getHeight must work","getHeight")
{
  test("5 4 -1 4 1 1", 3);
  test("5 -1 0 4 0 3", 4);
}

TEST_CASE("getHeight maximum depth","getHeight")
{
  int qtd = 100000;
  auto tree = std::vector<int>(qtd);
  auto start = 0;
  auto begin = tree.begin();
  ++begin;
  std::generate(begin, tree.end(), [&](){return start++;});
  tree[0] = -1;
  auto ss = std::stringstream();
  ss << qtd;
  for(auto x : tree)
  {
    ss << x << " ";
  }
  auto str = ss.str();
  test(str, qtd);
}

#else

int main (int argc, char **argv)
{
  auto root = buildTree(std::cin);
  auto height = getHeight(root);
  std::cout << height;
  return 0;
}

#endif