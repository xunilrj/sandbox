#include <algorithm>
#include <iostream>
#include <vector>

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
  std::cout << std::string(depth*2, ' ') << *root << std::endl;
  root->forEachChildren([=](auto c){
    printTree(c, depth+1);
  });
}

int getHeight(Node * root, int depth = 0)
{
  int maxh = std::numeric_limits<int>::min();

  root->forEachChildren([&](auto c){
    auto h = getHeight(c, depth+1);
    maxh = std::max(maxh,h);
  });

  return maxh;
}

Node * buildTree(std::istream &in)
{
  int count = 0;
  in >> count;

  std::cout << "building " << count << " nodes." << std::endl;;
  
  Node * root = nullptr;
  auto nodes = std::vector<Node*>(count);
  for(auto i = 0; i < count; ++i) nodes[i] = new Node();

  for(auto i = 0; i < count; ++i)
  {
    auto currentParent = 0;
    in >> currentParent;

    if(currentParent == -1)
    {
      std::cout << "root found!" << std::endl;
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

  printTree(root, 0);

  return root;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("a","a")
{
  auto str = std::stringstream{"5 4 -1 4 1 1", std::ios_base::in | std::ios_base::out};
  //auto str = std::stringstream{"5\n-1 0 4 0 3", std::ios_base::in | std::ios_base::out};
  auto root = buildTree(str);
  auto height = getHeight(root);
  REQUIRE(height == 2);
}

#else

int main (int argc, char **argv)
{
  return 0;
}

#endif