#include <iostream>
#include <cassert>
#include <string>
#include <sstream>
#include <vector>
#include <stack>
#include <algorithm>
#include <functional>

using std::vector;
using std::string;
using std::max;
using std::min;

template<typename T>
void permutations(vector<T>& set, std::function<void(std::vector<T>)> f) {
   do {
        f(set);
    } while(std::next_permutation(set.begin(), set.end()));
}

long long eval(long long a, long long b, char op) {
  if (op == '*') {
    return a * b;
  } else if (op == '+') {
    return a + b;
  } else if (op == '-') {
    return a - b;
  } else {
    assert(0);
  }
}

struct node{
  bool digit;
  union{
    long long number;
    char op;
  };
};

node digit(long long d)
{
  auto n = node();
  n.digit = true;
  n.number = d;
  return n;
}

node digit(char d)
{
  auto n = node();
  n.digit = true;
  n.number = (long long)(d-48);
  return n;
}

node op(char op)
{
  auto n = node();
  n.digit = false;
  n.op = op;
  return n;
}

node make_node(char c)
{
  if((c == '+')||(c == '-')||(c == '*'))
  {
    return op(c);
  }
  else
  {
    return digit(c);
  }
}

#include <memory>
template <template <typename, typename> class Container, 
          typename Allocator=std::allocator<node> >
std::ostream & operator << (std::ostream & o, Container<node,Allocator> nodes)
{
  o << "[";

  for(auto current : nodes)
  {
    if(current.digit){
      o << "(" << current.number << "),";
    }else{
      o << "(" << current.op << ")";
    }
  }

  return o << "]";
}

std::ostream & operator << (std::ostream & o, std::vector<int> order)
{
  o << "[";

  for(auto current : order)
  {
    o << current << ",";
  }

  return o << "]";
}

long long evaluate(std::vector<char> problem, std::vector<int> order)
{
  auto stack = std::deque<node>();

  bool first = true;
  std::for_each(order.begin(), order.end(), [&](const int& i)
  {
    auto operator_i = (i * 2) + 1;
    auto r = operator_i + 1;
    
    if(first){
      auto l = operator_i - 1;
      stack.push_back(make_node(problem[l]));
      first = false;
    }
    stack.push_back(make_node(problem[r]));
    stack.push_back(make_node(problem[operator_i]));
  });

  std::cout << stack << std::endl;

  auto ops = std::vector<node>();
  while(!stack.empty())
  {
    auto current = stack.front();
    stack.pop_front();
    if(!current.digit)
    {
      // std::cout << ops[0].number << current.op << ops[1].number << std::endl;
      auto result = eval(ops[0].number, ops[1].number, current.op);
      stack.push_front(digit(result));
      ops.clear();
      std::cout << stack << std::endl;
    }
    else
    {
      ops.push_back(current);
    }
  }

  return ops[0].number;
}

long long get_maximum_value_naive(string exp) {
  std::stringstream ss(exp);
  auto operators = std::vector<char>();
  for(char c; ss >> c;)
  {
    operators.push_back(c);
  }

  auto order = std::vector<int>(operators.size()/2);
  std::iota(order.begin(), order.end(), 0);

  long long best = std::numeric_limits<int>::min();
  permutations<int>(order, [&](std::vector<int> x){
    std::cout << x << std::endl;
    auto r = evaluate(operators, x);
    if(r > best) best = r;
  });

  return best;
}


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("get_maximum_value must work","get_maximum_value")
{
  REQUIRE(get_maximum_value_naive("1 + 5") == 6);
  REQUIRE(get_maximum_value_naive("5 - 8 + 7 * 4 - 8 + 9") == 200);
}

#else

int main() {
  string s;
  std::cin >> s;
  std::cout << get_maximum_value(s) << '\n';
}

#endif