#include <iostream>
#include <cassert>
#include <string>
#include <sstream>
#include <vector>
#include <stack>
#include <algorithm>
#include <functional>
#include <tuple>

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

template <template <typename, typename> class Container, 
          typename Value,
          typename Allocator=std::allocator<Value> >
std::ostream & operator << (std::ostream & o, Container<Value,Allocator> v)
{
  o << "[";

  for(auto current : v)
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

  // std::cout << stack << std::endl;

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
      // std::cout << stack << std::endl;
    }
    else
    {
      ops.push_back(current);
    }
  }

  return ops[0].number;
}

std::ostream & tab(int depth)
{
  return std::cout << std::string(depth, ' ');
}

std::tuple<int,int> get_maximum_value_naive_internal(std::vector<char> exp, int i, int j, int depth = 0)
{
  if(i > j)
  {
    tab(depth) << "ERROR!";
    return std::make_tuple(0,0);
  }
  else if (i+1 == j)
  {
    auto v = (long long)(exp[i*2]-48);
    // tab(depth) << "digit " << i << " " << v << std::endl;
    return std::make_tuple(v,v);
  }

  auto min = std::numeric_limits<long long>::max(),
    max = std::numeric_limits<long long>::min();
  for(int newi = i+1;newi < j;++newi)
  {
    auto op = exp[(2*newi)-1];
    tab(depth) << "[" << i << "," << newi << "] " << op << " [" << newi << "," << j << "]" << std::endl;

    int minl = 0, maxl = 0; 
    std::tie(minl,maxl) = get_maximum_value_naive_internal(exp, i, newi, depth + 1);

    int minr = 0, maxr = 0; 
    std::tie(minr,maxr) = get_maximum_value_naive_internal(exp, newi, j, depth + 1);
    
    auto a = eval(minl, minr, op);
    auto b = eval(minl, maxr, op);
    auto c = eval(maxl, minr, op);
    auto d = eval(maxl, maxr, op);
    auto cmin = 0, cmax = 0;
    std::tie(cmin,cmax) = std::minmax({a,b,c,d});

    tab(depth) << "options {" << std::vector<long long>{a,b,c,d} << "}" << std::endl;

    if(cmin < min) min = cmin;
    if(cmax > max) max = cmax;
  }

  return std::make_tuple(min,max);
}

long long get_maximum_value_naive(string exp) {
  std::stringstream ss(exp);
  auto operators = std::vector<char>();
  for(char c; ss >> c;)
  {
    operators.push_back(c);
  }

  // std::cout << operators << std::endl;

  auto digit_count = (operators.size() / 2) + 1;
  int min = 0, max = 0; 
  std::tie(min,max) = get_maximum_value_naive_internal(operators, 0, digit_count, 0);
  return max;
}

void print(long long *array, int w, int h, int cx, int cy){
  auto at = [&](int x,int y){return x+(y*(w));};
  for (int j = 0; j < h; j++){
    for (int i = 0; i < w; i++){      
        if(i == cx & j == cy ){
          std::cout << "[" << array[at(i,j)] << "] ";
        }
        else{
          std::cout << array[at(i,j)] << " ";
        }
      }
      std::cout << std::endl;
  }
}

void inspectMatrix(int w, int j, int i)
{
  std::cout << j << " " << i << std::endl;
  auto istart = i-1;
  auto jstart = i;
  for(int index = 0; index < i;++index)
  {
    std::cout << j << " " << istart-index << " + " << jstart-index << "," << i << std::endl;
  }
}

std::tuple<long long,long long> minmax(std::vector<char> &operators, std::vector<long long> &mins, std::vector<long long> &maxs, int w, int i, int j)
{
  // std::cout << "minmax " << j << " " << i << std::endl;

  auto at = [&](int i, int j){return i+(j*w); };
  long long min = std::numeric_limits<long long>::max(),
    max = std::numeric_limits<long long>::min();
    
  auto istart = i-1;
  auto jstart = i;
  for(int index = 0; index < (i-j);++index)
  {
    auto left_j = j;
    auto left_i = istart-index;
    auto left_min = mins[at(left_i,left_j)];
    auto left_max = maxs[at(left_i,left_j)];

    auto below_j = jstart-index;
    auto below_i = i;
    auto below_min = mins[at(below_i,below_j)];
    auto below_max = maxs[at(below_i,below_j)];

    auto op = operators[((left_i)*2)+1];
    auto r1 = eval(left_min,below_min, op);
    auto r2 = eval(left_max,below_max, op);
    auto possible_min = std::min(r1,r2);
    auto possible_max = std::max(r1,r2);

    // std::cout <<left_min << " " << op << " " << below_min << " = " << r1 << " or " << left_max << " " << op << " " << below_max << " = " << r2 << " [" << left_j << "," << left_i << "]" << " [" << below_j << "," << below_i << "] " << std::endl;

    if(possible_min < min) min = possible_min;
    if(possible_max > max) max = possible_max;
  }

  mins[at(i,j)] = min;
  maxs[at(i,j)] = max;
}

long long get_maximum_value_dynamic(string exp){
  std::stringstream ss(exp);
  auto operators = std::vector<char>();
  for(char c; ss >> c;)
  {
    operators.push_back(c);
  }

  // std::cout << operators << std::endl;

  auto digit_count = (operators.size() / 2) + 1;
  auto stride = digit_count;
  auto mins = std::vector<long long>(stride*stride);
  auto maxs = std::vector<long long>(stride*stride);
  auto at = [&](int i, int j){return i+(j*stride); };
  for(int x = 0; x < digit_count; ++x)
  {
    auto c = (long long)(operators[x*2]-48);
    mins[at(x,x)] = c;
    maxs[at(x,x)] = c;
  }

  // print(&mins[0], stride, stride, -1, -1);
  // std::cout << std::endl;
  // print(&maxs[0], stride, stride, -1, -1);
  // std::cout << "-----------------------------" << std::endl;

  auto n = digit_count;
  for(int s = 0; s < n;++s)
  {
    for(int j = 0; j < n-s;++j)
    {
      int i = j + s;    
      if(i==j) continue;
      // std::cout << "MINS -----------------------------------" << std::endl;
      // print(&mins[0], stride, stride, i, j);
      // std::cout << "MAXS -----------------------------------" << std::endl;
      // print(&maxs[0], stride, stride, i, j);
      // std::cout << std::endl;

      minmax(operators, mins, maxs, stride, i, j);
    }
  }

  // std::cout << "MINS -----------------------------" << std::endl;
  // print(&mins[0], stride, stride, n-1, 0);
  // std::cout << "MAXS -----------------------------" << std::endl;
  // print(&maxs[0], stride, stride, n-1, 0);

  return std::max(mins[at(n-1,0)],maxs[at(n-1,0)]);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("get_maximum_value_naive must work","get_maximum_value_naive")
{
  // REQUIRE(get_maximum_value_naive("1 + 5") == 6);
  // REQUIRE(get_maximum_value_naive("1 + 5 + 2") == 8);
  // REQUIRE(get_maximum_value_naive("1 + 5 * 2") == 12);
  // REQUIRE(get_maximum_value_naive("1 - 5 * 2") == -8);
  // REQUIRE(get_maximum_value_naive("5 - 8 + 7 * 4 - 8 + 9") == 200);
  REQUIRE(get_maximum_value_naive("2+8*3-9-5-3")== 25);
}

TEST_CASE("get_maximum_value_dynamic must work","get_maximum_value_dynamic")
{
  // REQUIRE(get_maximum_value_dynamic("1 + 5") == 6);
  // REQUIRE(get_maximum_value_dynamic("1 + 5 + 2") == 8);
  // REQUIRE(get_maximum_value_dynamic("1 + 5 * 2") == 12);
  // REQUIRE(get_maximum_value_dynamic("1 - 5 * 2") == -8);
  // REQUIRE(get_maximum_value_dynamic("5 - 8 + 7 * 4 - 8 + 9") == 200);
  // REQUIRE(get_maximum_value_dynamic("2+8*3-9-5-3")== 25);
}

void printProg(int x, int total){
  int progress = ((double)x / total) * 100;
  std::cout << "[";
  for (int i=1;i<=100;i++){
      if (i<progress || progress==100)
          std::cout << "=";
      else if (i==progress)
          std::cout << ">";
      else
          std::cout << " ";
  }

  std::cout << "] " << x << "/" << total << "\r" << std::flush;
}

std::string random_expression( size_t length )
{
    auto randdigit = []() -> char {
      static const char digits[] = "0123456789";
      const size_t max_index = (sizeof(digits) - 1);
      return digits[ rand() % max_index ];
    };
    auto randop = []() -> char{
        static const char ops[] = "+-*";
        const size_t max_index = (sizeof(ops) - 1);
        return ops[ rand() % max_index ];
    };
    std::string str(length,0);
    for(int i = 0; i < str.size();++i){
      if((i % 2) == 0){
        str[i] = randdigit();
      }else{
        str[i] = randop();
      }
    }
    return str;
}


void compare()
{
  auto size = (std::rand() % 5) + 2;
  size = size + (size-1);
  auto expression = random_expression(size);
  auto r1 = get_maximum_value_naive(expression);
  auto r2 = get_maximum_value_dynamic(expression);
  auto isEqual = r1 == r2;
  if(!isEqual) std::cout << expression << " result: "<< r1 << r2 << std::endl;

  REQUIRE(r1==r2);
}

TEST_CASE("get_maximum_value must be equal","get_maximum_value")
{
  // for(int i = 0; i <= 1000000;++i)
  // {
  //   printProg(i,1000000);
  //   compare();
  // }
}

#else

int main() {
  string s;
  std::cin >> s;
  std::cout << get_maximum_value_dynamic(s) << '\n';
}

#endif