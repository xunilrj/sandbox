#include <iostream>
#include <vector>
#include <algorithm>
#include <limits>
#include <iterator>
#include <map>

using std::vector;


std::ostream & tab(int size)
{
    return (std::cout << std::string(size * 3, ' '));
}

std::ostream & operator <<(std::ostream & o, std::vector<int> a)
{
    o << "[";
    for(auto i : a)
    {
        o << i << ",";
    }
    o << "]";
}

std::vector<int> unionn (std::vector<int> l, std::vector<int> r)
{
  auto re = std::vector<int>();
  std::copy(l.begin(),l.end(),std::back_inserter(re));
  std::copy(r.begin(),r.end(),std::back_inserter(re));
  return re;
}

std::map<int,vector<int>> memoize{{1,{1}}};

vector<int> naive_optimal_sequence_internal(int n, std::vector<int> current, int depth)
{
  auto search = memoize.find(n);
  if(search != memoize.end()) {
    // tab(depth) << n << " memoized " << search->second << std::endl;
    return search->second;
  }

  // tab(depth) << n << " " << current << std::endl;
  if(n <= 1) return {1};

  auto r1score = std::numeric_limits<int>::max();
  auto r2score = std::numeric_limits<int>::max();
  std::vector<int> r1;
  std::vector<int> r2;
  if((n % 3) == 0)
  {
    r1 = unionn(naive_optimal_sequence_internal(n/3, current, depth + 1),{n});
    r1score = r1.size();
  }
  if((n%2) == 0)
  {
    r2 = unionn(naive_optimal_sequence_internal(n/2, current, depth + 1),{n});
    r2score = r2.size();
  }

  auto r3 = unionn(naive_optimal_sequence_internal(n-1, current, depth + 1),{n});
  
  if(r1score <= r2score && r1score <= r3.size()) 
  {
    memoize[n] = r1;
    return r1;
  }
  else if(r2score <= r1score && r2score <= r3.size())
  {
    memoize[n] = r2;
    return r2;
  }
  else
  {
    memoize[n] = r3;
    return r3;
  }
}

vector<int> naive_optimal_sequence(int n)
{
  auto r = naive_optimal_sequence_internal(n,{}, 0);
  // reverse(r.begin(), r.end());
  return  r;
}

std::map<int,int> path{{1,1}};

vector<int> rebuild_path(int n)
{
  auto result = std::vector<int>();
  while(n>1)
  {
    result.push_back(n);
    auto nscore = path[n] - 1;
    
    if(((n%3)==0)&&(path[n/3] == nscore)){n = n/3;}
    else if(((n%2)==0)&&(path[n/2] == nscore)){n = n/2;}
    else{n = n -1;}
  }
  result.push_back(1);
  reverse(result.begin(),result.end());
  return result;
}

vector<int> optimal_sequence_dynamic(int n)
{
  for(int i = 1; i <= n; ++i)
  {
    // std::cout << i << " ";

    auto search = path.find(i);
    if(search == path.end())
    {
      // std::cout << " ... ";

      auto r1score = std::numeric_limits<int>::max();
      auto r2score = std::numeric_limits<int>::max();
      auto r3score = std::numeric_limits<int>::max();

      if((i%3)==0)
      {
        // std::cout << "analysing i/3" << std::endl;
        r1score = path[i/3]+1;
      }
      if((i%2)==0)
      {
        // std::cout << "analysing i/2" << std::endl;
        r2score = path[i/2] + 1;
      }
      
      {
        // std::cout << "analysing i-1" << std::endl;        r3 = memoize[i-1];
        r3score = path[i-1] + 1;
      }

      if(r1score <= r2score && r1score <= r3score){path[i] = r1score;}
      else if(r2score <= r1score && r2score <= r3score){path[i] = r2score;}
      else{path[i] = r3score;}
    }
    else
    {
      // std::cout << "already calculated ";
    }
    // std::cout << memoize[i] << std::endl;
  }
  return rebuild_path(n);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

bool compareDynamic(int n, std::vector<int> result)
{
  auto r = optimal_sequence_dynamic(n);
  return r == result;
}

TEST_CASE("optimal_sequence must work" "[optimal_sequence]")
{
  //1 2 3 4 5 6 7 8 9
  //1 2 2 3 4 3 4 4 3
  REQUIRE(compareDynamic(1,{1}));
  REQUIRE(compareDynamic(2,{1,2}));
  REQUIRE(compareDynamic(3,{1,3}));
  REQUIRE(compareDynamic(4,{1,2,4}));
  REQUIRE(compareDynamic(5,{1,2,4,5}));
  REQUIRE(compareDynamic(6,{1,2,6}));
  REQUIRE(compareDynamic(7,{1,2,6,7}));
  REQUIRE(compareDynamic(8,{1,2,4,8}));
  REQUIRE(compareDynamic(9,{1,3,9}));
  REQUIRE(compareDynamic(10,{1,3,9,10}));
  REQUIRE(compareDynamic(15,{1,2,4,5,15}));
  REQUIRE(compareDynamic(96234,{1,3,9,10,11,22,66,198,594,1782,5346,16038,16039,32078,96234}));
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

bool compare(int n)
{
  auto r1 = naive_optimal_sequence(n);
  auto r2 = optimal_sequence_dynamic(n);
  return r1 == r2;
}

TEST_CASE("optimal_sequence random cases" "[optimal_sequence]")
{
  for(int i = 0; i <= 1000000;++i)
  {
    printProg(i,1000000);
    compare(i);
  }
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<int> sequence = optimal_sequence_dynamic(n);
  std::cout << sequence.size() - 1 << std::endl;
  for (size_t i = 0; i < sequence.size(); ++i) {
    std::cout << sequence[i] << " ";
  }
}

#endif