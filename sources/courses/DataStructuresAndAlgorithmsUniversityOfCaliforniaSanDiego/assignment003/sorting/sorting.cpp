#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>
#include <cstdlib>
#include <tuple>

using std::vector;
using std::swap;

void printVector(const std::vector<int> &a)
{
  std::cout << "[";
  for(auto i : a)
  {
    std::cout << i << ",";
  }
  std::cout << "]" << std::endl;
}

std::tuple<int,int> partition2(vector<int> &a, int l, int r) {
  int x = a[l];
  int j = l;
  for (int i = l + 1; i <= r; i++) {
    if (a[i] <= x) {
      j++;
      swap(a[i], a[j]);
    }
  }
  swap(a[l], a[j]);
  return std::make_tuple(j,j);
}

std::tuple<int,int> partition3(vector<int> &a, int l, int r) {
  // std::cout << "PART " << l << " " << r << std::endl;
  int x = a[l];
  int lt = l;
  int gt = r;
  for (int i = l ; i <= gt; i++) {
    // std::cout << "-----------------------" << std::endl;
    // printVector(a);
    // std::cout << i << "," << x << "," << lt << "," << gt << std::endl;
    if (a[i] < x) {
      // std::cout << a[i] << " smaller " << x << std::endl;
      swap(a[i], a[lt]);
      lt++;      
    }
    else if(a[i] > x)
    {
      // std::cout << a[i] << " bigger " << x << std::endl;
      swap(a[i], a[gt]);
      gt--;
      i--;  
    }
    else
    {
      // std::cout << a[i] << " equal " << x << std::endl;
    }
    // printVector(a);
    // std::cout << "-----------------------" << std::endl;
  }
  // std::cout << "x" << "," << x << "," << lt << "," << gt << std::endl;
  return std::make_tuple(lt,gt);
}

template<typename Partition>
void randomized_quick_sort(Partition p, vector<int> &a, int l, int r) {
  if (l >= r) {
    return;
  }

  int k = l + rand() % (r - l + 1);
  swap(a[l], a[k]);

  int m1,m2;
  std::tie(m1,m2) = p(a, l, r);

  if(m1 > l) randomized_quick_sort(p, a, l, m1-1);
  if(m2 < r) randomized_quick_sort(p, a, m2+1, r);
}

void randomized_quick2(vector<int> &a)
{
	randomized_quick_sort(partition2, a, 0, (int)a.size()-1);
}

void randomized_quick3(vector<int> &a)
{
  randomized_quick_sort(partition3, a, 0, (int)a.size()-1);
}

void randomized_quick(vector<int> &a)
{
  randomized_quick3(a);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

std::vector<int> randomized_quick_sort_(std::vector<int> a)
{
  // printVector(a);
	randomized_quick(a);
  // printVector(a);
	return a;
}

std::vector<int> randomized_quick2_(std::vector<int> a)
{
	randomized_quick2(a);
	return a;
}

bool _ (std::vector<int> l, std::vector<int> r)
{
	return l == r;
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

TEST_CASE("quick3 must work", "[quick3]")
{
  	REQUIRE(_(randomized_quick_sort_({2,3,9,2,2}), {2,2,2,3,9}));
    REQUIRE(_(randomized_quick_sort_({2,3,9,2,2,1}), {1,2,2,2,3,9}));
    REQUIRE(_(randomized_quick_sort_({2,3,9,2,2,1,7,8}), {1,2,2,2,3,7,8,9}));

    REQUIRE(_(randomized_quick_sort_({1,2,2,3}), {1,2,2,3}));
    REQUIRE(_(randomized_quick_sort_({1,2,2,2,3}), {1,2,2,2,3}));
    REQUIRE(_(randomized_quick_sort_({1,2,2,2,2,3}), {1,2,2,2,2,3}));
    REQUIRE(_(randomized_quick_sort_({1,2,2,2,2,2,3}), {1,2,2,2,2,2,3}));
    REQUIRE(_(randomized_quick_sort_({1,2,2,2,2,2,2,2,3}), {1,2,2,2,2,2,2,2,3}));

    REQUIRE(_(randomized_quick_sort_({1,2,2,2,5,4,4,4,3}), {1,2,2,2,3,4,4,4,5}));
}

#include <chrono>
#include <thread>
#include <iterator>

std::vector<int> getNumbers()
{
  std::vector<int> numbers;
  std::ifstream file("./numbers");
  std::copy(std::istream_iterator<int> (file), 
    std::istream_iterator<int>(),
    std::back_inserter(numbers));

  return numbers;
}

void printVector(std::string name, const std::vector<int> &a)
{
  std::ofstream output_file(name);
  for(auto i : a)
  {
    output_file << i << std::endl;
  }
}



std::tuple<int,int> partition3_(vector<int> a, int l, int r){
  // printVector(a);
  auto result = partition3(a, l, r); 
  // printVector(a);
  return result;
}

TEST_CASE("partition3 must work","[partition3]")
{
  REQUIRE(partition3_({2,4,1,2},0,3) == std::make_tuple(1,2));
  REQUIRE(partition3_({2,4,4,1,1,2,2,2},0,7) == std::make_tuple(2,5));
  REQUIRE(partition3_({3,4,6,1,2,3,3,3},0,7) == std::make_tuple(2,5));
  REQUIRE(partition3_({2,1,3,3},0,3) == std::make_tuple(1,1));
  REQUIRE(partition3_({3,4,6,4},0,3) == std::make_tuple(0,0));
  REQUIRE(partition3_({1,1,1,1},0,3) == std::make_tuple(0,3));
}

TEST_CASE("quick3 corner cases", "[quick3]")
{
  auto numbers = std::vector<int>(100000);
  std::iota(numbers.begin(),numbers.end(),0);
  REQUIRE(_(randomized_quick_sort_(numbers), numbers));

  REQUIRE(_(randomized_quick_sort_({1000000000,1,500}), {1,500,1000000000}));

  for(int i = 0; i <= 100000; ++i)
  {
    printProg(i, 100000);

    auto numbers = std::vector<int>(rand() % 100000);
    std::generate(numbers.begin(), numbers.end(), std::rand);
    auto r1 = randomized_quick_sort_(numbers);
    auto r2 = randomized_quick2_(numbers);
    auto areEqual = r1 == r2;
    if(areEqual == false)
    {
      std::ofstream output_file("./numbers");
      std::ostream_iterator<int> output_iterator(output_file, "\n");
      std::copy(numbers.begin(), numbers.end(), output_iterator);
    }
     REQUIRE(areEqual);
  }

  std::cout<<std::endl;
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<int> a(n);
  for (size_t i = 0; i < a.size(); ++i) {
    std::cin >> a[i];
  }
  randomized_quick(a);
  for (size_t i = 0; i < a.size(); ++i) {
    std::cout << a[i] << ' ';
  }
}

#endif