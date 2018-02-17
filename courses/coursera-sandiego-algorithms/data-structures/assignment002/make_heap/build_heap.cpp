#include <iostream>
#include <vector>
#include <algorithm>

using std::vector;
using std::swap;
using std::pair;
using std::make_pair;



class HeapBuilder {
 private:
  vector<int> data_;
  vector< pair<int, int> > swaps_;

  void WriteResponse(std::ostream & out) const {
    out << swaps_.size() << std::endl;
    for (int i = 0; i < swaps_.size(); ++i) {
      out << swaps_[i].first << " " << swaps_[i].second << std::endl;
    }
  }

  void ReadData(std::istream & in) {
    int n;
    in >> n;
    data_.resize(n);
    for(int i = 0; i < n; ++i)
      in >> data_[i];
  }

  void GenerateSwaps() {
    // std::cout << "GenerateSwap -------------------------" << std::endl;
    swaps_.clear();
    auto n = data_.size();
    for(int i = n/2;i >= 0;--i)
    {
      SiftDown(i);
    }
    // // The following naive implementation just sorts 
    // // the given sequence using selection sort algorithm
    // // and saves the resulting sequence of swaps.
    // // This turns the given array into a heap, 
    // // but in the worst case gives a quadratic number of swaps.
    // //
    // // TODO: replace by a more efficient implementation
    // for (int i = 0; i < data_.size(); ++i)
    //   for (int j = i + 1; j < data_.size(); ++j) {
    //     if (data_[i] > data_[j]) {
    //       swap(data_[i], data_[j]);
    //       swaps_.push_back(make_pair(i, j));
    //     }
    //   }
  }

  int parent_of(int i) const{
    return ((i+1)/2)-1;
  }

  int lchild_of(int i) const{
    return (2*(i+1)) - 1;
  }

  int rchild_of(int i) const {
    return (2*(i+1));
  }

  bool compare(int l, int r) const {
    return l < r;
  }

  void printTree(int currentindex, int i = 0, int depth = 0)
  {
    if(currentindex == i)
    {
      std::cout << std::string(depth*2, ' ') << "[" << data_[i] << "]" << std::endl;
    }
    else
    {
      std::cout << std::string(depth*2, ' ') << data_[i] << std::endl;
    }

    auto lindex = lchild_of(i);
    if(lindex < data_.size())
    {
      printTree(currentindex, lindex, depth + 1);
    }
    
    auto rindex = rchild_of(i);
    if(rindex < data_.size())
    {
      printTree(currentindex, rindex, depth + 1);
    }
  }

  void SiftDown(int i){
    while(true){
      // std::cout << i << " -----------------------------" << std::endl;
      // printTree(i);      
      auto betterindex = i;
      auto bettervalue = data_[i];

      auto lindex = lchild_of(i);
      if(lindex < data_.size())
      {
        auto lvalue = data_[lindex];
        if(compare(lvalue,bettervalue))
        {
          betterindex = lindex;
          bettervalue = lvalue;
        }  
      }
      
      auto rindex = rchild_of(i);
      if(rindex < data_.size())
      {
        auto rvalue = data_[rindex];
        if(compare(rvalue,bettervalue))
        {
          betterindex = rindex;
          bettervalue = rvalue;
        }  
      }      
      
      if(betterindex != i)
      {
        // std::cout << "better index " << i << " " << betterindex << std::endl;
        std::swap(data_[i], data_[betterindex]);
        swaps_.emplace_back(i, betterindex);
        i = betterindex;
      }
      else
      {
        // std::cout << "breaking" << std::endl;
        break;
      }
    }
  }
 public:
  void Solve(std::istream & in, std::ostream& out) {
    ReadData(in);
    GenerateSwaps();
    WriteResponse(out);
  }
};

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& str, const std::string& expected)
{
  auto in = std::stringstream{str};
  auto out = std::stringstream{};
  HeapBuilder heap_builder;
  heap_builder.Solve(in, out);
  out.seekg(0);
  // std::cout << out.str();
  // out.seekg(0);

  auto expectedOut = std::stringstream{expected};
  while (!expectedOut.eof()) {
    int e, a;
    expectedOut >> e;
    out >> a;

    REQUIRE(e == a);
  }    
}

TEST_CASE("getHeight must work","getHeight")
{
  test("5 5 4 3 2 1", "3 1 4 0 1 1 3");
  test("5 1 2 3 4 5", "0");
}

#else

int main() {
  HeapBuilder heap_builder;
  heap_builder.Solve(std::cin, std::cout);
  return 0;
}

#endif
