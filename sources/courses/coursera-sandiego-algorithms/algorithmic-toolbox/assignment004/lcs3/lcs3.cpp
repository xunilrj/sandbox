#include <iostream>
#include <vector>
#include <tuple>
#include <algorithm>

using std::vector;



long long lcs2(vector<long long> &a, vector<long long> &b, int i, int j)
{
  if((i == -1)||(j==-1))
  {
    return 0;
  }
  else if (a[i] == b[j])
  {
    return 1 + lcs2(a,b, i-1,j-1);
  }
  else
  {
    auto r1 = lcs2(a,b,i-1,j);
    auto r2 = lcs2(a,b,i,j-1);
    return std::max(r1,r2);
  }
}

long long lcs2(vector<long long>& a, vector<long long>& b)
{
  auto h = a.size()+1;
  auto w = b.size()+1;
  auto solutions = std::vector<long long>(h*w);
  auto at = [=](const int row, const int column) { return column+(row*w);};

  for(auto y = 0; y < h;++y)
  {
    for(auto x = 0; x < w;++x)
    {
        if((x==0)||(y==0))
        {
          solutions[at(y,x)] = 0;
        }
        else
        {
          if(a[y-1] == b[x-1])
          {
            solutions[at(y,x)] = solutions[at(y-1,x-1)] + 1;
          }
          else
          {
            auto r1 = solutions[at(y-1,x)];
            auto r2 = solutions[at(y,x-1)];
            solutions[at(y,x)] = std::max(r1,r2);
          }
        }
    }
  }
  auto r = solutions[at(a.size(),b.size())];
  return r;
}

long long lcs3(vector<long long> &a, vector<long long> &b, vector<long long> &c) {
  // auto ab = lcs2(a,b);
  // auto bc = lcs2(b,c);
  // auto ac = lcs2(a,c);
  // return std::min(std::min(ab, bc), ac);
  auto h = a.size()+1;
  auto w = b.size()+1;
  auto d = c.size()+1;
  auto solutions = std::vector<long long>(h*w*d);
  auto at = [=](const int row, const int column, const int depth) { return column+(row*w)+(depth*w*h);};

  for(auto y = 0; y < h;++y)
  {
    for(auto x = 0; x < w;++x)
    {
      for(auto z = 0; z < d;++z)
      {
        if((x==0)||(y==0)||(z==0))
        {
          solutions[at(y,x,z)] = 0;
        }
        else
        {
          if((a[y-1] == b[x-1]) && (b[x-1] == c[z-1]))
          {
            solutions[at(y,x,z)] = solutions[at(y-1,x-1,z-1)] + 1;
          }
          else
          {
            auto r1 = solutions[at(y-1,x-0,z-0)]; //y
            auto r2 = solutions[at(y-0,x-1,z-0)]; //x
            auto r3 = solutions[at(y-0,x-0,z-1)]; //z
            auto r4 = solutions[at(y-1,x-0,z-1)]; //yz
            auto r5 = solutions[at(y-0,x-1,z-1)]; //xz
            auto r6 = solutions[at(y-1,x-1,z-0)]; //xy
            long long min, max;
            std::tie(min,max) = std::minmax({r1,r2,r3,r4,r5,r6});
            solutions[at(y,x,z)] = max;
          }
        }
      }
    }
  }
  auto r = solutions[at(a.size(),b.size(),c.size())];
  return r;
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

long long lcs2_(std::vector<long long> a, std::vector<long long> b)
{
  return lcs2(a,b);
}

long long lcs3_(std::vector<long long> a, std::vector<long long> b, std::vector<long long> c)
{
  return lcs3(a,b,c);
}

TEST_CASE("lcs2 must work","lcs2")
{
  // REQUIRE(lcs2_({1},{1}) == 1);
  // REQUIRE(lcs2_({1,2},{1,2}) == 2);
  // REQUIRE(lcs2_({1,2,3},{1,4,3}) == 2);

  REQUIRE(lcs3_({1},{1},{1}) == 1);
  // REQUIRE(lcs3_({1000000000},{1000000000},{1000000000}) == 1);
  // REQUIRE(lcs3_({-1000000000},{-1000000000},{-1000000000}) == 1);
  REQUIRE(lcs3_({1},{2},{3}) == 0);
  REQUIRE(lcs3_({1,2,3},{2,1,3},{1,3,5}) == 2);
  REQUIRE(lcs3_({8,3,2,1,7},{8, 2, 1, 3, 8, 10, 7},{6, 8, 3, 1, 4, 7}) == 3);

  // REQUIRE(lcs3_({1,2,1,2,1},{1, 2, 1, 2},{1, 2, 1, 2}) == 4);
}

#else

int main() {
  size_t an;
  std::cin >> an;
  vector<long long> a(an);
  for (size_t i = 0; i < an; i++) {
    std::cin >> a[i];
  }
  size_t bn;
  std::cin >> bn;
  vector<long long> b(bn);
  for (size_t i = 0; i < bn; i++) {
    std::cin >> b[i];
  }
  size_t cn;
  std::cin >> cn;
  vector<long long> c(cn);
  for (size_t i = 0; i < cn; i++) {
    std::cin >> c[i];
  }
  std::cout << lcs3(a, b, c) << std::endl;
}

#endif
