#include <iostream>
#include <string>
#include <algorithm>
#include <numeric>
#include <tuple>
#include <map>

using std::string;

void print(int *array, int w, int h, int cx, int cy){
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
  std::cout << std::endl;
}

bool contains(std::string& s, char c)
{
  auto result = s.find(c);
  return result != std::string::npos;
}

int edit_distance_dynamic(const string str1, const string str2) {
  auto w = str1.size();
  auto h = str2.size();
  auto solutions = std::vector<int>(w*h);
  auto at = [&](int x,int y) {return (x-1)+((y-1)*w);};
  
  //bases cases
  auto strj = str2.substr(0,1);
  for(int i = 1;i <= str1.size();++i)
  {
    // print(&solutions[0], w, h, i-1, 0);
    auto stri = str1.substr(0,i);
    if(stri == strj)
    {
      solutions[at(i,1)] = 0;
    }else
    {
      auto stri_contains_j = contains(stri, str2[0]);
      auto score = stri_contains_j ? i-1 : i;
      solutions[at(i,1)] = score;
    }
  }
  auto stri = str1.substr(0,1);
  for(int j = 1;j <= str2.size();++j)
  {
    auto strj = str2.substr(0,j);
    // print(&solutions[0], w, h, 0, j-1);
    if(stri == strj)
    {
      solutions[at(1,j)] = 0;
    }else
    {
      auto strj_contains_i = contains(strj, str1[0]);
      auto score = strj_contains_i ? j-1 : j;
      solutions[at(1,j)] = score;
    }
  }

  for(int i = 2;i <= str1.size();++i)
  {
    for(int j = 2;j <= str2.size();++j)
    {
      // print(&solutions[0], w, h, i-1, j-1);
      //insert is (i,j-1)+1
      //deletion is (i-1,j)+1
      //mismatch is (i-1,j-1)+1
      //match is (i-1,j-1)

      auto dinsert = solutions[at(i,j-1)] + 1;
      auto ddeletion = solutions[at(i-1,j)]+1;
      auto d = solutions[at(i-1,j-1)];
      auto dmismatch = std::numeric_limits<int>::max();
      auto dmatch = std::numeric_limits<int>::max();

      if(str1[i-1] != str2[j-1])
      {
        dmismatch = d+1;
      }

      if(str1[i-1] == str2[j-1])
      {
        dmatch = d;
      }

      auto min = 0, max = 0;
      std::tie(min,max) = std::minmax({dinsert,ddeletion,dmismatch,dmatch});

      solutions[at(i,j)] = min;
    }
  }

  // print(&solutions[0], w, h, -1, -1);
  return solutions[at(str1.size(),str2.size())];
}

std::ostream & tab(int depth)
{
  return std::cout << depth << " " << std::string((depth*2), '.');
}

std::map<std::tuple<int,int>,int> memoize;
int edit_distance_naive_internal(const string str1, const string str2, int i, int j, int depth)
{
  // tab(depth) << "[" << i << "," << j << "] ";

  auto memoizekey = std::make_tuple(i,j);
  auto search = memoize.find(memoizekey);
  if(search != memoize.end()) {
    // std::cout << " memoized " << search->second << std::endl;
    return search->second;
  }

  auto stri = str1.substr(0,i);
  auto strj = str2.substr(0,j);

  if(i == 1 && j == 1)
  {
    // std::cout << stri << " " << strj << std::endl;

    if(stri == strj) return 0;
    else return 1;
  }
  else if(i <= 1 && j > 1 )
  {
    auto strj_contains_i = contains(strj, str1.c_str()[0]);
    auto score = strj_contains_i ? j-1 : j;
    // std::cout << stri << " " << strj << " " << score << std::endl;
    return score;
  }
  else if(j <= 1 && i > 1)
  {
    auto stri_contains_j = contains(stri, str2.c_str()[0]);
    auto score = stri_contains_j ? i-1 : i;
    // std::cout << stri << " " << strj << " " << score << std::endl;
    return score;
  }
  else
  {
    // std::cout << stri << " " << strj << std::endl;
  }

  auto dinsert = edit_distance_naive_internal(str1, str2, i, j - 1, depth + 1) + 1; 
  auto ddeletion = edit_distance_naive_internal(str1, str2, i - 1, j, depth + 1) + 1; 
  auto d = edit_distance_naive_internal(str1, str2, i - 1, j - 1, depth + 1);
  auto dmismatch = std::numeric_limits<int>::max();
  auto dmatch = std::numeric_limits<int>::max();

  if(str1[i-1] != str2[j-1])
  {
    dmismatch = d+1;
  }

  if(str1[i-1] == str2[j-1])
  {
    dmatch = d;
  }

  // tab(depth) << "choosing [" << i << "," << j << "]: " << dinsert << "," << ddeletion << "," << dmismatch << "," << dmatch << std::endl;
  auto min = 0, max = 0;
  std::tie(min,max) = std::minmax({dinsert,ddeletion,dmismatch,dmatch});

  memoize[memoizekey] = min;
  return min;
}

int edit_distance_naive(const string str1, const string str2)
{
  memoize.clear();
  // std::cout << "-----------------------------" << std::endl;
  auto r = edit_distance_naive_internal(str1, str2, str1.size(), str2.size(), 0);

  // for(auto kv : memoize)
  // {
  //     std::string s1,s2;
  //     int i, j;
  //     std::tie(s1,s2,i,j) = kv.first;
  //     auto stri = s1.substr(0,i);
  //     auto strj = s2.substr(0,j);
  //     std::cout << "[" << stri << "," << strj << "] = " << kv.second << std::endl;
  // } 

  // std::cout << "-----------------------------" << std::endl;
  return  r;
}


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void compare(std::string str1, std::string str2)
{
  auto r1 = edit_distance_naive(str1,str2);
  auto r2 = edit_distance_dynamic(str1,str2);
  if(r1 != r2)
  {
    std::cout << str1 << " " << str2 << std::endl;
  }
  REQUIRE(r1 == r2);
}

TEST_CASE("edit_distance_naive must work","edit_distance_naive")
{
  REQUIRE(edit_distance_dynamic("a","b") == 1);
  REQUIRE(edit_distance_dynamic("ab","ab") == 0);
  REQUIRE(edit_distance_dynamic("aa","ab") == 1);
  //short-
  //110001
  //-ports
  REQUIRE(edit_distance_dynamic("short","ports") == 3);
  // lQXmi-
  // 101111
  // -QVvTt
  REQUIRE(edit_distance_dynamic("lQXmi","QVvTt") == 5);
}

std::string random_string( size_t length )
{
    auto randchar = []() -> char
    {
        const char charset[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
        const size_t max_index = (sizeof(charset) - 1);
        return charset[ rand() % max_index ];
    };
    std::string str(length,0);
    std::generate_n( str.begin(), length, randchar );
    return str;
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

TEST_CASE("edit_distance_dynamic must work", "edit_distance_dynamic")
{
  for(int i = 0; i <= 100000;++i)
  {
    printProg(i,100000);
    auto s1 = random_string((std::rand() % 100)+1);
    auto s2 = random_string((std::rand() % 100)+1);
    //std::cout << s1 << " " << s2 << std::endl;
    compare(s1,s2);
  }
}

#else

int main() {
  string str1;
  string str2;
  std::cin >> str1 >> str2;
  std::cout << edit_distance_dynamic(str1, str2) << std::endl;
  return 0;
}

#endif