#include <iostream>
#include <vector>
#include <functional>
#include <cmath>
#include <numeric>
#include <map>

using std::vector;

std::vector<int> getOnLocations(int a) {
  std::vector<int> result;
  int place = 0;
  while (a != 0) {
    if (a & 1) {
      result.push_back(place);
    }
    ++place;
    a >>= 1;
  }
  return result;
}

template<typename T>
void powerSet(const vector<T>& set, std::function<void(std::vector<T>)> f) {
  int numPowerSets = static_cast<int>(pow(2.0, static_cast<double>(set.size())));
  for (size_t i = 0; i < numPowerSets; ++i) {
    vector<int> onLocations = getOnLocations(i);
    vector<T> subSet;
    for (size_t j = 0; j < onLocations.size(); ++j) {
      subSet.push_back(set.at(onLocations.at(j)));
    }
   f(subSet);
  }
}

int optimal_weight_naive(int W, vector<int> w) {
  int current_weight = 0;
  powerSet<int>(w, [&](auto s){
    auto weight = std::accumulate(s.begin(), s.end(), 0);
    if(weight <= W && weight > current_weight)
    {
      current_weight = weight;
    }
  });
  return current_weight;
}

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
}


int optimal_weight_dynamic(int capacity, vector<int> items) {
  auto itemsSize = items.size();
  auto solutions = new int[(capacity+1)*(itemsSize+1)];
  auto at = [&](int x,int y){return x+(y*(capacity+1));};

  for(int i = 0;i <= itemsSize;++i) solutions[at(0,i)] = 0;
  for(int w = 0;w <= capacity;++w) solutions[at(w,0)] = 0;

  for(int i = 1;i <= itemsSize;++i)
  {
    auto currentItemWeight = items[i-1];
    // std::cout << " item weigth: " << currentItemWeight << std::endl;
    for(int w = 1;w <= capacity;++w)
    {
      // print(solutions, capacity+1, itemsSize+1, w, i);
      //a knapsack of capacity w using i items
      //is at least as good a on with capacity
      //w using i-1 items.
      solutions[at(w,i)] = solutions[at(w,i-1)];
      
      if(currentItemWeight <= w)
      {
        //another possible solution is a solution
        //not using the current item but
        //with enought space for this current item
        auto val = solutions[at(w-currentItemWeight,i-1)] + currentItemWeight;
        if(solutions[at(w,i)] < val)
        {
          solutions[at(w,i)] = val;
        }
      }

      // std::cout << std::endl;
    }
  }

  // print(solutions, capacity+1, itemsSize+1, -1, -1);

  return solutions[at(capacity,itemsSize)];
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("optimal_weigth must work","[optimal_weigth]")
{
  // REQUIRE(optimal_weight_naive(10,{1,4,8}) == 9);
  REQUIRE(optimal_weight_dynamic(10,{1,4,8}) == 9);
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

bool compare(int n, std::vector<int> weights)
{
  auto r1 = optimal_weight_naive(n, weights);
  auto r2 = optimal_weight_dynamic(n, weights);
  return r1 == r2;
}

TEST_CASE("optimal_sequence random cases" "[optimal_sequence]")
{
  for(int i = 0; i <= 100000;++i)
  {
    printProg(i,100000);

    auto weights = std::vector<int>(std::rand() % 12);
    std::generate(weights.begin(), weights.end(), [](){ return std::rand() % 100;});
    compare(std::rand() % 1000, weights);
  }
}

#else

int main() {
  int n, W;
  std::cin >> W >> n;
  vector<int> w(n);
  for (int i = 0; i < n; i++) {
    std::cin >> w[i];
  }
  std::cout << optimal_weight_dynamic(W, w) << '\n';
}

#endif