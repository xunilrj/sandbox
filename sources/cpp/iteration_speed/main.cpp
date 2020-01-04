#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include "../deltaTime/deltaTime.h"

template <typename T>
void time_for_each(const T& items)
{
    typename T::value_type accum = 0;

    deltaTime<double> dt{ true };
    for(auto&& x : items)
    {
        accum += x;
    }
    auto elapsed = dt.elapsed();
    std::cout << "For Each: " << elapsed << "ms. [" << accum << "]" << std::endl;
}

template <typename T>
void time_begin_end(const T& items)
{
    typename T::value_type accum = 0;

    deltaTime<double> dt{ true };
    auto it = items.cbegin();
    auto end = items.cend();
    while(it != end)
    {
        accum += *it;
        ++it;
    }
    auto elapsed = dt.elapsed();
    std::cout << "Begin/End: " << elapsed << "ms. [" << accum << "]" << std::endl;
}

template <typename T>
void time_by_index(const T& items)
{
    typename T::value_type accum = 0;

    deltaTime<double> dt{ true };
    auto size = items.size();
    for(int i = 0; i < size; ++i)
    {
        accum += items[i];
    }
    auto elapsed = dt.elapsed();
    std::cout << "Indices: " << elapsed << "ms. [" << accum << "]" << std::endl;
}

template <typename T>
void time_data_directly(const T& items)
{
    typename T::value_type accum = 0;
    auto size = items.size();
    auto* item = items.data();

    deltaTime<double> dt{ true };
    for(int i = 0; i < size; ++i,++item)
    {
        accum += *item;
    }
    auto elapsed = dt.elapsed();
    std::cout << "Data: " << elapsed << "ms. [" << accum << "]" << std::endl;
}

int main()
{
    std::vector<int> numbers (100000000);
    std::fill(numbers.begin(), numbers.end(), 0);
    
    time_for_each(numbers);
    time_begin_end(numbers);
    time_by_index(numbers);
    time_data_directly(numbers);

    return 0;
}