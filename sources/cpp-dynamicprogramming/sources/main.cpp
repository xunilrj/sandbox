

// Template Recursive Fibonacci
// Complexity
// T( n <= 1 ) = O(1)
// T( n ) = T(n-1) + T(n-2) + O(1)
 
template <int N>
struct CTFibonacci
{
    static constexpr int value()
    {
        return CTFibonacci<N - 1>::value() + CTFibonacci<N - 2>::value();
    }
};

template <>
struct CTFibonacci<1>
{
    static constexpr int value()
    {
        return 1;
    }
};

template <>
struct CTFibonacci<0>
{
    static constexpr int value()
    {
        return 0;
    }
};

#include <iostream>

int main(int argc, char **argv)
{
    int fibs[] { CTFibonacci<0>::value(), CTFibonacci<1>::value(), CTFibonacci<2>::value(), CTFibonacci<3>::value() };

    for(auto item : fibs)
    {
        std::cout << item << " ";
    }

    std::cout << std::endl;
    
    return 0;
}