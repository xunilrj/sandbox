#include <iostream>

int fibonacci_sum_naive(long long n) {
    if (n <= 1)
        return n;

    long long previous = 0;
    long long current  = 1;
    long long sum      = 1;

    for (long long i = 0; i < n - 1; ++i) {
        long long tmp_previous = previous;
        previous = current;
        current = tmp_previous + current;
        sum += current;
    }

    return sum % 10;
}

long long pisano_period(long long m) {
    long long a = 0, b = 1, c = a + b;
    for (int i = 0; i < m * m; i++) {
        c = (a + b) % m;
        a = b;
        b = c;
        if (a == 0 && b == 1) return i + 1;
    }
}

long long fibonacci_mod(long long n, int m)
{
    auto pisanoperiod = pisano_period(m);
    auto newn = n % pisanoperiod;
    if(newn <= 1) return newn;
    auto nminus2 = 0;
    auto nminus1 = 1;
    auto fib = 1;
    for(auto i = 2; i <= newn; ++i)
    {
        fib = (nminus2 + nminus1) % m;
        nminus2 = nminus1;
        nminus1 = fib;
    }
    return fib % m;
}

//http://mathforum.org/library/drmath/view/52707.html
//Sum(F[i]) = F[n+2] - 1
int fibonacci_sum_last_digit(long long n){
    auto result = (fibonacci_mod(n+2, 10) - 1);
    //we can´t mod here because mod does not work with negative numbers
    //https://stackoverflow.com/questions/11630321/why-does-c-output-negative-numbers-when-using-modulo
    if(result == -1) return 9;
    return result; 
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("fibonacci_sum must work ", "[fib]")
{
    for(int i = 1; i < 60; ++i){
        REQUIRE(fibonacci_sum_naive(i) == fibonacci_sum_last_digit(i));
    } 

    REQUIRE(fibonacci_sum_last_digit(100) == 5);
    REQUIRE(fibonacci_sum_last_digit(239) == 0);
}

TEST_CASE("fibonacci_sum corner cases", "[fib]")
{
    REQUIRE(fibonacci_sum_last_digit(100000000000000) == 5);
}

#else

int main() {
    long long n = 0;
    std::cin >> n;
    std::cout << fibonacci_sum_last_digit(n);
}

#endif