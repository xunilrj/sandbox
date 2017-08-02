#include <iostream>
#include <vector>
using std::vector;

long long get_fibonacci_partial_sum_naive(long long from, long long to) {
    if (to <= 1)
        return to;

    long long previous = 0;
    long long current  = 1;

    for (long long i = 0; i < from - 1; ++i) {
        long long tmp_previous = previous;
        previous = current;
        current = tmp_previous + current;
    }

    long long sum = current;

    for (long long i = 0; i < to - from; ++i) {
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

int fibonacci_sum_last_digit(long long n, int m)
{
    auto result = (fibonacci_mod(n+2, m) - 1);
    if(result == -1) return (m-1);
    return result; 
}

long long get_fibonacci_partial_sum(long long from, long long to) {
    int mod = 10000;
    auto sumto = fibonacci_sum_last_digit(to, mod);
    auto sumfrom = fibonacci_sum_last_digit(from-1, mod);
    auto result =  sumto - sumfrom; 
    if(result < 0) return (mod + result) % 10;
    return result % 10;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("fibonacci_sum must work ", "[fib]")
{
    REQUIRE(get_fibonacci_partial_sum(3,7) == 1);
    REQUIRE(get_fibonacci_partial_sum(10,10) == 5);
    REQUIRE(get_fibonacci_partial_sum(10,200) == 2);

    for(int i = 1; i < 20; ++i){
        for(int j = 1; j < 20; ++j){
            REQUIRE(get_fibonacci_partial_sum_naive(i, i+j) == get_fibonacci_partial_sum(i,i+j));
        }
    } 
}

TEST_CASE("fibonacci_sum corner cases", "[fib]")
{
    REQUIRE(get_fibonacci_partial_sum(1000000000000000000,1000000000000000000) == 5);
    REQUIRE(get_fibonacci_partial_sum(1000000000000000000,1000000000000000000-1) == 0);
    REQUIRE(get_fibonacci_partial_sum(1000000000000000000-1,1000000000000000000) == 1);
}

#else

int main() {
    long long from, to;
    std::cin >> from >> to;
    std::cout << get_fibonacci_partial_sum(from, to) << '\n';
}

#endif