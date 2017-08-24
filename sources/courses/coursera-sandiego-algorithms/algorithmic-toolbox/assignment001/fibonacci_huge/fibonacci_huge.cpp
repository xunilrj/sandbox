#include <iostream>

long long get_fibonacci_huge_naive(long long n, long long m) {
    if (n <= 1)
        return n;

    long long previous = 0;
    long long current  = 1;

    for (long long i = 0; i < n - 1; ++i) {
        long long tmp_previous = previous;
        previous = current;
        current = tmp_previous + current;
    }

    return current % m;
}

//https://oeis.org/A001175
//calculate fibonacci mod m
//if we find 0 1 again,
//assume is a cycle
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
    // std::cout << "pisano_period "<< n << " " << m <<std::endl;
    auto pisanoperiod = pisano_period(m);
    auto newn = n % pisanoperiod;

    // std::cout << "fibonacci_mod "<< newn << " " << m <<std::endl;
    
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

    // std::cout << "result " << fib % m << std::endl;

    return fib % m;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("Pisano Period must work ", "[fib]")
{
  REQUIRE(pisano_period(2) == 3); //1*n = 3
  REQUIRE(pisano_period(4) == 6); //2*n = 6
  REQUIRE(pisano_period(6) == 24); //3*n = 24
  REQUIRE(pisano_period(8) == 12); //4*n = 12
  REQUIRE(pisano_period(10) == 60); //5*n = 60
  REQUIRE(pisano_period(12) == 24); //6*n = 24
  REQUIRE(pisano_period(14) == 48); //7*n = 48
  REQUIRE(pisano_period(12) == 24); //2 * 2 * 3
  REQUIRE(pisano_period(14) == 48); //2 * 7
  REQUIRE(pisano_period(1000) == 1500); //2 * 7
}

TEST_CASE("FibonacciMod must work", "[fib]")
{
  REQUIRE(fibonacci_mod(1, 239) == 1);
  REQUIRE(fibonacci_mod(2015, 3) == 1);
  REQUIRE(fibonacci_mod(239, 1000) == 161);
  REQUIRE(fibonacci_mod(2816213588, 30524) == 10249);
}

TEST_CASE("FibonacciMod corner cases", "[fib]")
{
  REQUIRE(fibonacci_mod(1000000000000000000, 100000) == 46875);
  REQUIRE(fibonacci_mod(99999999999999999, 3) == 1);
}

#else

int main() {
    long long n, m;
    std::cin >> n >> m;
    std::cout << fibonacci_mod(n, m) << '\n';
}

#endif