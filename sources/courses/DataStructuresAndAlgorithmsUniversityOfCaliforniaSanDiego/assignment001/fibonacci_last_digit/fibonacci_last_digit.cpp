#include <iostream>

int get_fibonacci_last_digit_naive(int n)
{
    if (n <= 1)
        return n;

    int previous = 0;
    int current = 1;

    for (int i = 0; i < n - 1; ++i)
    {
        int tmp_previous = previous;
        previous = current;
        current = tmp_previous + current;
    }

    return current % 10;
}

int get_fibonacci_last_digit(int n)
{
    if (n < 0)
        return 0;
    if (n <= 1)
        return n;

    int previous = 0;
    int current = 1;

    for (int i = 0; i < n - 1; ++i)
    {
        int tmp_previous = previous;
        previous = current;
        current = (tmp_previous + current) % 10;
    }

    return current;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

TEST_CASE("Fibonacci Last Digit negative numbers", "[fibonacci]")
{
    REQUIRE(get_fibonacci_last_digit(-1) == 0);
    REQUIRE(get_fibonacci_last_digit(std::numeric_limits<int>::max() + 1) == 0);
}

TEST_CASE("Fibonacci Last Digit corner cases", "[fibonacci]")
{
    REQUIRE(get_fibonacci_last_digit(0) == 0);
    REQUIRE(get_fibonacci_last_digit(1) == 1);
    REQUIRE(get_fibonacci_last_digit(std::numeric_limits<int>::max()) == 3);
}

TEST_CASE("Fibonacci Last Digit must be correct", "[fibonacci]")
{
    REQUIRE(get_fibonacci_last_digit(3) == 2);
    REQUIRE(get_fibonacci_last_digit(331) == 9);
    REQUIRE(get_fibonacci_last_digit(327305) == 5);
}

#else

int main()
{
    int n;
    std::cin >> n;
    int c = get_fibonacci_last_digit(n);
    std::cout << c << '\n';
}

#endif