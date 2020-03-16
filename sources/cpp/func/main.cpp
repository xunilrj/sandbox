#define FUNC_BODY
#include "func.h"

#include <vector>
#include <random>
#include <algorithm>
#include <iterator>
#include <iostream>
#include <functional>

int sum(int a, int b, int c)
{
    return a+b+c;
}

int times(int a, int b)
{
    return a*b;
}

#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

TEST_CASE("Func.Currying.Call Syntax", "[ok]")
{
    auto f = $(sum);

    REQUIRE(f(1)(2)(3) == 6);
    REQUIRE(f(1, 2)(3) == 6);
    REQUIRE(f(1)(2, 3) == 6);
    REQUIRE(f(1, 2, 3) == 6);
}

TEST_CASE("Func.Currying.Call Syntax and STL integration", "[ok]")
{
    auto f = $(sum);

    auto v = std::vector<int>{ 1,2,3 };
    std::transform(std::begin(v), std::end(v),
        std::begin(v),
        $(times)(2));
    REQUIRE(v[0] == 2);
    REQUIRE(v[1] == 4);
    REQUIRE(v[2] == 6);
}

TEST_CASE("Func.Currying.Operator << Syntax", "[ok]")
{
    auto f = $(sum);

    REQUIRE((f << 1 << 2 << 3)() == 6);
    REQUIRE((f << 1 << 2)(3) == 6);
    REQUIRE((f << 1)(2, 3) == 6);
}

TEST_CASE("Func.Currying.Operator << Syntax and STL integration", "[ok]")
{
    auto f = $(sum);

    auto v = std::vector<int>{ 1,2,3 };
    std::transform(std::begin(v), std::end(v),
        std::begin(v),
        $(times) << 2);
    REQUIRE(v[0] == 2);
    REQUIRE(v[1] == 4);
    REQUIRE(v[2] == 6);
}

TEST_CASE("Func.Pipeline.Simple Call", "[ok]")
{
    auto p1 = $$($(sum) << 1 << 2, $(times) << 2);
    REQUIRE(p1(4) == 14);
}

TEST_CASE("Func.Pipeline.STL Integration", "[ok]")
{
    auto v = std::vector<int>{ 1,2,3 };

    std::transform(std::begin(v), std::end(v),
        std::begin(v),
        $$($(sum) << 1 << 2, $(times) << 2));
    REQUIRE(v[0] == 8);
    REQUIRE(v[1] == 10);
    REQUIRE(v[2] == 12);
}

TEST_CASE("Func.Performance.Should not be slower than manual code", "[ok]")
{
    #ifdef NDEBUG
    using namespace std;

    random_device rnd_device;
    mt19937 mersenne_engine{ rnd_device() };
    uniform_int_distribution<int> dist{ 1, 52 };
    auto gen = [&dist, &mersenne_engine]() { return dist(mersenne_engine); };
    vector<int> vec(3);

    std::clock_t    start;
    start = std::clock();
    /* MANUAL CODE */
    auto r = true;
    for (int i = 0; i < 10000000; ++i)
    {
        generate(begin(vec), end(vec), gen);
        auto expected = sum(vec[0], vec[1], vec[2]);

        r &= (sum(vec[0], vec[1], vec[2]) == expected);
    }
    /* MANUAL CODE */
    auto manualTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
    

    start = std::clock();
    /* FUNC CODE */
    auto rr = true;
    for (int i = 0; i < 10000000; ++i)
    {
        generate(begin(vec), end(vec), gen);
        auto expected = sum(vec[0], vec[1], vec[2]);

        auto f = $(sum) << vec[0] << vec[1] << vec[2];
        rr &= (f() == expected);
    }
    /* FUNC CODE */
    auto funcTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);


    std::cout << "manual: " << manualTime 
        << ", func: " << funcTime 
        << " (func/manual = " << (float)funcTime / (float)manualTime << ")" << std::endl;
    REQUIRE(r == rr);
    
    REQUIRE(funcTime < (manualTime * 1.05)); // Func cannot be 5% slower than manual code
    #endif
}

class NotificationSender
{
    virtual void Sender() = 0;
};

class SomeController
{
    NotificationSender* sender;
    SomeController() {}
public:
    SomeController build()
    {
        return {};
    }
};

TEST_CASE("Func.DependencyInjection", "[ok]")
{
    auto f = $(&SomeController::build);
}