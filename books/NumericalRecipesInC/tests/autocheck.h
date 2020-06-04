#pragma once
#include <string>
#include <iostream>
#include <functional>
#include <tuple>
#include <algorithm>
#include <complex>

#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#include "../src/roots.h"

#include <autocheck/autocheck.hpp>

class catch_reporter : public autocheck::catch_reporter
{
    std::string name;

public:
    catch_reporter(const std::string &name) : name{name} {}

    virtual void success(size_t tests, size_t max_tests,
                         size_t trivial, autocheck::distribution &&dist) const
    {
        autocheck::report_success(std::clog, tests, max_tests, trivial, std::move(dist));
        REQUIRE(true);
    }

    virtual void failure(size_t tests, const char *reason) const
    {
        std::ostringstream out;
        autocheck::report_failure(out, tests, reason);
        FAIL(name + "-" + out.str());
    }
};

template <typename T>
autocheck::generator<T> genenator()
{
    return {};
}
auto any_int() { return genenator<int>(); }
auto any_float() { return genenator<float>(); }

template <typename TProp, typename TGen1>
void check(const std::string &name, const TGen1 &gen1, const TProp &prop, size_t size = 100)
{
    catch_reporter rep{name};
    auto arb = autocheck::make_arbitrary(gen1);
    autocheck::check<typename TGen1::result_type>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2, typename TGen3>
void check(const std::string &name, const TGen1 &gen1, const TGen2 &gen2, const TGen3 &gen3, const TProp &prop, size_t size = 100)
{
    catch_reporter rep{name};
    auto arb = autocheck::make_arbitrary(gen1, gen2, gen3);

    autocheck::check<
        typename TGen1::result_type,
        typename TGen2::result_type,
        typename TGen3::result_type>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2, typename TGen3, typename TDiscard>
void check_if(const std::string &name,
              const TGen1 &gen1, const TGen2 &gen2, const TGen3 &gen3,
              const TDiscard &discard,
              const TProp &prop, size_t size = 100)
{
    catch_reporter rep{name};
    auto arb = autocheck::make_arbitrary(gen1, gen2, gen3)
                   .discard_if(discard);

    autocheck::check<
        typename TGen1::result_type,
        typename TGen2::result_type,
        typename TGen3::result_type>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2, typename TGen3, typename TGen4, typename TDiscard>
void check_if(const std::string &name,
              const TGen1 &gen1, const TGen2 &gen2, const TGen3 &gen3, const TGen4 &gen4,
              const TDiscard &discard,
              const TProp &prop, size_t size = 100)
{
    catch_reporter rep{name};
    auto arb = autocheck::make_arbitrary(gen1, gen2, gen3, gen4)
                   .discard_if(discard);

    autocheck::check<
        typename TGen1::result_type,
        typename TGen2::result_type,
        typename TGen3::result_type,
        typename TGen4::result_type>(prop, size, arb, rep);
}

template <typename T>
void collect_worst(T &worst, polynomial<T, 2> &eq)
{
    auto max_error = std::max(
        std::abs(eq.eval(eq.r.r[0].real)),
        std::abs(eq.eval(eq.r.r[1].real)));
    auto max_coeff = std::abs(eq.max_coeff());
    // std::cout << max_error << " " << max_coeff << std::endl;
    worst = std::max(worst, max_error / max_coeff);
}

template <typename T>
void collect_worst(T &worst, polynomial<T, 3> &eq)
{
    auto max_error = std::max({std::abs(eq.eval(eq.r.r[0].real)),
                               std::abs(eq.eval(eq.r.r[1].real)),
                               std::abs(eq.eval(eq.r.r[2].real))});
    auto max_coeff = std::abs(eq.max_coeff());
    // std::cout << max_error << " " << max_coeff << std::endl;
    worst = std::max(worst, max_error / max_coeff);
}

template <typename T>
bool require_is_zero(T v, T tolerance = 0.000001)
{
    auto r = is_zero(v, tolerance);
    CHECK(r);

    return r;
}
