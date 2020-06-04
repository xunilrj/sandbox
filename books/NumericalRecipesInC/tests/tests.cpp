#include "autocheck.h"

TEST_CASE("math.roots.quadratic.KahanMethod", "[ok]")
{
    auto eq2 = polynomial<float, 2>{0.279144f, -43.8352f, -3.27402f};
    eq2.solve();

    check_if(
        "Kahan, all solvable quadratic", any_float(), any_float(), any_float(),
        [&](auto &&a, auto &&b, auto &&c) { return !has_solution_quadratic(a, b, c); },
        [&](auto &&a, auto &&b, auto &&c) {
            polynomial<float, 2> eq = {a, b, c};
            eq.solve();
            auto check = eq.check_roots();

            if (!check.all_ok)
            {
                std::cout << "nok " << check.max_error << std::endl;
                std::cout << eq.max_abs_coeff() << std::endl;
                std::cout << eq.coeff[0] << " " << eq.coeff[1] << " " << eq.coeff[2] << std::endl;
                std::cout << eq.r.r[0].is_imag << ":" << eq.eval(eq.r.r[0].real) << " " << eq.r.r[0].is_imag << ":" << eq.eval(eq.r.r[1].real) << std::endl;
            }

            return check.all_ok;
        });
}

TEST_CASE("math.roots.cubic.KahanMethod", "[ok]")
{

    check_if(
        "Kahan, all solvable cubics", any_float(), any_float(), any_float(), any_float(),
        [&](auto &&a, auto &&b, auto &&c, auto &&d) {
            return !has_solution_cubic(a, b, c, d);
        },
        [&](auto &&a, auto &&b, auto &&c, auto &&d) {
            polynomial<float, 3> eq = {a, b, c, d};
            eq.solve();
            auto check = eq.check_roots();

            if (!check.all_ok)
            {
                std::cout << "nok " << check.max_error << std::endl;
                std::cout << eq.max_abs_coeff() << std::endl;
                std::cout << eq.coeff[0] << " " << eq.coeff[1] << " " << eq.coeff[2] << std::endl;
                std::cout << eq.r.r[0].is_imag << ":" << eq.eval(eq.r.r[0].real) << " " << eq.r.r[0].is_imag << ":" << eq.eval(eq.r.r[1].real) << std::endl;
            }

            return check.all_ok;
        });
}