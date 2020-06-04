#pragma once
#include <cmath>
#include <algorithm>

namespace roots
{

    constexpr float pif = 3.141592653589793238462643383279502884197169399375105820974f;
    constexpr double pid = 3.141592653589793238462643383279502884197169399375105820974;
    constexpr long double pild = 3.141592653589793238462643383279502884197169399375105820974;

    template <typename T>
    bool is_zero(T v, T tolerance = 0.001)
    {
        return (v >= -tolerance) && (v <= tolerance);
    }

    template <typename T>
    bool is_equal(T a, T b, T tolerance = 0.001)
    {
        return is_zero(std::abs(a - b), tolerance);
    }

    template <typename T, typename F, typename FPrime>
    T newtonRaphson(T x, F f, FPrime fprime, int steps = 100, T tolerance = 0.0001)
    {
        T h = f(x) / fprime(x);
        while (steps > 0 && std::abs(h) >= tolerance)
        {
            h = f(x) / fprime(x);
            x -= h;
            // std::cout << x << " " << f(x) / fprime(x) << std::endl;
            --steps;
        }
        return x;
    }

    template <typename T>
    bool has_solution_quadratic(T a, T b, T c)
    {
        if (std::isnan(a) || std::isnan(b) || std::isnan(c))
            return false;
        if (std::isinf(a) || std::isinf(b) || std::isinf(c))
            return false;

        if (a == 0)
            return false;
        else
        {
            auto delta = b * b - 4 * a * c;
            return delta >= 0;
        }
    }

    template <typename T>
    bool has_solution_cubic(T a, T b, T c, T d)
    {
        if (std::isnan(a) || std::isnan(b) || std::isnan(c))
            return false;
        if (std::isinf(a) || std::isinf(b) || std::isinf(c))
            return false;

        if (a == 0)
            return false;
        else
        {
            auto olda = a;
            a = b / olda;
            b = c / olda;
            c = d / olda;

            T Q = (a * a - 3 * b) / (T)9.0;
            T R = (2 * a * a * a - 9 * a * b + 27 * c) / (T)54.0;

            T R2 = R * R;
            T Q3 = Q * Q * Q;
            if (R2 < Q3)
                return true;
            else
                return false;
        }
    }

    template <typename T>
    struct root
    {
        bool is_imag;
        T real;
        T imag;
    };

    template <typename T, unsigned int SIZE>
    struct roots
    {
        bool ok;
        root<T> r[SIZE];
    };

    template <typename T>
    auto evaluate(T x, T a, T b, T c, T d)
    {
        struct evaluate_result
        {
            T Q, dQ, B1, C2;
        };
        T q0 = a * x;
        T B1 = q0 + b;
        T C2 = B1 * x + c;
        return evaluate_result{C2 * x + d, (q0 + B1) * x + C2, B1, C2};
    }

    // Numerics Recipes in C
    // Chapter 5. Evaluation of Functions page 184
    //
    // To Solve a Real Cubic Equation
    // https://people.eecs.berkeley.edu/~wkahan/Math128/Cubic.pdf
    // page 6
    template <typename T>
    roots<T, 2> solve_quadratic(T a, T b, T c)
    {
        if (is_zero(a) && is_zero(b) && is_zero(c))
            return {false, {{}, {}}};

        T neg_half_b = -b / 2;
        T quarter_delta = neg_half_b * neg_half_b - a * c;
        if (quarter_delta < 0)
        {
            T X = neg_half_b / a;
            T Y = std::sqrt(-quarter_delta) / a;
            return {true, {{true, X, Y}, {true, X, -Y}}};
        }
        else
        {
            T r = neg_half_b + std::copysign(std::sqrt(quarter_delta), b);
            if (r == 0)
                return {true, {{false, c / a, 0}, {false, -c / a, 0}}};
            else
                return {true, {{false, c / r, 0}, {false, r / a, 0}}};
        }
    }

    // Numerics Recipes in C
    // Chapter 5. Evaluation of Functions page 184
    // This equation first appears in Chapter VI of
    // Francois Viète’s treatise "De emendatione," published in 1615!
    template <typename T>
    roots<T, 3> solve_cubic(T A, T B, T C, T D)
    {
        T X, quadratic_a, quadratic_b, quadratic_c;

        roots<T, 3> roots;
        if (A == 0)
        {
            quadratic_a = B;
            quadratic_b = C;
            quadratic_c = D;
        }
        else if (D == 0)
        {
            X = 0;
            quadratic_a = A;
            quadratic_b = B;
            quadratic_c = C;
            roots.r[0] = {false, X, 0};
        }
        else
        {
            quadratic_a = A;
            X = -(B / A) / 3;

            auto evalInfo = evaluate(X, A, B, C, D);
            T q = evalInfo.Q;
            T dq = evalInfo.dQ;
            quadratic_b = evalInfo.B1;
            quadratic_c = evalInfo.C2;

            T t = q / A;
            T r = std::pow(std::abs(t), 1 / 3);
            t = -dq / A;
            if (t > 0)
            {
                r = 1.324717957244746 * std::max(r, std::sqrt(t));
            }
            T s = std::copysign(1, t);
            T x0 = X - s * r;
            if (x0 != X)
            {
                T EPS = 1e-10;
                T den = 1 + (100 * EPS);
                do
                {
                    X = x0;
                    evalInfo = evaluate(X, A, B, C, D);
                    q = evalInfo.Q;
                    dq = evalInfo.dQ;
                    quadratic_b = evalInfo.B1;
                    quadratic_c = evalInfo.C2;
                    x0 = (dq == 0 ? X : X - (q / dq) / den);
                } while (s * x0 > s * X);

                if (std::abs(A) * X * X > std::abs(D / X))
                {
                    quadratic_c = -D / X;
                    quadratic_b = (quadratic_c - C) / X;
                }
            }
            roots.r[0] = {false, X, 0};
        }

        auto qroots = solve_quadratic(quadratic_a, quadratic_b, quadratic_c);
        roots.r[1] = qroots.r[0];
        roots.r[2] = qroots.r[1];
        return roots;
    };

    template <typename T, unsigned int N>
    struct polynomial
    {
        T coeff[N + 1];
        roots<T, N> r;

        template <typename... Ts>
        polynomial(Ts... coeffs) : coeff{coeffs...}
        {
        }

        roots<T, N> &solve()
        {
            if constexpr (N == 2)
                r = solve_quadratic(coeff[0], coeff[1], coeff[2]);
            else if constexpr (N == 3)
                r = solve_cubic(coeff[0], coeff[1], coeff[2], coeff[3]);
            return r;
        }

        T eval(T v) const
        {
            T accum = 0.0;
            for (auto i = 0; i <= N; ++i)
                accum += coeff[i] * std::pow(v, N - i);
            return accum;
        }

        T derivative_eval(T v) const
        {
            T accum = 0.0;
            for (auto i = 0; i <= N - 1; ++i)
                accum += (N - i) * coeff[i] * std::pow(v, N - i - 1);
            return accum;
        }

        void try_improve_root(int i, size_t steps = 100, T tolerance = 0.0001)
        {
            r.r[i].real = newtonRaphson(
                r.r[i].real,
                [&](auto x) { return eval(x); },
                [&](auto x) { return derivative_eval(x); },
                steps, tolerance);
        }

        T max_abs_coeff() const
        {
            T maxabs = 0;
            for (size_t i = 0; i < N + 1; ++i)
            {
                maxabs = std::max(maxabs, std::abs(coeff[i]));
            }
            return maxabs;
        }

        struct check_roots_result
        {
            bool all_ok;
            bool ok[N];
            T error[N];
            T max_error;
        };
        check_roots_result check_roots(T tolerance = 0.01) const
        {
            auto maxcoeff = max_abs_coeff();

            auto c = check_roots_result{};
            c.max_error = 0;
            c.all_ok = true;
            for (auto i = 0; i < N; ++i)
            {
                if (!r.r[i].is_imag)
                {
                    auto e = eval(r.r[i].real);
                    c.error[i] = std::abs(e / maxcoeff);
                    c.max_error = std::max(c.max_error, c.error[i]);
                    c.all_ok &= (c.ok[i] = is_zero(c.error[i], tolerance));
                }
                else
                {
                    c.ok[i] = true;
                }
            }
            return c;
        }
    };

} // namespace roots
