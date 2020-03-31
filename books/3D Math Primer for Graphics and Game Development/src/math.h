#pragma once
#include <type_traits>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <array>
#include <ostream>
#include <vector>
#include <unordered_map>

namespace math
{
#ifndef DEFAULT_FLOAT
#define DEFAULT_FLOAT f32
#endif

    template<typename T>
    using EnableIfArithmetic = std::enable_if_t<std::is_arithmetic_v<T>>;


    #define ENABLE_FOR_NUMBERS typename = std::enable_if_t<std::is_floating_point_v<TNumber> || std::is_integral_v<TNumber>>

    template <typename T, size_t N>
    class make_tuple
    {
        template<size_t> using T_ = T;
        template<size_t... Is> static auto gen(std::index_sequence<Is...>) { return std::tuple<T_<Is>...>{}; }
        static auto gen() { return make_tuple<T, N>::gen(std::make_index_sequence<N>{}); }
    public:
        using type = decltype(gen());
    };
    template <typename T, size_t N>
    using make_tuple_t = typename make_tuple<T, N>::type;

    constexpr float pif = 3.141592653589793238462643383279502884197169399375105820974f;
    constexpr double pid = 3.141592653589793238462643383279502884197169399375105820974;
    constexpr long double pild = 3.141592653589793238462643383279502884197169399375105820974;
    struct AbsoluteRelativeComparison
    {
        // Real-Time Collision Detection
        // Chapter 11
        //http://realtimecollisiondetection.net/blog/?p=89
        //https://www.itu.dk/~sestoft/bachelor/IEEE754_article.pdf
        static bool equals(float x, float y, float absTol = 0.0001f, float relTol = 0.0001f)
        {
            return (std::abs(x - y) <= std::max(absTol, relTol * std::max(std::abs(x), std::abs(y))));
        }

        static bool absTolEqual(float x, float y, float absTol = 0.0001f)
        {
            return std::abs(x - y) <= absTol; // Absolute tolerance comparison
        }

        static bool relTolQeual(float x, float y, float relTol = 0.0001f)
        {
            return std::abs(x - y) <= relTol * std::max(1.0f, std::max(std::abs(x), std::abs(y))); // Relative tolerance comparison
        }
    };

    class f32
    {
        float value;
    public:
        using T = float;
        f32() : value{ 0 } {}
        f32(const float v) : value{ v } {}

        f32 operator - (const f32 v) const { return { value - v.value }; }
        f32 operator + (const f32 v) const { return { value + v.value }; }
        f32 operator * (const f32 v) const { return { value * v.value }; }
        f32 operator / (const f32 v) const { return { value / v.value }; }

        template <typename TNumber> f32 operator - (const TNumber v) const { return { value - v }; }
        template <typename TNumber> f32 operator + (const TNumber v) const { return { value + v }; }
        template <typename TNumber, typename = EnableIfArithmetic<TNumber>> f32 operator * (const TNumber v) const { return { value * v }; }
        template <typename TNumber> f32 operator / (const TNumber v) const { return { value / v }; }
        template <typename TNumber> f32& operator *= (const TNumber v) { value *= v; return *this; }
        template <typename TNumber> f32& operator /= (const TNumber v) { value /= v; return *this; }
        template <typename TNumber> f32& operator += (const TNumber v) { value += v; return *this; }
        template <typename TNumber> f32& operator -= (const TNumber v) { value -= v; return *this; }

        bool operator == (const f32 b) const { return AbsoluteRelativeComparison::equals(value, b.value); }

        f32 operator - () const { return { -value }; }
        operator float() const { return value; }

        template <typename TNumber> f32& operator += (const f32 v) { value += v.value; return *this; }
        template <typename TNumber> f32& operator -= (const f32 v) { value -= v.value; return *this; }
        template <typename TNumber> f32& operator *= (const f32 v) { value *= v.value; return *this; }
        template <typename TNumber> f32& operator /= (const f32 v) { value /= v.value; return *this; }

        friend std::ostream& operator<<(std::ostream& out, f32 v);
        friend bool operator == (float a, f32 b);
        friend bool operator == (f32 a, float b);
        friend bool operator == (int a, f32 b);
        friend bool operator == (f32 a, int b);

        void swap(f32& other) {
            std::swap(value, other.value);
        }
    };

    struct FloatComparison
    {
        static bool lessThanZero(float a)
        {
            return a < -0.0001;
        }

        static bool moreThanOne(float a)
        {
            return a > 0.9999;
        }

        static bool willCauseDivProblem(float a)
        {
            return a >= -0.0001 && a <= 0.0001;
        }
    };
    


    bool operator == (const float a, const f32 b) { return AbsoluteRelativeComparison::equals(a, b.value); }
    bool operator == (const f32 a, const float b) { return AbsoluteRelativeComparison::equals(a.value, b); }
    bool operator == (const int a, const f32 b) { return AbsoluteRelativeComparison::equals((float)a, b.value); }
    bool operator == (const f32 a, const int b) { return AbsoluteRelativeComparison::equals(a.value, (float)b); }

    std::ostream& operator<<(std::ostream& out, math::f32 v)
    {
        return out << v.value;
    }


    template <size_t D, typename T = float> struct named_store { T data[D]; };
    template <typename T> struct named_store<2, T> { union { T data[2]; struct { T x, y; }; }; };
    template <typename T> struct named_store<3, T> { union { T data[3]; struct { T x, y, z; }; }; };
    template <typename T> struct named_store<4, T> { union { T data[4]; struct { T x, y, z, w; }; }; };

   
    // DECLARATIONS

    template <typename T> struct base_quat;
    using quat = base_quat<DEFAULT_FLOAT>;

    template <typename T, size_t D, size_t... Is> struct AABB;
    using AABB3 = AABB<DEFAULT_FLOAT, 3, 0, 1, 2>;

    // VECTORS

    template <typename TStore, typename Types, typename Indices>
    struct base_vector {};

    template <
        typename T,
        typename... Ts,
        size_t... Is>
        struct base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>
        : public named_store<sizeof...(Ts), T>
    {
        using TVECTOR = base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>;
        template <size_t D>
        using TDVECTOR = base_vector<T,
            make_tuple_t<T, D>,
            std::make_index_sequence<D>>;

        static TVECTOR zero() { return { static_cast<Ts>(0)... }; }
        template <size_t N>
        static TVECTOR e() { return { static_cast<Ts>(Is == N ? 1 : 0)... }; }

        auto operator == (const TVECTOR& r) const
        {
            return ((this->data[Is] == r.data[Is]) && ...);
        }

        base_vector(const Ts&&... args) : named_store<sizeof...(Ts), T>{ static_cast<T>(args)... }
        {
        }

        T& operator [] (size_t i) { return this->data[i]; }
        const T& operator [] (size_t i) const { return this->data[i]; }

        template <size_t DNORM = 2> T norm2() const { return 0; /*TODO*/ }
        template <> T norm2<2>() const
        {
            return ((this->data[Is] * this->data[Is]) + ...);
        }

        template <size_t DNORM = 2> T norm() const { return 0; /*TODO*/ }
        template <> T norm<2>() const
        {
            return std::sqrt(norm2<2>());
        }

        template <size_t DNORM = 2> TVECTOR& normalize()
        {
            auto n = norm<DNORM>();
            if (n == 0) return *this;

            ((this->data[Is] /= n), ...);
            return *this;
        }
        template <size_t DNORM = 2> TVECTOR normalized() const
        {
            auto n = norm<DNORM>();
            if (n == 0) return *this;

            return { this->data[Is] / n... };
        }

        TVECTOR operator -() const
        {
            return { -this->data[Is]... };
        }

        template <typename TNumber/*, typename = EnableIfArithmetic<TNumber>*/>
        TVECTOR operator *(const TNumber& f) const
        {
            return { this->data[Is] * f... };
        }

        template <typename TNumber/*, typename = EnableIfArithmetic<TNumber>*/>
        TVECTOR operator /(const TNumber f) const
        {
            return { this->data[Is] / f... };
        }

        template <typename TNumber/*, typename = EnableIfArithmetic<TNumber>*/>
        TVECTOR operator *=(const TNumber f)
        {
            ((this->data[Is] *= f), ...);
            return *this;
        }

        template <typename TNumber/*, typename = EnableIfArithmetic<TNumber>*/>
        TVECTOR operator /=(const TNumber f)
        {
            ((this->data[Is] /= f), ...);
            return *this;
        }

        TVECTOR operator +(const TVECTOR& r) const
        {
            return { this->data[Is] + r.data[Is]... };
        }

        TVECTOR operator -(const TVECTOR& r) const
        {
            return { this->data[Is] - r.data[Is]... };
        }

        TVECTOR& operator +=(const TVECTOR& r)
        {
            ((this->data[Is] += r.data[Is]), ...);
            return *this;
        }

        TVECTOR& operator -=(const TVECTOR& r)
        {
            ((this->data[Is] -= r.data[Is]), ...);
            return *this;
        }

        T operator * (const TVECTOR& r) const { return dot(r); }
        T dot(const TVECTOR& r) const
        {
            return ((this->data[Is] * r.data[Is]) + ...);
        }

        template<size_t D = sizeof...(Ts), typename = std::enable_if_t<D == 3>>
        TVECTOR operator % (const TVECTOR& b) const
        {
            return {
                this->y * b.z - this->z * b.y,
                this->z * b.x - this->x * b.z,
                this->x * b.y - this->y * b.x
            };
        }

        template <typename F>
        auto all_of(F f) const
        {
            return true && (f(this->data[Is]) && ...);
        }

        template <typename F>
        auto any_of(F f) const
        {
            return true && (f(this->data[Is]) || ...);
        }

        template <typename F>
        auto none_of(F f) const
        {
            return true && !(f(this->data[Is]) || ...);
        }

        //TODO: should be private
        template <typename TAny>
        auto ltimes(const TAny& l) const { return (*this) * l; }

        template<size_t D = sizeof...(Ts), typename = std::enable_if_t<D >= 3>>
        TDVECTOR<3> xyz () const
        {
            return { (T)this->data[0], (T)this->data[1], (T)this->data[2] };
        }
    };

    template <
        typename TNumber,
        typename T,
        typename... Ts,
        size_t... Is>
        auto operator * (const TNumber& l, const base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>& r)
    {
        return r.ltimes(l);
    }


    template <typename T, size_t D>
    using vector = base_vector<T,
        make_tuple_t<T, D>,
        std::make_index_sequence<D>>;

    template<typename T, size_t> using id_t = T;
    template<size_t> struct zero_v { static constexpr int value = 0; };

    using vec2 = vector<DEFAULT_FLOAT, 2>;
    using vec3 = vector<DEFAULT_FLOAT, 3>;
    using vec4 = vector<DEFAULT_FLOAT, 4>;

    std::ostream& operator<< (std::ostream& out, const vec4& v)
    {
        return out << "[" << v.x << ";" << v.y << ";" << v.z << ";" << v.w << "]";
    }

    template <size_t D, typename T = DEFAULT_FLOAT>
    constexpr auto zeros() { return vector<T, D>::zero(); }

    // MATRICES

    template <typename T, size_t H, size_t W, typename Indices>
    struct base_matrix { };

    struct all_elements {};
    struct lower_triangular_elements {};
    struct main_diagonal_elements {};

    template <typename T, size_t N, size_t... Is, size_t... AIs>
    struct base_matrix<T, N, N, std::tuple<std::index_sequence<Is...>, std::index_sequence<AIs...>>>
    {
        using TMAT = base_matrix<T, N, N, std::tuple<std::index_sequence<Is...>, std::index_sequence<AIs...>>>;

        T data[N * N];

        base_matrix(id_t<T,AIs>... args) : data{ args... }
        {
        }

        static TMAT id()
        {
            TMAT m = { zero_v<AIs>::value... };
            m.for_each(main_diagonal_elements{}, [](auto i, auto j, auto& x) { x = 1; });
            return m;
        }

        static TMAT diagonal(id_t<T, Is>... args)
        {
            TMAT m = { zero_v<AIs>::value... };
            ((
                m(Is, Is) = args
            ), ...);
            return m;
        }

        static TMAT scale(id_t<T, Is>... args)
        {
            TMAT m = { zero_v<AIs>::value... };
            ((
                m(Is, Is) = args
                ), ...);
            return m;
        }

        static TMAT translation(id_t<T, Is>... args)
        {
            TMAT m = id();
            ((
                m.data[N*N - N + Is] = args
            ), ...);
            return m;
        }

        template<typename = std::enable_if_t<N == 4>>
        static TMAT rotation(const vector<T,3>& n, float theta)
        {
            auto nhat = n.normalized();
            auto nx = nhat.x, ny = nhat.y, nz = nhat.z;
            auto nx2 = nhat.x * nhat.x;
            auto ny2 = nhat.y * nhat.y;
            auto nz2 = nhat.z * nhat.z;
            auto costheta = std::cos(theta);
            auto sintheta = std::sin(theta);
            /*
                nx2 + (1 - nx2) * costheta,                     -nx * ny * costheta + nx * ny + nz * sintheta,  -nx * nz * costheta + nx * nz - ny * sintheta,  0
                -nx * ny * costheta + nx * ny - nz * sintheta,  ny2 + (1 - ny2) * costheta,                     nx * sintheta - ny * nz * costheta + ny * nz,   0
                -nx * nz * costheta + nx * nz + ny * sintheta,  -nx * sintheta - ny * nz * costheta + ny * nz,  nz2 + (1 - nz2) * costheta,                     0
                0,                                              0,                                              0,                                              1
            */
            TMAT m = {
                nx2 + (1 - nx2) * costheta,
                -nx * ny * costheta + nx * ny - nz * sintheta,
                -nx * nz * costheta + nx * nz + ny * sintheta,
                0,
                -nx * ny * costheta + nx * ny + nz * sintheta,
                ny2 + (1 - ny2) * costheta,
                -nx * sintheta - ny * nz * costheta + ny * nz,
                0,
                -nx * nz * costheta + nx * nz - ny * sintheta,
                nx * sintheta - ny * nz * costheta + ny * nz,
                nz2 + (1 - nz2) * costheta,
                0,
                0,
                0,
                0,
                1,
            };
            return m;
        }

        T& operator () (size_t i, size_t j)
        {
            return data[j * N + i];
        }

        const T& operator () (size_t i, size_t j) const
        {
            return data[j * N + i];
        }

        auto operator == (const TMAT& r) const
        {
            return (operator_equal_row<Is>(r, std::make_index_sequence<N>{}) && ...);
        }

        

        auto operator * (const TMAT& r) const
        {
            TMAT m;
            mul(m, *this, r);
            return m;
        }

        auto operator * (const vector<T, N>& r) const
        {
            return vector<T, N> { matvec(Is, r)... };
        }

        template<typename TNumber, typename = std::enable_if_t<N == 4>>
        AABB<TNumber, N-1, 0, 1, 2> operator * (const AABB<TNumber, N-1, 0, 1, 2>& box) const
        {
            auto& m = *this;

            auto max = vector<T,3>{ (T)m(0,3), (T)m(1,3), (T)m(2,3) };
            auto min = max;

            auto m11 = m(1, 1); auto m12 = m(1, 2); auto m13 = m(1, 3);
            auto m21 = m(2, 1); auto m22 = m(2, 2); auto m23 = m(2, 3);
            auto m31 = m(3, 1); auto m32 = m(3, 2); auto m33 = m(3, 3);

            auto m11gtZero = m11 > 0.0f;
            auto m21gtZero = m21 > 0.0f;
            auto m31gtZero = m31 > 0.0f;
            auto m12gtZero = m12 > 0.0f;
            auto m22gtZero = m22 > 0.0f;
            auto m32gtZero = m32 > 0.0f;
            auto m13gtZero = m13 > 0.0f;
            auto m23gtZero = m23 > 0.0f;
            auto m33gtZero = m33 > 0.0f;

            min.x += m11 * (m11gtZero ? box.min.x : box.max.x);
            max.x += m11 * (m11gtZero ? box.max.x : box.min.x);

            min.y += m21 * (m21gtZero ? box.min.x : box.max.x);
            max.y += m21 * (m21gtZero ? box.max.x : box.min.x);

            min.z += m31 * (m31gtZero ? box.min.x : box.max.x);
            max.z += m31 * (m31gtZero ? box.max.x : box.min.x);

            min.x += m12 * (m12gtZero ? box.min.x : box.max.x);
            max.x += m12 * (m12gtZero ? box.max.x : box.min.x);

            min.y += m22 * (m22gtZero ? box.min.x : box.max.x);
            max.y += m22 * (m22gtZero ? box.max.x : box.min.x);

            min.z += m32 * (m32gtZero ? box.min.x : box.max.x);
            max.z += m32 * (m32gtZero ? box.max.x : box.min.x);

            min.x += m13 * (m13gtZero ? box.min.x : box.max.x);
            max.x += m13 * (m13gtZero ? box.max.x : box.min.x);

            min.y += m23 * (m23gtZero ? box.min.x : box.max.x);
            max.y += m23 * (m23gtZero ? box.max.x : box.min.x);

            min.z += m33 * (m33gtZero ? box.min.x : box.max.x);
            max.z += m33 * (m33gtZero ? box.max.x : box.min.x);

            return AABB<T,N-1, 0, 1, 2>{ min, max };
        }

        template <typename TNumber, ENABLE_FOR_NUMBERS>
        auto operator * (const TNumber& n) const
        {
            TMAT m = *this;
            m.for_each([&](auto i, auto j, T& x) { x *= n; });
            return m;
        }

        auto operator - (const TMAT& r) const
        {
            TMAT m = *this;
            ((m.data[Is] -= r.data[Is]), ...);
            return m;
        }

        static void mul(TMAT& r, const TMAT& a, const TMAT& b)
        {
            for (auto i = 0; i < N; i++) {
                auto jN = 0;
                for (auto j = 0; j < N; j++, jN += N) {
                    r.data[jN + i] += ((a.data[Is * N + i] * b.data[jN + Is]) + ...);
                }
            }
        }

        TMAT transpose()
        {
            for_each(lower_triangular_elements{}, [&](auto i, auto j, T& x)
                {
                    std::swap(x, (*this)(j, i));
                });
            return *this;
        }

        auto transposed() const
        {
            TMAT m = *this;
            return m.transpose();
        }

        template <typename F>
        void for_each(F f)
        {
            for_each(all_elements{}, f);
        }

        template <typename F, typename Type>
        void for_each(Type t, F f)
        {
            if constexpr( std::is_same_v<Type, all_elements> )
                (for_each_row<F, Is, 0>(f, std::make_index_sequence<N>{}), ...);
            else if constexpr (std::is_same_v<Type, lower_triangular_elements>)
                (for_each_row<F, Is, Is>(f, std::make_index_sequence<N - Is>{}), ...);
            else if constexpr (std::is_same_v<Type, main_diagonal_elements>)
            {
                ((
                    f(Is, Is, (*this)(Is, Is))
                ), ...);
            }
        }

        template <typename F>
        auto all_of(F f) const
        {
            return (f(data[Is]) && ...);
        }

        template <typename TNumber>
        auto ltimes (const TNumber& l) const
        {
            TMAT m = *this;
            m.for_each([&](auto i, auto j, T& x) { x = l * x; });
            return m;
        }

        //TODO JUST COPIED REVIEW AND UNDERSTAND
        auto lu() const
        {
            TMAT l, u;
            int i = 0, j = 0, k = 0;
            for (i = 0; i < N; i++) {
                for (j = 0; j < N; j++) {
                    if (j < i)
                        l(j,i) = 0;
                    else {
                        l(j,i) = (*this)(j,i);
                        for (k = 0; k < i; k++) {
                            l(j,i) = l(j,i) - l(j,k) * u(k,i);
                        }
                    }
                }
                for (j = 0; j < N; j++) {
                    if (j < i)
                        u(i,j) = 0;
                    else if (j == i)
                        u(i,j) = 1;
                    else {
                        u(i,j) = (*this)(i,j) / l(i,i);
                        for (k = 0; k < i; k++) {
                            u(i,j) = u(i,j) - ((l(i,k) * u(k,j)) / l(i,i));
                        }
                    }
                }
            }
            return std::tuple{ l,u };
        }

        T determinant() const
        {
            auto [l, u] = lu();
            auto d = 1;
            //TODO aggregate/fold here
            u.for_each(main_diagonal_elements{}, [&](auto&&i, auto&&j, auto&&x)
            {
                d *= x;
            });
            return d;
        }

        //TODO COPIED FROM GLU
        template<typename = std::enable_if_t<N == 4>>
        std::tuple<bool,TMAT> inverse() const
        {
            TMAT r;
            auto& inv = r.data;
            auto& m = this->data;

            T det;

            inv[0] = m[5] * m[10] * m[15] - m[5] * m[11] * m[14] - m[9] * m[6] * m[15] +  m[9] * m[7] * m[14] + m[13] * m[6] * m[11] -  m[13] * m[7] * m[10];
            inv[4] = -m[4] * m[10] * m[15] + m[4] * m[11] * m[14] + m[8] * m[6] * m[15] - m[8] * m[7] * m[14] - m[12] * m[6] * m[11] +  m[12] * m[7] * m[10];
            inv[8] = m[4] * m[9] * m[15] - m[4] * m[11] * m[13] - m[8] * m[5] * m[15] + m[8] * m[7] * m[13] + m[12] * m[5] * m[11] - m[12] * m[7] * m[9];
            inv[12] = -m[4] * m[9] * m[14] + m[4] * m[10] * m[13] + m[8] * m[5] * m[14] - m[8] * m[6] * m[13] - m[12] * m[5] * m[10] + m[12] * m[6] * m[9];
            inv[1] = -m[1] * m[10] * m[15] + m[1] * m[11] * m[14] + m[9] * m[2] * m[15] - m[9] * m[3] * m[14] - m[13] * m[2] * m[11] + m[13] * m[3] * m[10];
            inv[5] = m[0] * m[10] * m[15] - m[0] * m[11] * m[14] - m[8] * m[2] * m[15] + m[8] * m[3] * m[14] + m[12] * m[2] * m[11] - m[12] * m[3] * m[10];
            inv[9] = -m[0] * m[9] * m[15] + m[0] * m[11] * m[13] + m[8] * m[1] * m[15] - m[8] * m[3] * m[13] - m[12] * m[1] * m[11] + m[12] * m[3] * m[9];
            inv[13] = m[0] * m[9] * m[14] - m[0] * m[10] * m[13] - m[8] * m[1] * m[14] + m[8] * m[2] * m[13] + m[12] * m[1] * m[10] - m[12] * m[2] * m[9];
            inv[2] = m[1] * m[6] * m[15] - m[1] * m[7] * m[14] - m[5] * m[2] * m[15] + m[5] * m[3] * m[14] + m[13] * m[2] * m[7] - m[13] * m[3] * m[6];
            inv[6] = -m[0] * m[6] * m[15] + m[0] * m[7] * m[14] + m[4] * m[2] * m[15] - m[4] * m[3] * m[14] - m[12] * m[2] * m[7] + m[12] * m[3] * m[6];
            inv[10] = m[0] * m[5] * m[15] - m[0] * m[7] * m[13] - m[4] * m[1] * m[15] + m[4] * m[3] * m[13] + m[12] * m[1] * m[7] - m[12] * m[3] * m[5];
            inv[14] = -m[0] * m[5] * m[14] + m[0] * m[6] * m[13] + m[4] * m[1] * m[14] - m[4] * m[2] * m[13] - m[12] * m[1] * m[6] + m[12] * m[2] * m[5];
            inv[3] = -m[1] * m[6] * m[11] + m[1] * m[7] * m[10] + m[5] * m[2] * m[11] - m[5] * m[3] * m[10] - m[9] * m[2] * m[7] + m[9] * m[3] * m[6];
            inv[7] = m[0] * m[6] * m[11] - m[0] * m[7] * m[10] - m[4] * m[2] * m[11] + m[4] * m[3] * m[10] + m[8] * m[2] * m[7] - m[8] * m[3] * m[6];
            inv[11] = -m[0] * m[5] * m[11] + m[0] * m[7] * m[9] + m[4] * m[1] * m[11] - m[4] * m[3] * m[9] - m[8] * m[1] * m[7] + m[8] * m[3] * m[5];
            inv[15] = m[0] * m[5] * m[10] - m[0] * m[6] * m[9] - m[4] * m[1] * m[10] + m[4] * m[2] * m[9] + m[8] * m[1] * m[6] - m[8] * m[2] * m[5];

            det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

            if (det == 0)
                return { false, r };

            det = 1.0 / det;

            for (auto i = 0; i < 16; i++)
                inv[i] = inv[i] * det;

            
            return { true, r };
        }

        template<typename = std::enable_if_t<N == 4>>
        quat to_quat() const
        {
            // 0 4 8  12 | 0 3 6
            // 1 5 9  13 | 1 4 7
            // 2 6 10 14 | 2 5 8
            // 3 7 11 15 |
            auto& m = this->data;

            auto fTrace = m[0] + m[5] + m[10];
            auto fRoot = 0.0f;

            float data[4];

            if (fTrace > 0.0f) {
                // |w| > 1/2, may as well choose w > 1/2
                fRoot = std::sqrt(fTrace + 1.0f); // 2w
                data[3] = 0.5f * fRoot;
                fRoot = 0.5f / fRoot; // 1/(4w)
                data[0] = (m[6] - m[9]) * fRoot;
                data[1] = (m[8] - m[2]) * fRoot;
                data[2] = (m[1] - m[4]) * fRoot;
            }
            else {
                // |w| <= 1/2
                auto i = 0;
                if (m[5] > m[0]) i = 1;
                if (m[10] > m[i * 4 + i]) i = 2;
                auto j = (i + 1) % 3;
                auto k = (i + 2) % 3;

                fRoot = std::sqrt(m[i * 4 + i] - m[j * 4 + j] - m[k * 4 + k] + 1.0f);
                data[i] = 0.5f * fRoot;
                fRoot = 0.5f / fRoot;
                data[3] = (m[j * 4 + k] - m[k * 4 + j]) * fRoot;
                data[j] = (m[j * 4 + i] + m[i * 4 + j]) * fRoot;
                data[k] = (m[k * 4 + i] + m[i * 4 + k]) * fRoot;
            }

            return {data[0], data[1], data[2], data[3] };
        }
    private:
        template <size_t I> T determinant_pos() const
        {
            ((std::cout << " " << (*this)(Is, (Is + I) % N)), ...);
            std::cout << std::endl;
            return ((*this)(Is,(Is + I) % N) * ...);
        }
        template <size_t I> T determinant_neg() const
        {
            ((std::cout << " " << (*this)(Is, (I - Is) % N)), ...);
            std::cout << std::endl;
            return ((*this)(Is, (I - Is) % N) * ...);
        }

        base_matrix()
        {
        }

        template <typename F, size_t COL, size_t RowStart, size_t... ROWs>
        void for_each_row(F f, std::index_sequence <ROWs...>)
        {
            ( f(RowStart + ROWs, COL, (*this)(RowStart + ROWs, COL)), ...);
        }

        template <size_t COL, size_t... ROWs>
        auto operator_equal_row(const TMAT& r, std::index_sequence <ROWs...>) const
        {
            return (((*this)(ROWs, COL) == r(ROWs, COL)) && ...);
        }

        auto matvec(const size_t row, const vector<T, N>& r) const
        {
            return (((*this)(row, Is) * r[Is]) + ...);
        }
    };

    template <typename TNumber, typename T, size_t N, size_t... Is, size_t... AIs>
    auto operator * (
        const TNumber& l,
        const base_matrix<T, N, N, std::tuple<std::index_sequence<Is...>, std::index_sequence<AIs...>>>& r)
    {
        return r.ltimes(l);
    }

    using mat4 = base_matrix<DEFAULT_FLOAT, 4, 4,
        std::tuple<
            std::make_index_sequence<4>,
            std::make_index_sequence<16>
        >
    >;
    template <typename T>
    using mat4_t = base_matrix<T, 4, 4,
        std::tuple<
        std::make_index_sequence<4>,
        std::make_index_sequence<16>
        >
    >;

    // QUATERNIONS
    template <typename T>
    struct base_quat
    {
        using TQUAT = base_quat<T>;

        T x, y, z, w;

        base_quat(T x, T y, T z, T w) : x{ x }, y{ y }, z{ z }, w{ w }
        {
        }

        template <typename T2, typename T3>
        static TQUAT from(const vector<T2, 3>& n, const T3& angle)
        {
            auto cos = std::cos(angle / 2);
            auto sin = std::sin(angle / 2);
            return { (T)sin * n.x, (T)sin * n.y, (T)sin * n.z, (T)cos };
        }

        template <typename T2, typename T3>
        static TQUAT from(T2 nx, T2 ny, T2 nz, const T3& angle)
        {
            auto cos = std::cos(angle / 2);
            auto sin = std::sin(angle / 2);
            return { (T)sin * nx, (T)sin * ny, (T)sin * nz, (T)cos };
        }

        template <typename T2>
        static TQUAT from_euler(T2 h, T2 p, T2 b)
        {
            return {
                cos(h/2)*sin(p/2)*cos(b/2) + sin(h/2)*cos(p/2)*sin(b/2),
                sin(h/2)*cos(p/2)*cos(b/2) - cos(h/2)*sin(p/2)*sin(b/2),
                cos(h/2)*cos(p/2)*sin(b/2) - sin(h/2)*sin(p/2)*cos(b/2),
                cos(h/2)*cos(p/2)*cos(b/2) + sin(h/2)*sin(p/2)*sin(b/2)
            };
        }

        static TQUAT id()
        {
            return { 0,0,0,1 };
        }

        TQUAT operator - () const
        {
            return { -x,-y,-z,-w };
        }

        template <typename T2>
        bool operator == (const base_quat<T2>& r) const
        {
            return x == r.x && y == r.y && z == r.z && w == r.w;
        }

        TQUAT operator * (const TQUAT& r) const
        {
            return
            {
                w*r.x + x*r.w + y*r.z - z*r.y,
                w*r.y + y*r.w + z*r.x - x*r.z,
                w*r.z + z*r.w + x*r.y - y*r.x,
                w*r.w - x*r.x - y*r.y - z*r.z,
            };
        }

        template <typename T2>
        TQUAT& operator *= (const T2& f)
        {
            x *= f;
            y *= f;
            z *= f;
            w *= f;
            return *this;
        }

        template <typename T2>
        TQUAT& operator /= (const T2& f)
        {
            x /= f;
            y /= f;
            z /= f;
            w /= f;
            return *this;
        }

        template <typename T2>
        TQUAT operator * (const T2& f) const
        {
            return {x*f,y*f,z*f,w*f};
        }

        template <typename T2>
        TQUAT operator / (const T2& f) const
        {
            return { x / f,y / f,z / f,w / f };
        }

        template <size_t DNORM = 2> T norm2() const { return 0; /*TODO*/ }
        template <> T norm2<2>() const
        {
            return x * x + y * y + z * z + w * w;
        }

        template <size_t DNORM = 2> T norm() const { return 0; /*TODO*/ }
        template <> T norm<2>() const
        {
            return std::sqrt(norm2<2>());
        }

        //https://stackoverflow.com/a/12934750/5397116
        TQUAT& normalize()
        {
            auto n = norm();
            return (*this) /= n;
        }

        TQUAT normalized() const
        {
            auto n = norm();
            return (*this) / n;
        }

        TQUAT conjugate()
        {
            x *= -1;
            y *= -1;
            z *= -1;
            return *this;
        }

        TQUAT conjugated() const
        {
            return { -x,-y,-z, w };
        }

        std::tuple<bool,TQUAT> inversed() const
        {
            auto n = norm();
            if (n == 0) return { false, {0,0,0,0} };

            return { true, conjugated() / n };
        }

        template <typename T2>
        vector<T2,3> rotate(const vector<T2,3>& p) const
        {
            auto [r, inv] = inversed();

            auto p4 = TQUAT{ p.x, p.y, p.z, (T2)0 };
            auto rp4 = (*this) * p4 * inv;
            return vector<T2, 3>{ (T2)rp4.x, (T2)rp4.y, (T2)rp4.z };
        }

        template <typename T2>
        vector<T2, 4> rotate(const vector<T2, 4>& p) const
        {
            auto [r, inv] = inversed();
            auto p4 = TQUAT{ p.x, p.y, p.z, p.w };
            auto rp4 = (*this) * p4 * inv;
            return { (T2)rp4.x, (T2)rp4.y, (T2)rp4.z, (T2)rp4.w };
        }

        mat4_t<T> to_mat4() const
        {
            return {
               1 - 2 * y * y - 2 * z * z,
               2 * x * y + 2 * w * z,
               2 * x * z - 2 * w * y,
               0,

               2 * x * y - 2 * w * z,
               1 - 2 * x * x - 2 * z * z,
               2 * y * z + 2 * w * x,
               0,

               2 * x * z + 2 * w * y,
               2 * y * z - 2 * w * x,
               1 - 2 * x * x - 2 * y * y,
               0,

               0,
               0,
               0,
               1
            };
        }

        vector<T, 3> to_euler() const
        {
            float h, p, b;

            float sp = -2.0f *(y*z - w*x);
            
            if(fabs(sp) > 0.9999f) {
                p = 1.570796f * sp;
                h = atan2(-x*z + w*y, 0.5f - y*y - z*z);
                b = 0.0f;
            }
            else
            {
                p = asin(sp);
                h = atan2(x*z + w*y, 0.5f - x*x - y*y);
                b = atan2(x*y + w*z, 0.5f - x*x - z*z);
            }

            return { p, h, b };
        }
    };

   

    std::ostream& operator<< (std::ostream& out, const quat& v)
    {
        return out << "[" << v.x << ";" << v.y << ";" << v.z << ";" << v.w << "]";
    }
    
    // AABB

    template <typename T,
        size_t D,
        size_t... Is>
    struct AABB
    {
        vector<T, D> min;
        vector<T, D> max;

        AABB() :
            min{ static_cast<id_t<T,Is>>(0)... },
            max{ static_cast<id_t<T,Is>>(0)... }
        {
        }

        AABB(const vector<T, D>& min, const vector<T, D>& max) : min{ min }, max{ max }
        {
        }

        void add(const vector<T,D>& p)
        {
            ((min[Is] = p[Is] < min[Is] ? p[Is] : min[Is]), ...);
            ((max[Is] = p[Is] > max[Is] ? p[Is] : max[Is]), ...);
        }

        bool inside(const vector<T, D>& p) const
        {
            return ((p[Is] >= min[Is] && p[Is] <= max[Is]) && ...);
        }
    };

    // Plane

    template <typename T, size_t D>
    struct plane
    {
        using TPLANE = plane<T, D>;
        using TVEC = vector<T, D>;

        TVEC normal;
        T d;

        static std::tuple<bool,TPLANE> from(const TVEC& a, const TVEC& b, const TVEC& c)
        {
            auto v1 = a - b;
            auto v2 = c - b;

            auto n = (v1 % v2);
            auto norm = n.norm();

            if (norm == 0) return { false, {{0,0,0}, 0} };

            auto d = n * a;

            return { true, {n, d} };
        }
    };

    using plane3 = plane<DEFAULT_FLOAT, 3>;

    // DISTANCES

    template <size_t DNORM = 2, 
        typename T,
        typename... Ts,
        size_t... Is>
    auto dist(
        const base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>& a, 
        const base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>& b)
    {
        return (a - b).template norm<DNORM>();
    }

    template <size_t DNORM = 2,
        typename T,
        typename... Ts,
        size_t... Is>
    auto dist(
        const base_vector<T, std::tuple<Ts...>, std::index_sequence<Is...>>& a,
        const plane<T,sizeof...(Is)>& p)
    {
        return std::abs((p.normal * a) - p.d);
    }

    // LERP
    template <typename T,
        typename = std::enable_if_t<
            std::is_same_v<T,float> || std::is_same_v<T, double> || std::is_same_v<T, f32>
        >>
        auto lerp(
            const T& a,
            const T& b,
            const T& t)
    {
        auto delta = b - a;
        return a + t * delta;
    }

    template <typename T,
        typename T2,
        typename = std::enable_if_t<
        std::is_same_v<T, float> || std::is_same_v<T, double> || std::is_same_v<T, f32>
        >>
    auto slerp(
        const base_quat<T>& a,
        const base_quat<T>& b,
        const T2& tt)
    {
        T t = tt;
        auto x1 = a.x;
        auto y1 = a.y;
        auto z1 = a.z;
        auto w1 = a.w;

        auto x2 = b.x;
        auto y2 = b.y;
        auto z2 = b.z;
        auto w2 = b.w;

        T k0 = 0;
        T k1 = 0;

        float cosw = x1*x2 + y1*y2 + z1*z2 + w1*w2;
        if(FloatComparison::lessThanZero(cosw))
        {
            w1 = -w1;
            x1 = -x1;
            y1 = -y1;
            z1 = -z1;
            cosw = -cosw;
        }

        if (FloatComparison::moreThanOne(cosw))
        {
            k0 = 1.0 - t;
            k1 = t;
        }
        else
        {
            float sinw = std::sqrt(1.0 - cosw*cosw);
            float omega = std::atan2(sinw, cosw);
            float oneOverSinW = 1.0 / sinw;
            k0 = std::sin((1.0 - t) * omega) * oneOverSinW;
            k1 = std::sin(t * omega) * oneOverSinW;
        }
        
        return quat{
            x1*k0 + x2*k1,
            y1*k0 + y2*k1,
            z1*k0 + z2*k1,
            w1*k0 + w2*k1,
        };
    }

    // Intersection

    template <typename T, size_t D>
    struct ray
    {
        vector<T, D> start;
        vector<T, D> dir;
    };

    using ray3 = ray<DEFAULT_FLOAT, 3>;

    template <typename T, size_t D>
    struct line_segment
    {
        vector<T, D> start;
        vector<T, D> end;

        std::tuple<ray<T, D>,T> get_ray() const
        {
            auto d = end - start;
            auto l = d.norm();
            return { {start, d / l }, l };
        }
    };

    using seg3 = line_segment<DEFAULT_FLOAT, 3>;

    template <typename T, size_t D>
    struct sphere
    {
        vector<T, D> center;
        T radius;
    };

    using sphere3 = sphere<DEFAULT_FLOAT, 3>;

    template <typename T>
    struct quadratic_solution
    {
        size_t sols;
        T smaller;
        T bigger;
    };

    // a*x**2 + b*x + c = 0 
    template <typename T>
    quadratic_solution<T> solve_polynomial(T a, T b, T c)
    {
        float delta = b * b - 4 * a * c;

        if (delta < 0)
            return { 0, 0, 0 };

        auto sqrtDelta = std::sqrt(delta);

        auto twoa = 2 * a;

        auto t1 = (-b - sqrtDelta) / twoa;
        if (delta == 0)
            return { 1, t1, t1 };

        auto t2 = (-b + sqrtDelta) / twoa;
        return { 2, t1, t2 };
    }

    // An Introduction to Ray Tracing - IRT p.39,91;
    // The Graphics Gems series. - p.388; <- does not handle behind ray
    // http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.49.9172&rep=rep1&type=pdf
    // Geometric Tools
    // 3D Games: Real-time Rendering and Software Technology, 3DG p.16;
    // Geometric Tools for Computer Graphics p.501;
    // http://web.archive.org/web/20190911160248/http://www.andrewaye.com:80/Teikitu%20Gaming%20System/source_collision.shtml
    // Real-Time Collision Detection 5.3.2 page 177
    // http://graphicscodex.com/
    // Real-Time Rendering, Fourth Edition p.955;
    // Game Physics Cookbook;
    // http://www.iquilezles.org/www/articles/intersectors/intersectors.htm
    std::vector<vec3> intersection(const ray3& r, const sphere3& s, float tmin = 0, float tmax = 9999999999)
    {
        auto ps = std::vector<vec3>{};

        auto m = r.start - s.center;
        auto m2 = m.dot(m);
        auto r2 = s.radius * s.radius;

        float b = 2*m.dot(r.dir);
        float c = m2 - r2;

        // rays’s origin outside sphere (c > 0)
        // and ray is pointing away from sphere (b > 0)
        if (c > 0.0f && b > 0.0f) 
            return ps;

        auto sol = solve_polynomial<f32>(1, b, c);

        if ((sol.sols >= 1) && (sol.smaller >= tmin) && (sol.smaller <= tmax))
            ps.push_back(r.start + sol.smaller * r.dir);
        if ((sol.sols >= 2) && (sol.bigger >= tmin) && (sol.bigger <= tmax))
            ps.push_back(r.start + sol.bigger * r.dir);

        return ps;
    }

    std::vector<vec3> intersection(const sphere3& s, const ray3& r)
    {
        return intersection(r, s);
    }

    std::vector<vec3> intersection(const seg3& seg, const sphere3& s, float tmin = 0, float tmax = 9999999999)
    {
        auto [ray,l] = seg.get_ray();
        return intersection(ray, s, 0, l);
    }

    enum class object_type
    {
        sphere = 1
    };

    // Uniform Grid
    struct spatial_partitioning_record
    {
        object_type type;
        void* obj;
    };

    template <typename T, size_t D>
    struct spatial_partitioning_presence
    {
        vector<T, D> pos;
        std::unordered_map<void*, int> inside;
    };

    enum class entering_exiting
    {
        exiting = 0,
        entering = 1,
    };

    template <typename T, size_t D>
    struct spatial_partitioning_step_to_result
    {
        vector<T, D> pos;
        void* obj;
        entering_exiting status;
    };

    template <typename T, size_t D>
    struct spatial_partitioning
    {
        std::vector<spatial_partitioning_record> objs;

    public:
        void insert(const sphere<T, D>* s)
        {
            objs.push_back({
                object_type::sphere,
                (void*)s
            });
        }

        spatial_partitioning_presence<T,D> 
            new_presence(vector<T, D> pos)
        {
            return { pos };
        }

        std::vector<spatial_partitioning_step_to_result<T,D>>
            step_to(
                spatial_partitioning_presence<T, D>& p,
                vector<T, D> target)
        {
            auto intersections = std::vector<spatial_partitioning_step_to_result<T,D>>{};

            auto segment = line_segment<T,D>{ p.pos, target };

            for (auto&& o : objs)
            {
                if (o.type == object_type::sphere)
                {
                    auto& s = *(sphere<T,D>*)o.obj;
                    auto ints = intersection(segment, s);

                    int* inside_int = nullptr;

                    if (ints.size() > 0)
                    {
                        inside_int = &p.inside[o.obj];
                    }

                    for (auto&& i : ints)
                    {
                        (*inside_int)++;
                        *inside_int = (*inside_int) % 2;

                        intersections.push_back({
                            i,
                            o.obj,
                            (entering_exiting )*inside_int
                        });
                    }
                }
            }

            p.pos = target;

            return intersections;
        }
    };

    using ugrid3 = spatial_partitioning<DEFAULT_FLOAT, 3>;
}

namespace std
{
    void swap(math::f32& a, math::f32& b) {
        a.swap(b);
    }
}

math::f32 operator "" _f32(long double v) { return math::f32{ (float)v }; }
