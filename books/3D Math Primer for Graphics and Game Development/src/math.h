#pragma once
#include <type_traits>
#include <algorithm>
#include <cmath>

namespace math
{
    #define SWIZZLE_XX vector<2,T> xx() const { return {x,x}; }
    #define SWIZZLE_YY vector<2,T> yy() const { return {y,y}; }

    template <size_t D, typename T = float> struct vector
    {
        T data[D];

        vector(T v) : data{ v }
        {
        }
    };

    template <typename T> struct vector<2,T>
    {
        using TME = vector<2, T>;
        union
        {
            struct { T x, y; };
            T data[2];
        };

        vector(T x, T y) : x{ x }, y{ y }
        {
        }

        constexpr T& operator [] (size_t i)
        {
            //static_assert (i < 2); // waiting http://open-std.org/JTC1/SC22/WG21/docs/papers/2019/p1045r1.html
            return data[i];
        }
        
        SWIZZLE_XX;
        SWIZZLE_YY;

        template <size_t DNORM = 2> T norm2() const { static_assert(false); /*TODO*/ }
        template <> T norm2<2>() const { return x * x + y * y; }

        template <size_t DNORM = 2> T norm() const { static_assert(false); /*TODO*/ }
        template <> T norm<2>() const { return std::sqrt(norm2<2>()); }
        

        template <size_t DNORM = 2> TME& normalize() { return (*this) /= norm<DNORM>(); }
        template <size_t DNORM = 2> TME normalized() const { auto v = *this; return v.normalize<DNORM>(); }

        template <typename TNumber> bool operator == (const vector<2, TNumber>& b) const
        {
            return (x == b.x) && (y == b.y);
        }

        TME operator -() const { return { -x,-y }; }

        template <typename TNumber> TME operator *(const TNumber f) const { return { f * x, f * y }; }
        template <typename TNumber> TME& operator *=(const TNumber f) { x *= f; y *= f; return *this; } 
        template <typename TNumber> TME operator /(const TNumber f) const { return { f / x, f / y }; }
        template <typename TNumber> TME& operator /=(const TNumber f) { x /= f; y /= f; return *this; }

        template <typename TNumber> TME operator + (const vector<2, TNumber>& b) const { return { x + b.x, y + b.y }; }
        template <typename TNumber> TME& operator += (const vector<2, TNumber>& b) { x += b.x;  y += b.y; return *this; }

        template <typename TNumber> TME operator - (const vector<2, TNumber>& b) const { return { x - b.x, y - b.y }; }
        template <typename TNumber> TME& operator -= (const vector<2, TNumber>& b) { x -= b.x;  y -= b.y; return *this; }

        template <typename TNumber> T operator *(const vector<2, TNumber>& b) const { return dot(b); }
        template <typename TNumber> T dot(const vector<2, TNumber>& b) const { return x * b.x + y * b.y; }
    };
    
    template <typename T> struct vector<3,T>
    {
        using TME = vector<3, T>;

        T x, y, z;

        TME operator -() const { return { -x,-y, -z }; }

        template <typename TNumber> bool operator == (const vector<3, TNumber>& b) const
        {
            return (x == b.x) && (y == b.y) && (z == b.z);
        }

        template <typename TNumber> TME operator *(const TNumber f) const { return { f * x, f * y, f * z }; }
        template <typename TNumber> TME operator +(const vector<3, TNumber>& b) const
        {
            return { x + b.x, y + b.y, z + b.z };
        }

        template <typename TNumber> TME operator %(const vector<3, TNumber>& b) const { return cross(b); }
        template <typename TNumber> TME cross(const vector<3, TNumber>& b) const
        {
            return { y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x };
        }
    };
    template <typename T> struct vector<4,T> {  T x, y, z, w; };

    template <typename TNumber, size_t D, typename T> vector<D,T> operator *(TNumber f, vector<D, T> v) { return v * f; }

    using vec2 = vector<2, float>;
    using vec3 = vector<3, float>;
    using vec4 = vector<4, float>;

    using dvec2 = vector<2, double>;
    using dvec3 = vector<3, double>;
    using dvec4 = vector<4, double>;

    template <size_t D, typename T = float>
    constexpr auto zeros()
    {
        if constexpr (D == 2) return vec2{ 0,0 };
        else if constexpr (D == 3) return vec3{ 0,0,0 };
        else if constexpr (D == 4) return vec4{ 0,0,0,0 };
        else return vector<D, T> { (T)0 };
    }

    template <size_t D, typename T, typename F>
    constexpr auto all(vector<D,T> v, F f)
    {
        if constexpr (D == 2) return f(v.x) && f(v.y);
        else if constexpr (D == 3) return f(v.x) && f(v.y) && f(v.z);
        else if constexpr (D == 4) return f(v.x) && f(v.y) && f(v.w);
        else return std::all_of(std::begin(v.data), std::end(v.data), f);
    }

    template <size_t D, typename T> constexpr auto neg(vector<D, T> v) { return -v; }

    template <typename TNumber, size_t D, typename T> constexpr auto mul(vector<D, T> v, TNumber n) { return v * n; }
    template <typename TNumber, size_t D, typename T> constexpr auto mul(TNumber n, vector<D, T> v) { return n * v; }

    template <size_t D, typename T1, typename T2> constexpr auto sum(vector<D, T1> a, vector<D, T2> b) { return a + b; }
    template <size_t D, typename T1, typename T2> constexpr auto diff(vector<D, T1> a, vector<D, T2> b) { return a - b; }


    namespace distance
    {
        template <typename TA, typename TB> struct dist {};
        template <size_t D, typename T1, typename T2> struct dist<vector<D, T1>, vector<D, T2>>
        {
            template <size_t DNORM> T1 run(const vector<D, T1> &a, const vector<D, T2> &b)
            {
                return (a - b).norm<DNORM>();
            }
        };
    }
    template <size_t DNORM = 2, typename TA, typename TB> auto dist(const TA& a, const TB& b) { auto d = distance::dist<TA, TB>{}; return d.run<DNORM>(a, b); }
}