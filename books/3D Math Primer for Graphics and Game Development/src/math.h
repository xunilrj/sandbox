#pragma once
#include <type_traits>
#include <algorithm>

namespace math
{
    template <size_t D, typename T = float> struct vector
    {
        T data[D];

        vector(T v) : data{ v }
        {
        }
    };
    template <typename T> struct vector<2,T>
    {
        union
        {
            struct { T x, y; };
            T data[2];
            class { T x; public: operator vector<2, T>() const { return { x, x }; } } xx;
        };

        vector(T x, T y) : x{ x }, y{ y }
        {
        }

        constexpr T& operator [] (size_t i)
        {
            //static_assert (i < 2); // waiting http://open-std.org/JTC1/SC22/WG21/docs/papers/2019/p1045r1.html
            return data[i];
        }
        
    };
    template <typename T> struct vector<3,T> {  T x, y, z; };
    template <typename T> struct vector<4,T> {  T x, y, z, w; };

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
}