#pragma once

namespace math
{
    template <size_t D, typename T = float> struct vector { T data[D]; };
    template <typename T> struct vector<2,T>
    {
        union
        {
            T data[2];
            T x, y;
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
    auto zeros()
    {
        if constexpr (D == 2) return vec2{ 0,0 };
        if constexpr (D == 3) return vec2{ 0,0,0 };
        if constexpr (D == 4) return vec2{ 0,0,0,0 };
        else
        {
            return vector<D, T> { (T)0 };
        }
    }
}