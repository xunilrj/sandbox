#pragma once
#include <type_traits>
#include <algorithm>
#include <cmath>
#include <numeric>

#include <ostream>

namespace math
{
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
        f32() : value{ 0 } {}
        f32(const float v) : value{ v } {}

        f32 operator - (const float v) const { return { value - v }; }
        f32 operator + (const float v) const { return { value + v }; }
        f32 operator * (const float v) const { return { value * v }; }
        f32 operator / (const float v) const { return { value / v }; }

        bool operator == (const f32 b) const { return AbsoluteRelativeComparison::equals(value, b.value); }

        f32 operator - () const { return { -value }; }
        operator float() const { return value; }

        template <typename TNumber> f32& operator += (const f32 v) { value += v.value; return *this; }
        template <typename TNumber> f32& operator -= (const f32 v) { value -= v.value; return *this; }
        template <typename TNumber> f32& operator *= (const f32 v) { value *= v.value; return *this; }
        template <typename TNumber> f32& operator /= (const f32 v) { value /= v.value; return *this; }

        template <typename TNumber> f32& operator *= (const TNumber v) { value *= v; return *this; }
        template <typename TNumber> f32& operator /= (const TNumber v) { value /= v; return *this; }
        template <typename TNumber> f32& operator += (const TNumber v) { value += v; return *this; }
        template <typename TNumber> f32& operator -= (const TNumber v) { value -= v; return *this; }

        friend std::ostream& operator<<(std::ostream& out, f32 v);
        friend bool operator == (float a, f32 b);
        friend bool operator == (f32 a, float b);
        friend bool operator == (int a, f32 b);
        friend bool operator == (f32 a, int b);
    };

    bool operator == (const float a, const f32 b) { return AbsoluteRelativeComparison::equals(a, b.value); }
    bool operator == (const f32 a, const float b) { return AbsoluteRelativeComparison::equals(a.value, b); }
    bool operator == (const int a, const f32 b) { return AbsoluteRelativeComparison::equals((float)a, b.value); }
    bool operator == (const f32 a, const int b) { return AbsoluteRelativeComparison::equals(a.value, (float)b); }

    std::ostream& operator<<(std::ostream& out, math::f32 v)
    {
        return out << v.value;
    }

    template <size_t D, typename T = f32> struct named_store { T data[D]; };
    template <typename T> struct named_store<2, T> { union { T data[2]; struct { T x, y; }; }; };
    template <typename T> struct named_store<3, T> { union { T data[3]; struct { T x, y, z; }; }; };
    template <typename T> struct named_store<4, T> { union { T data[4]; struct { T x, y, z, w; }; }; };

    template <size_t D, typename T = f32, typename... TMixings>
    struct base_vector : public named_store<D, T>, public TMixings...
    {
        template <typename TT>
        using TME = base_vector<D, TT, TMixings...>;

        static TME<T> zero()
        {
            auto r = TME{};
            r.data = { 0 };
            return r;
        }

        template <typename... TArgs>
        base_vector(TArgs... args) : named_store<D, T>{ static_cast<T>(args)... }
        {
        }

        constexpr T& operator [] (size_t i)
        {
            //static_assert (i < 2); // waiting http://open-std.org/JTC1/SC22/WG21/docs/papers/2019/p1045r1.html
            return this->data[i];
        }

        constexpr const T& operator [] (size_t i) const
        {
            //static_assert (i < 2); // waiting http://open-std.org/JTC1/SC22/WG21/docs/papers/2019/p1045r1.html
            return this->data[i];
        }

        template <size_t DNORM = 2> T norm2() const { return 0; /*TODO*/ }
        template <> T norm2<2>() const { T r = 0; for (auto i = 0; i < D; ++i) { r += this->data[i] * this->data[i]; } return r; }

        template <size_t DNORM = 2> T norm() const { return 0; /*TODO*/ }
        template <> T norm<2>() const { return std::sqrt(norm2<2>()); }

        template <size_t DNORM = 2> TME<T>& normalize() { return (*this) /= norm<DNORM>(); }
        template <size_t DNORM = 2> TME<T> normalized() const { auto v = *this; return v.template normalize<DNORM>(); }

        template <typename TNumber> bool operator == (const TME<TNumber>& b) const
        {
            auto r = true;
            for (auto i = 0; i < D; ++i)
                r &= this->data[i] == b.data[i];
            return r;
        }

        TME<T> operator -() const { auto r = TME<T>{}; for (auto i = 0; i < D; ++i) { r.data[i] = this->data[i] * -1.0f; } return r; }

        template <typename TNumber> TME<T> operator *(const TNumber f) const { auto r = TME<T>{ 0 }; for (auto i = 0; i < D; ++i) { r.data[i] = f * this->data[i]; } return r; }
        template <typename TNumber> TME<T> operator /(const TNumber f) const { auto r = TME<T>{ 0 }; for (auto i = 0; i < D; ++i) { r.data[i] = f / this->data[i]; } return r; }
        template <typename TNumber> TME<T>& operator *=(const TNumber f) { for (auto i = 0; i < D; ++i) { this->data[i] *= f; } return *this; }
        template <typename TNumber> TME<T>& operator /=(const TNumber f) { for (auto i = 0; i < D; ++i) { this->data[i] /= f; } return *this; }

        template <typename TNumber> TME<T> operator + (const TME<TNumber>& b) const { auto r = TME<T>{ 0 }; for (auto i = 0; i < D; ++i) { r.data[i] = this->data[i] + b.data[i]; } return r; }
        template <typename TNumber> TME<T> operator - (const TME<TNumber>& b) const { auto r = TME<T>{ 0 }; for (auto i = 0; i < D; ++i) { r.data[i] = this->data[i] - b.data[i]; } return r; }
        template <typename TNumber> TME<T>& operator +=(const TME<TNumber>& b) { for (auto i = 0; i < D; ++i) { this->data[i] += b.data[i]; } return *this; }
        template <typename TNumber> TME<T>& operator -=(const TME<TNumber>& b) { for (auto i = 0; i < D; ++i) { this->data[i] -= b.data[i]; } return *this; }

        template <typename TNumber> T operator *(const TME<TNumber>& b) const { return dot(b); }
        template <typename TNumber> T dot(const TME<TNumber>& b) const
        {
            T r = 0; for (auto i = 0; i < D; ++i) { r += this->data[i] * b.data[i]; } return r;
        }

        template <typename = typename std::enable_if<D == 3>::type>
        TME<T> operator % (const base_vector<3, T, TMixings...>& b) const
        {
            return {
                this->y * b.z - this->z * b.y,
                this->z * b.x - this->x * b.z,
                this->x * b.y - this->y * b.x
            };
        }
    };

    struct no_mixin {};

    #ifndef DEFAULT_TRAITS
        #define DEFAULT_TRAITS no_mixin
    #endif

    template <size_t D, typename T>
    using vector = base_vector<D, T, DEFAULT_TRAITS>;

    template <size_t D, typename T> 
    vector<D, T> operator *(int f, const vector<D, T>& v) { return v * f; }
    template <size_t D, typename T> 
    vector<D, T> operator *(float f, const vector<D, T>& v) { return v * f; }

    using vec2 = vector<2, f32>;
    using vec3 = vector<3, f32>;
    using vec4 = vector<4, f32>;

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
    constexpr auto all(vector<D, T> v, F f)
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
            template <size_t DNORM> T1 run(const vector<D, T1>& a, const vector<D, T2>& b)
            {
                return (a - b).template norm<DNORM>();
            }
        };
    }
    template <size_t DNORM = 2, typename TA, typename TB> auto dist(const TA& a, const TB& b) { auto d = distance::dist<TA, TB>{}; return d.run<DNORM>(a, b); }
}
