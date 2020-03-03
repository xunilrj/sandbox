#pragma once
#include <type_traits>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <array>
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

        template <typename TNumber> f32 operator - (const TNumber v) const { return { value - v }; }
        template <typename TNumber> f32 operator + (const TNumber v) const { return { value + v }; }
        template <typename TNumber> f32 operator * (const TNumber v) const { return { value * v }; }
        template <typename TNumber> f32 operator / (const TNumber v) const { return { value / v }; }

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


    template <size_t D, typename T = float> struct named_store { T data[D]; };
    template <typename T> struct named_store<2, T> { union { T data[2]; struct { T x, y; }; }; };
    template <typename T> struct named_store<3, T> { union { T data[3]; struct { T x, y, z; }; }; };
    template <typename T> struct named_store<4, T> { union { T data[4]; struct { T x, y, z, w; }; }; };

    template<typename T>
    using EnableIfArithmetic = std::enable_if_t<std::is_arithmetic_v<T>>;

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

        static TVECTOR zero() { return { static_cast<Ts>(0)... }; }

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
            ((this->data[Is] /= n),... );
            return *this;
        }
        template <size_t DNORM = 2> TVECTOR normalized() const
        {
            auto n = norm<DNORM>();
            return {this->data[Is] / n...};
        }

        TVECTOR operator -() const
        {
            return { -this->data[Is]... };
        }

        template <typename TNumber, typename = EnableIfArithmetic<TNumber>>
        TVECTOR operator *(const TNumber& f) const
        {
            return { this->data[Is] * f... };
        }

        template <typename TNumber, typename = EnableIfArithmetic<TNumber>>
        TVECTOR operator /(const TNumber f) const
        {
            return { this->data[Is] / f... };
        }

        template <typename TNumber, typename = EnableIfArithmetic<TNumber>>
        TVECTOR operator *=(const TNumber f)
        {
            ((this->data[Is] *= f), ...);
            return *this;
        }

        template <typename TNumber, typename = EnableIfArithmetic<TNumber>>
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

        template <typename TAny>
        auto ltimes (const TAny& l) const { return (*this) * l; }
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

    template <typename T, size_t N>
    class make_tuple
    {
        template<size_t> using T_ = T;
        template<size_t... Is> static auto gen(std::index_sequence<Is...>) { return std::tuple<T_<Is>...>{}; }
        static auto gen() { return gen(std::make_index_sequence<N>{}); }
    public:
        using type = decltype(gen());
    };
    template <typename T, size_t N>
    using make_tuple_t = typename make_tuple<T, N>::type;

    template <typename T, size_t D>
    using vector = base_vector<T,
        make_tuple_t<T,D>,
        std::make_index_sequence<D>>;


    #ifndef DEFAULT_TYPE
        #define DEFAULT_TYPE f32
    #endif

    using vec2 = vector<DEFAULT_TYPE, 2>;
    using vec3 = vector<DEFAULT_TYPE, 3>;
    using vec4 = vector<DEFAULT_TYPE, 4>;

    template <size_t D, typename T = DEFAULT_TYPE>
    constexpr auto zeros() { return vector<T, D>::zero(); }


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
}
