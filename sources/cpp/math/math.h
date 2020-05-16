#include <optional>

bool is_zero(float f, float tolerance = 0.0001f)
{
    return (f >= -tolerance) && (f <= tolerance);
}

bool is_between(float f, float min, float max, float tolerance = 0.0001f)
{
    return (f >= min-tolerance) && (f <= max+tolerance);
}

template <typename T>
struct vec3
{
    using type = T;
    using vec3_t = vec3<T>;

    T x, y, z;


    vec3_t operator + (const vec3_t& b) const
    {
        return {x+b.x, y+b.y, z+b.z};
    }

    vec3_t operator - (const vec3_t& b) const
    {
        return {x-b.x, y-b.y, z-b.z};
    }

    T dot(const vec3_t& b) const
    {
        return x*b.x + y*b.y + z*b.z;
    }

    vec3_t cross(const vec3_t& b) const
    {
        return
        {
            y * b.z - z * b.y,
            z * b.x - x * b.z,
            x * b.y - y * b.x
        };
    }
};

using vec3f = vec3<float>;
vec3f operator * (float f, const vec3f& b)
{
    return {f*b.x, f*b.y, f*b.z};
}


template <typename TVec, typename T>
TVec barycentric_coords(const T u, const T v,
    const TVec& a, const TVec& b, const TVec& c)
{
#ifdef GUARDS
    ASSERT(u >= 0);
    ASSERT(v >= 0);
    ASSERT(u+v <= 1);
#endif
    return (1-u-v)*a + u*b + v*c;
}

template <typename T>
struct ray_triangle_result
{
    T t;
    T u;
    T v;
};

// http://www.graphics.cornell.edu/pubs/1997/MT97.pdf
template <typename TVec>
std::optional<ray_triangle_result<typename TVec::type>> intersection_ray_triangle(
    const TVec& o, const TVec& dir,
    const TVec& a, const TVec& b, const TVec& c)
{
    auto edge1 = b - a;
    auto edge2 = c - a;

    auto pvec = dir.cross(edge2);
    auto det = edge1.dot(pvec);

    if(is_zero(det)) return {};
    auto inv_det = (typename TVec::type)1.0 / det;

    auto tvec = o - a;

    auto u = tvec.dot(pvec) * inv_det;
    if(!is_between(u,0,1)) return {};

    auto qvec = tvec.cross(edge1);
    auto v = dir.dot(qvec) * inv_det;
    if(!is_between(u,0,1-v)) return {};

    auto t = edge2.dot(qvec) * inv_det;

    return ray_triangle_result<typename TVec::type>{t,u,v};
}
