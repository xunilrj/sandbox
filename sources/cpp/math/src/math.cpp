// #include <iostream>

#include "./math.h"

#define FLOAT3(name) float name##x, float name##y, float name##z
#define USEFLOAT3(name) name##x, name##y, name##z

__attribute__((export_name("sum"))) float sum(FLOAT3(origin))
{
    return originx + originy + originz;
}

__attribute__((export_name("intersection_ray_triangle")))
ray_triangle_result<float>
    intersection_ray_triangle(
        FLOAT3(origin), FLOAT3(dir),
        FLOAT3(a), FLOAT3(b), FLOAT3(c))
{
    vec3f o = {USEFLOAT3(origin)};
    vec3f dir = {USEFLOAT3(dir)};
    vec3f a = {USEFLOAT3(a)};
    vec3f b = {USEFLOAT3(b)};
    vec3f c = {USEFLOAT3(c)};

    return intersection_ray_triangle(
        o, dir,
        a, b, c);
}

__attribute__((export_name("barycentric_coords")))
vec3f
barycentric_coords(
    float u, float v,
    FLOAT3(a), FLOAT3(b), FLOAT3(c))
{
    vec3f a = {USEFLOAT3(a)};
    vec3f b = {USEFLOAT3(b)};
    vec3f c = {USEFLOAT3(c)};

    return barycentric_coords(
        u, v,
        a, b, c);
}

// #include <iostream>
// int main()
// {
//     auto r = intersection_ray_triangle(
//         0.7900000000000003, 0.5, 100,
//         0, 0, -1,
//         1, 1, -4.0747809410095215,
//         0, -0, -4.318110466003418,
//         1, -0, -4.246045112609863);
//     std::cout << r.ok << std::endl;
//     std::cout << r.t << std::endl;
//     std::cout << r.u << std::endl;
//     std::cout << r.v << std::endl;
//     return 0;
// }