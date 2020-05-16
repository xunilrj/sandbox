#include <iostream>

#include "./math.h"

int main(int argc, char**argv)
{
    vec3f a = {-1, 0, 0};
    vec3f b = {1, 0, 0};
    vec3f c = {0, 1, 0};

    vec3f o = {0, 0.5, 1};
    vec3f dir = {0, 0, -1};

    auto r = *intersection_ray_triangle(
        o, dir,
        a, b, c
    );

    auto rv = barycentric_coords(
        r.u, r.v,
        a, b, c
    );

    std::cout << r.t << std::endl;
    std::cout << r.u << " " << r.v << std::endl;
    std::cout << rv.x << " " << rv.y << " " << rv.z << std::endl;
}