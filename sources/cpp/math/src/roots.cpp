#include "roots.h"

__attribute__((export_name("solve_quadratic_f32")))
roots::roots<float, 2>
solve_quadratic(float a, float b, float c) { return roots::solve_quadratic(a, b, c); }

int main()
{
    return 0;
}