#! /root/miniconda3/bin/python3
from sympy import symbols, sqrt, diff, solve, collect, expand, Poly, cse
from generator import CodeGenerator


def nearest2_point_line():
    gen = CodeGenerator()
    if (gen.skip_gen()):
        return

    gen = CodeGenerator()
    s = symbols("s")

    P0 = gen.newVec2("P0")
    L0 = gen.newLine2("L0", s)
    D = gen.distance(P0, L0)
    s_star = gen.argmin(D, s)[0]
    gen.gen(s_star)


def nearest2_point_quadratic_bezier():
    gen = CodeGenerator()
    if (gen.skip_gen()):
        return

    s = symbols("s")

    P0 = gen.newVec2("P0")

    # A quadratic Bézier curve is the path traced by the function B(t), given points P0, P1, and P2,
    # (1-t)( (1-t)*P0 + t*P1) ) + t*( (1-t)*P1 + t*P2 )
    P1 = gen.newVec2("P1")
    P2 = gen.newVec2("P2")
    P3 = gen.newVec2("P3")
    CURVE = (1-s)*((1-s)*P1 + s*P2) + s*((1-s)*P2 + s*P3)

    D = gen.distance_sq(P0, CURVE)
    Dprime = gen.diff_as_poly(D, s)
    gen.gen(Dprime)


def dist2_line_line():
    gen = CodeGenerator()
    if (gen.skip_gen()):
        return

    gen = CodeGenerator()
    s = symbols("s")

    L0 = gen.newLine2("L0", s)
    L1 = gen.newLine2("L1", s)

    D = gen.distance(L0, L1)
    s_star = gen.argmin(D, s)[0]
    gen.gen(s_star)


# f = lambdify((pox, poy, pex, pey, qox, qoy, qex, qey), s_star)
# print(f(-1.0, 0.0, 1.0, 0.0, 0.0, -1.0, 0.0, 1.0))

nearest2_point_line()
dist2_line_line()
nearest2_point_quadratic_bezier()
