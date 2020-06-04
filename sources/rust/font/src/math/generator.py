from sympy.utilities.lambdify import lambdify, implemented_function
import sys
from re import sub, MULTILINE, match
from sympy.printing.cxxcode import CXX17CodePrinter
from sympy.printing.rust import RustCodePrinter
from sympy.utilities.codegen import make_routine
from sympy import *
from sympy.printing import print_ccode
from sympy.utilities.codegen import codegen
from sympy.vector import Vector
from sympy.vector import CoordSys3D
from sympy.physics.vector import ReferenceFrame
from sympy.simplify.cse_main import cse
import sympy.printing.llvmjitcode as jit
from sympy.codegen.ast import Assignment
from sympy.printing.rust import rust_code
from sympy.printing.cxxcode import cxxcode
import inspect


class CodeGenerator:
    def __init__(self):
        self.R = ReferenceFrame('R')

    def newVec2(self, name):
        p0x, p0y = symbols("%s_x %s_y" % (name, name), real=True)
        return p0x*self.R.x + p0y*self.R.y

    def newLine2(self, name, s):
        P0 = self.newVec2(name + "_P0")
        P1 = self.newVec2(name + "_P1")
        return P0 + s*(P1-P0)

    def distance(self, A, B):
        W = A - B
        return sqrt(W.dot(W))

    def distance_sq(self, A, B):
        W = A - B
        return W.dot(W)

    def diff_as_poly(self, expr, var):
        Dprime = Poly(collect(expand(diff(expr, var)), var), var)
        return Dprime.coeffs()

    def argmin(self, expr, var):
        V = diff(expr, var)
        return solve(V, var, quick=True)

    def skip_gen(self):
        fileName = sys.argv[2]
        funcName = inspect.stack()[1].function
        genName = sys.argv[3]

        m = match(genName, funcName)
        return m == None

    def gen(self, value):
        parameters = []
        if type(value) is list:
            for v in value:
                parameters = parameters + list(v.atoms(Symbol))
        else:
            parameters = parameters + list(value.atoms(Symbol))

        parameters = list(set(parameters))
        parameters.sort(key=lambda x: str(x))
        sub_exprs, simplified_rhs = cse(value)

        print(sub_exprs, simplified_rhs)

        lang = sys.argv[1]
        code = ""
        fileName = sys.argv[2]
        funcName = inspect.stack()[1].function

        def to_cxx17():
            printer = CXX17CodePrinter()
            lines = []
            lines.append("template <typename T>")
            lines.append("auto " + funcName +
                         "(T p0_x, T p0_y, T p1_x, T p1_y, T q0_x, T q0_y, T q1_x, T q1_y) {")
            for var, expr in sub_exprs:
                lines.append('auto ' + printer.doprint(Assignment(var, expr)))

            rnum, rden = fraction(simplified_rhs[0])
            if (rden == 1):
                lines.append('return (' + printer.doprint(rnum) + ");")
            else:
                lines.append('auto rnum = ' + printer.doprint(rnum) + ";")
                lines.append('auto rden = ' + printer.doprint(rden) + ";")
                lines.append('return rnum/rden;')
            lines.append("}\n")
            return "\n".join(lines)

        class RustCodePrinter2(RustCodePrinter):
            def _print_Symbol(self, expr):
                return str(expr).lower()

            def _print_Mul(self, expr):
                args = []
                for i in expr.args:
                    if isinstance(i, numbers.Integer):
                        args.append(Float(i))
                    else:
                        args.append(i)
                return RustCodePrinter._print_Mul(self, Mul(*args))

        def to_rust(floatType):
            printer = RustCodePrinter2()
            lines = []
            returnType = 'f32'
            if(len(simplified_rhs) > 0):
                returnType = '(' + ", ".join(['f32'
                                              for resulti in range(len(simplified_rhs))]) + ')'
            lines.append("pub fn " + funcName + "(" +
                         ",".join([str(x).lower() + ": f32" for x in parameters]) + ") -> " + returnType + " {")

            for var, expr in sub_exprs:
                lines.append('let ' + printer.doprint(var) +
                             ": " + floatType + " = " + printer.doprint(expr) + ";")

            resulti = 0
            for expr in simplified_rhs:
                rnum, rden = fraction(expr)
                if (rden == 1):
                    lines.append('let result_' + str(resulti) + ' : ' +
                                 floatType + ' = ' + printer.doprint(rnum) + ";")
                else:
                    lines.append('let rnum _' + str(resulti) + ' : ' + floatType + '= ' +
                                 printer.doprint(rnum) + ";")
                    lines.append('let rden_' + str(resulti) + ' : ' + floatType + '= ' +
                                 printer.doprint(rden) + ";")
                    lines.append('let result_' + str(resulti) +
                                 ' : = rnum _' + str(resulti) + '/rden_' + str(resulti) + ';')
                resulti += 1

            if len(simplified_rhs) == 1:
                lines.append("return result_0;")
            else:
                lines.append("return (")
                lines.append(", ".join(['result_' + str(resulti)
                                        for resulti in range(len(simplified_rhs))]))
                lines.append(");")
            lines.append("}\n")
            return "\n".join(lines)

        if(lang == "cpp"):
            code = to_cxx17()
        elif(lang == "rust-f32"):
            code = to_rust("f32")

        f = open(fileName, "r+t")
        alltxt = f.read()
        alltxt = sub(
            "\/\/ ## start %s(.|\n)*\/\/ ## end %s" % (funcName, funcName),
            "// ## start %s\n%s// ## end %s" % (funcName, code, funcName),
            alltxt,
            flags=MULTILINE
        )
        f.seek(0)
        f.truncate()
        f.write(alltxt)
