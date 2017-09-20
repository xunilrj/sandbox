using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using System.Linq.Expressions;

namespace OOFunctional
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var add11 = F.New(() => 1 + 1);
            var add1 = F.New((int x) => x + 1);
            var add = F.New((int a, int b) => a + b);

            //Simple Invokes
            //!f is the same as f.Invoke()
            Assert.AreEqual(2, add11.Invoke());
            Assert.AreEqual(2, !add11);

            Assert.AreEqual(2, add1.Invoke(1));

            Assert.AreEqual(2, add.Invoke(1, 1));

            //Partial Apply using + operator
            Assert.AreEqual(2, (add + 1).Invoke(1));
            Assert.AreEqual(2, (add + 1 + 1).Invoke());
            Assert.AreEqual(2, !(add + 1 + 1));

            //Partial Apply using index operator
            Assert.AreEqual(2, add[1].Invoke(1));
            Assert.AreEqual(2, add[1, 1].Invoke());
            Assert.AreEqual(2, !add[1, 1]);

            Assert.AreEqual(2, (add[1][1]).Invoke());
            Assert.AreEqual(2, !add[1][1]);

            //Complete Apply
            Assert.AreEqual(2, !(add + Tuple.Create(1, 1)));
            Assert.AreEqual(2, !add[Tuple.Create(1, 1)]);

            //Functional composition
            var times2 = new F<int, int>(x => x * 2);
            var addthentimes2 = add | times2;
            Assert.AreEqual(4, !addthentimes2[1, 1]);

            System.Func<int, int, int> addthentimes2func = add | (x => x * 2);
            Assert.AreEqual(4, addthentimes2func(1, 1));

            var times = new F<int, int, int>((a, b) => a * b);
            addthentimes2 = add | times[2];
            Assert.AreEqual(4, !addthentimes2[1, 1]);

            //There is no way to make this composition using operator
            //because we cant inject a new generic type in a operator
            //definition.
            var timesf = F.New((int a, int b) => (float)(a * b));
            var g = add.Then(timesf[2]);
            Assert.AreEqual(4, !g[1, 1]);

            var genericadd = G.New((g a, g b) => a + b);
            Assert.AreEqual(2, !genericadd[1, 1]);
        }
    }

    public static class F
    {
        public static F<TR> New<TR>(Func<TR> f) => new F<TR>(f);
        public static F<T1, TR> New<T1, TR>(Func<T1, TR> f) => new F<T1, TR>(f);
        public static F<T1, T2, TR> New<T1, T2, TR>(Func<T1, T2, TR> f) => new F<T1, T2, TR>(f);
    }

    public static class G
    {
        //public static G<TR> New<TR>(Func<TR> f) => new F<TR>(f);
        //public static F<T1, TR> New<T1, TR>(Func<T1, TR> f) => new F<T1, TR>(f);
        public static G2<TR> New<TR>(Expression<Func<g, g, TR>> f) => new G2<TR>(f);
    }

    public class g
    {
        public static g operator +(g l, g r)
        {
            throw new NotImplementedException();
        }
    }

    public class gc
    {
        object Value;
         
        public gc(object value)
        {
            Value = value;
        }

        public static gc operator +(gc l, gc r)
        {
            throw new NotImplementedException();
        }
    }

    public class F<TR>
    {
        System.Func<TR> Func;

        public F()
        {
            Func = () => default(TR);
        }

        public F(System.Func<TR> f)
        {
            Func = f;
        }

        public TR Invoke()
        {
            return Func();
        }

        public static TR operator !(F<TR> f)
        {
            return f.Func();
        }

        public static implicit operator System.Func<TR>(F<TR> f)
        {
            return f.Func;
        }
    }

    public class F<T1, TR>
    {
        System.Func<T1, TR> Func;

        public F()
        {
            Func = (x) => default(TR);
        }

        public F(System.Func<T1, TR> f)
        {
            Func = f;
        }

        public F<TR> this[T1 a]
        {
            get
            {
                return this + a;
            }
        }

        public TR Invoke(T1 a)
        {
            return Func(a);
        }

        public static implicit operator System.Func<T1, TR>(F<T1, TR> f)
        {
            return f.Func;
        }

        public static F<TR> operator +(F<T1, TR> f, T1 a)
        {
            return new F<TR>(() => f.Func(a));
        }
    }

    public class F<T1, T2, TR>
    {
        System.Func<T1, T2, TR> Func;

        public F()
        {
            Func = (a, b) => default(TR);
        }

        public F(System.Func<T1, T2, TR> f)
        {
            Func = f;
        }

        public TR Invoke(T1 a, T2 b)
        {
            return Func(a, b);
        }

        public F<T2, TR> this[T1 a]
        {
            get
            {
                return this + a;
            }
        }

        public F<TR> this[T1 a, T2 b]
        {
            get
            {
                return this + a + b;
            }
        }

        public F<TR> this[Tuple<T1, T2> t]
        {
            get
            {
                return this + t.Item1 + t.Item2;
            }
        }

        public F<T1, T2, TR2> Then<TR2>(F<TR, TR2> f)
        {
            return F.New((T1 a, T2 b) => f.Invoke(Func(a, b)));
        }

        public static implicit operator System.Func<T1, T2, TR>(F<T1, T2, TR> f)
        {
            return f.Func;
        }

        public static implicit operator F<T1, T2, TR>(System.Func<T1, T2, TR> f)
        {
            return new F<T1, T2, TR>(f);
        }

        public static F<T2, TR> operator +(F<T1, T2, TR> f, T1 a)
        {
            return new F<T2, TR>(x => f.Func(a, x));
        }

        public static F<T2, TR> operator +(F<T1, T2, TR> f, Tuple<T1> a)
        {
            return new F<T2, TR>(x => f.Func(a.Item1, x));
        }

        public static F<TR> operator +(F<T1, T2, TR> f, Tuple<T1, T2> all)
        {
            return new F<TR>(() => f.Func(all.Item1, all.Item2));
        }

        public static F<T1, T2, TR> operator |(F<T1, T2, TR> l, F<TR, TR> r)
        {
            return new F<T1, T2, TR>((a, b) => r.Invoke(l.Invoke(a, b)));
        }

        public static F<T1, T2, TR> operator |(F<T1, T2, TR> l, System.Func<TR, TR> r)
        {
            return new F<T1, T2, TR>((a, b) => r(l.Invoke(a, b)));
        }
    }

    public class G0<TR>
    {
        Expression<Func<TR>> Exp;

        public G0(Expression<Func<TR>> exp)
        {
            Exp = exp;
        }

        public static TR operator !(G0<TR> f)
        {
            var cf = f.Exp.Compile();
            return cf();
        }
    }

    public class G1<TR>
    {
        Expression<Func<g, TR>> Exp;

        public G1(Expression<Func<g, TR>> exp)
        {
            Exp = exp;
        }

        public G0<TR> this[object a]
        {
            get
            {
                return this + a;
            }
        }

        public static G0<TR> operator +(G1<TR> f, object a)
        {
            var visitor = new CurryExpressionVisitor(a);
            var newExpression = (Expression<Func<TR>>)visitor.Visit(f.Exp);

            return new G0<TR>(newExpression);
        }
    }

    public class G2<TR>
    {
        Expression<Func<g, g, TR>> Exp;

        public G2(Expression<Func<g, g, TR>> exp)
        {
            Exp = exp;
        }

        public G1<TR> this[object a]
        {
            get
            {
                return this + a;
            }
        }

        public G0<TR> this[object a, object b]
        {
            get
            {
                return this + a + b;
            }
        }

        public static G1<TR> operator +(G2<TR> f, object a)
        {
            var visitor = new CurryExpressionVisitor(a);
            var newExpression = (Expression<Func<g, TR>>)visitor.Visit(f.Exp);

            return new G1<TR>(newExpression);
        }
    }

    public class ContainerExpressionVisitor : ExpressionVisitor
    {
        protected override Expression VisitParameter(ParameterExpression node)
        {
            if(node.Type == typeof(g))
            {
                return Expression.Parameter(typeof(gc), )
            }
            return base.VisitParameter(node);
        }
    }

    public class CurryExpressionVisitor : ExpressionVisitor
    {
        object NewValue;
        Func<ParameterExpression, bool> IsFirst;

        public CurryExpressionVisitor(object newValue)
        {
            NewValue = newValue;
        }

        //Remove first parameter
        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            IsFirst = x => x == node.Parameters[0];

            var newBody = Visit(node.Body);
            var newParameters = node.Parameters.Skip(1);
            return Expression.Lambda(newBody, newParameters);
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            if (IsFirst(node)) return Expression.Constant(new gc(NewValue));
            return node;
        }
    }
}
