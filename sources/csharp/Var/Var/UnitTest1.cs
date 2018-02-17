using System;
using System.Threading.Tasks;
using Xunit;
using static __;
using wstring = Var.w<string>;
using wint = Var.w<int>;

namespace Var
{
    public class UnitTest1
    {
        [Fact]
        public void WrapAndAction()
        {
            PrintVar(1);
        }

        [Fact]
        public empty WrapAndFunction()
        {
            var r1 = ToStringVar(1);
            return r1 + assert;
            void assert(string x) => Assert.Equal("1", x);
        }

        empty PrintVar(wint a)
        {
            return a + print;
            void print(int x) => System.Console.Write(x);
        }

        wstring ToStringVar(wint i)
        {
            return __.a<int, o<string>>(toString) + i;
            void toString(int x, o<string> r) => r = x.ToString();
        }
    }

    public struct empty
    {
    }

    public static class EmptyExtensions
    {
    }

    public struct o<T>
        {
        }

    public static class maybe
    {
        public static void MaybeBind<T1, T2>(this (a<T1> a1, w<T2> w2) t, Action<Action<T1>, T2> f)
            => Bind(((maybe<Action<T1>>)t.a1, (maybe<T2>)t.w2), f);
        public static T1R MaybeMap<T1, T1R, T2>(this (f<T1, T1R> f1, w<T2> w2) t,
            Func<Func<T1, T1R>, T2, T1R> f)
                    => Map(((maybe<Func<T1, T1R>>)t.f1, (maybe<T2>)t.w2), f);


        public static void Bind<T1, T2>(this (maybe<T1> m1,
            maybe<T2> m2) t, Action<T1, T2> f)
            => Map(t, (a, b) => { f(a, b); return new empty(); });
        public static TR Map<T1, T2, TR>(this (maybe<T1> m1,
                        maybe<T2> m2) t, Func<T1, T2, TR> f)
        {
            var m = t.m1.Map(x1 =>
                   t.m2.Map(x2 => new maybe<(T1, T2)>((x1, x2))));
            return m.Map(x =>
            {
                var (v1, v2) = x;
                return f(v1, v2);
            });
        }

        public static void Bind<T1, T2, T3>(this (maybe<T1> m1,
                        maybe<T2> m2,
                        maybe<T3> m3) t, Action<T1, T2, T3> f)
            => Map(t, (a, b, c) => { f(a, b, c); return new empty(); });
        public static TR Map<T1, T2, T3, TR>(this (maybe<T1> m1,
                maybe<T2> m2,
                maybe<T3> m3) t, Func<T1, T2, T3, TR> f)
        {
            var m = t.m1.Map(x1 =>
                   t.m2.Map(x2 =>
                   t.m3.Map(x3 => new maybe<(T1, T2, T3)>((x1, x2, x3)))));
            return m.Map(x =>
            {
                var (v1, v2, v3) = x;
                return f(v1, v2, v3);
            });
        }
    }
    public struct maybe<T>
    {
        T Value;
        public maybe(T v) => Value = v;
        public TR Map<TR>(Func<T, TR> f)
            => (Value != null) ? f(Value) : (default(TR));
    }

    public struct w<T>
    {
        T Value;
        private w(T v) => Value = v;
        public static implicit operator w<T>(T v) => new w<T>(v);
        public static c<T> operator +(w<T> w, Action<T> f) => (f, w);
        public static c<T> operator +(w<T> w, a<T> f) => (f, w);

        public static explicit operator maybe<T>(w<T> w) => new maybe<T>(w.Value);
    }
    public struct w<T1, T2>
    {
        w<T1> L;
        w<T2> R;
        w(w<T1> l, w<T2> r)
        {
            L = l;
            R = r;
        }
    }

    public struct a<T1>
    {
        Action<T1> Value;
        private a(Action<T1> v) => Value = v;
        public static implicit operator a<T1>(Action<T1> v) => new a<T1>(v);
        public static c<T1> operator +(a<T1> a, w<T1> w) => (a, w);
        public static explicit operator maybe<Action<T1>>(a<T1> a)
            => new maybe<Action<T1>>(a.Value);
    }
    public struct a<T1, T2>
    {
        Action<T1, T2> Value;
        private a(Action<T1, T2> v) => Value = v;
        public static implicit operator a<T1, T2>(Action<T1, T2> v) => new a<T1, T2>(v);
        public static c<T1, T2> operator +(a<T1, T2> f, (w<T1> p1, w<T2> p2) v)
            => (f, v.p1, v.p2);
        public static explicit operator maybe<Action<T1, T2>>(a<T1, T2> a)
                    => new maybe<Action<T1, T2>>(a.Value);
    }

    public struct f<T1, TR>
    {
        Func<T1, TR> Value;
        private f(Func<T1, TR> v) => Value = v;
        public static implicit operator f<T1, TR>(Func<T1, TR> v) => new f<T1, TR>(v);
        public static cf<T1, TR> operator +(f<T1, TR> f, w<T1> w) => (f, w);
        public static explicit operator maybe<Func<T1, TR>>(f<T1, TR> f) =>
            new maybe<Func<T1, TR>>(f.Value);
    }

    public struct c<T1>
    {
        w<T1> W; a<T1> A;
        c(a<T1> a, w<T1> w) { W = w; A = a; }
        public static implicit operator empty(c<T1> c)
        {
            (c.A, c.W).MaybeBind((f, a) => f(a));
            return new empty();
        }
        public static implicit operator c<T1>((a<T1> a, w<T1> w) t) => new c<T1>(t.a, t.w);
    }
    public struct c<T1, T2>
    {
        w<T1> W1; w<T2> W2; a<T1, T2> A;
        c(a<T1, T2> a, w<T1> w1, w<T2> w2) { A = a; W1 = w1; W2 = w2; }
        public static implicit operator empty(c<T1, T2> c)
        {
            ((maybe<Action<T1, T2>>)c.A,
                (maybe<T1>)c.W1,
                (maybe<T2>)c.W2).Bind((f, a, b) =>
            {
                f(a, b);
            });
            return new empty();
        }
        public static implicit operator c<T1, T2>((a<T1, T2> a, w<T1> w1, w<T2> w2) t)
            => new c<T1, T2>(t.a, t.w1, t.w2);
    }

    public struct cf<T1, TR>
    {
        w<T1> W; f<T1, TR> F;
        cf(f<T1, TR> f, w<T1> w) { W = w; F = f; }
        public static implicit operator TR(cf<T1, TR> c) => (c.F, c.W).MaybeMap((f, a) => f(a));
        public static implicit operator w<TR>(cf<T1, TR> c) => (TR)c;
        public static implicit operator cf<T1, TR>((f<T1, TR> a, w<T1> w) t) => new cf<T1, TR>(t.a, t.w);
    }

    public static class CFExtensions
    {
        public static cf<T1, TR> Apply<T1, TR>((Func<T1, TR> f, w<T1> w) t)
            => ((f<T1, TR>)t.f, t.w);
    }
}


public static class VarExtensions
{
    public static void Join<T, TR>(Func<T, TR> f)
    {
    }
}

public static class __
{
    public static Var.a<T1> a<T1>(this Action<T1> f) => f;
    public static Var.a<T2> a<T1, T2>(this Action<T1, T2> f, T1 v) => new Action<T2>(x => f(v, x));
    public static Var.f<T1, TR> _<T1, TR>(this Func<T1, TR> f) => f;
    public static Var.a<T1, TR> a<T1, TR>(this Action<T1, TR> f) => f;

}
