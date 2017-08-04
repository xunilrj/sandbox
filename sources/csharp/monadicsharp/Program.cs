using System;

public static class _
{
    public static Func<T, T> Id<T>()
    {
        return x => x;
    }





    public static Func<TIn, TOut> Pipe<TIn, TR1, TOut>(Func<TIn, TR1> f1, Func<TR1, TOut> f2)
    {
        return (x =>
        {
            return f2(f1(x));
        });
    }


}

public struct Maybe<T>
{
    public static Maybe<T> None => new Maybe<T>(false);

    bool HasValue;
    T Value;

    private Maybe(T value) : this()
    {
        HasValue = true;
        Value = value;
    }

    private Maybe(bool HasValue) : this()
    {
        HasValue = false;
        Value = default(T);
    }

    public Maybe<V> Bind<V>(Func<T, V> bind)
    {
        if (HasValue == false) return Maybe<V>.None;
        else
        {
            var newvalue = bind(Value);
            return newvalue == null ? Maybe<V>.None : new Maybe<V>(newvalue);
        }
    }

    public static implicit operator Maybe<T>(T value)
    {
        return new Maybe<T>(value);
    }

    public static Maybe<T> Unwrap(Maybe<Maybe<T>> m)
    {
        if (m.HasValue) return m.Value;
        return None;
    }
}

public static class MaybeExtensions
{
    public static Func<Maybe<T1>, Maybe<T2>, Maybe<TR>> Maybeify<T1, T2, TR>(this Func<T1, T2, TR> f)
    {
        return (l, r) =>
        {
            return Maybe<TR>.Unwrap(l.Bind(lvalue => r.Bind(rvalue => f(lvalue, rvalue))));
        };
    }

    public static Func<T2, TR> PartialApply<T1, T2, TR>(this Func<T1, T2, TR> f, T1 t1)
    {
        return (t2 =>
        {
            return f(t1, t2);
        });
    }

    public static Func<T, T> Tap<T>(this Action<T> action)
    {
        return (x) =>
        {
            action?.Invoke(x);
            return x;
        };
    }
}

public static class Program
{
    static int Times(int a, int b)
    {
        return a * b;
    }

    static Maybe<int> Division(int a, int b)
    {
        if (b == 0) return Maybe<int>.None;
        return a / b;
    }

    public static int Main(string[] argv)
    {
        // Traditional Monadic Approach for C#
        var division = new Func<int, int, Maybe<int>>(Division);
        var times = new Func<int, int, int>(Times).Maybeify();
        var write = new Action<int>(x => Console.WriteLine(x.ToString()));
        var writeChainable = write.Tap();

        var result = Division(5, 0);
        result.Bind(writeChainable); // Write here is never called!

        result = Division(5, 1);
        result.Bind(writeChainable);

        result = times(Division(5, 1), 10);
        result.Bind(writeChainable);

        var f = _.Pipe(division.PartialApply(5), times.PartialApply(10));
        result = f(1);
        result.Bind(writeChainable);

        result = f(0);
        result.Bind(writeChainable); // Write here is never called!
        return 0;
    }
}
