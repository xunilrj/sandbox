using System;

public static class _
{
    public static Func<T, T> Id<T>()
    {
        return x => x;
    }

    public static Func<T, T> Tap<T>(Action<T> action)
    {
        return (x) =>
        {
            action?.Invoke(x);
            return x;
        };
    }

    public static Func<T2, TR> PartialApply<T1, T2, TR>(Func<T1, T2, TR> f, T1 t1)
    {
        return (t2 =>
        {
            return f(t1, t2);
        });
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

public static class Program
{
    static int Times(int a, int b)
    {
        return a * b;
    }

    static Maybe<int> Times(Maybe<int> a, Maybe<int> b)
    {
        return Maybe<int>.Unwrap(a.Bind(ax =>
        {
            return b.Bind(bx => Times(ax, bx));
        }));
    }

    static Maybe<int> Division(int a, int b)
    {
        if (b == 0) return Maybe<int>.None;
        return a / b;
    }

    public static int Main(string[] argv)
    {
        var result = Division(5, 0);
        result.Bind(_.Tap((int x) => { Console.WriteLine(x); }));

        result = Division(5, 1);
        result.Bind(_.Tap((int x) => { Console.WriteLine(x); }));

        result = Times(Division(5, 1), 10);
        result.Bind(_.Tap((int x) => { Console.WriteLine(x); }));

        var f = _.Pipe(
          _.PartialApply<int, int, Maybe<int>>(Division, 5),
          _.PartialApply<Maybe<int>, Maybe<int>, Maybe<int>>(
            Times, 10
          )
        );
        result = f(1);
        result.Bind(_.Tap((int x) => { Console.WriteLine(x); }));
        return 0;
    }
}
