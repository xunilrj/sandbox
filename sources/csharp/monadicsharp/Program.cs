using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

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
}

public static class TaskExtensions
{
    public static Func<Task<T1>, Task<T2>, Task<TR>> Taskify<T1, T2, TR>(this Func<T1, T2, TR> f)
    {
        return async (l, r) =>
        {
            var lvalue = await l;
            var rvalue = await r;
            return f(lvalue, rvalue);
        };
    }

    public static TaskAwaiter<Task<int>> GetAwaiter(this int value)
    {
        return Task.FromResult(Task.FromResult(value)).GetAwaiter();
    }
}

public static class FuncionalExtensions
{
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

    public static class MaybeApproach
    {
        public static Maybe<int> Division(int a, int b)
        {
            if (b == 0) return Maybe<int>.None;
            return a / b;
        }
    }

    public static class TaskApproach
    {
        public static Task<int> Division(int a, int b)
        {
            if (b == 0) return Task.FromException<int>(new DivideByZeroException());
            return Task.FromResult(a / b);
        }
    }

    static void WriteNumber(int x)
    {
        Console.WriteLine(x);
    }

    static void RunMaybeApproach()
    {
        // Traditional Monadic Approach for C# using a Maybe class
        var division = new Func<int, int, Maybe<int>>(MaybeApproach.Division);
        var times = new Func<int, int, int>(Times).Maybeify();
        var write = new Action<int>(WriteNumber);
        var writeChainable = write.Tap();

        var result = division(5, 0);
        result.Bind(writeChainable); // Write here is never called!

        result = division(5, 1);
        result.Bind(writeChainable);

        result = times(division(5, 1), 10);
        result.Bind(writeChainable);

        var f = _.Pipe(division.PartialApply(5), times.PartialApply(10));
        result = f(1);
        result.Bind(writeChainable);

        result = f(0);
        result.Bind(writeChainable); // Write here is never called!
    }

    static void RunTaskApproach()
    {
        //Monadic Approach with Task
        var division = new Func<int, int, Task<int>>(TaskApproach.Division);
        var times = new Func<int, int, int>(Times).Taskify();
        var write = new Action<int>(WriteNumber);

        Task.Factory.StartNew(async () =>
        {
            var result = await division(5, 0);
            WriteNumber(result); // Write here is never called!
        }).Wait();

        Task.Factory.StartNew(async () =>
        {
            var result = await division(5, 1);
            WriteNumber(result);
        }).Wait();

        Task.Factory.StartNew(async () =>
        {
            var result = await times(division(5, 1), await 10);
            WriteNumber(result);
        }).Wait();

        Task.Factory.StartNew(async () =>
        {
            var f = _.Pipe(division.PartialApply(5), times.PartialApply(await 10));
            var result = await f(1);
            WriteNumber(result);
        }).Wait();

        Task.Factory.StartNew(async () =>
        {
            var f = _.Pipe(division.PartialApply(5), times.PartialApply(await 10));
            var result = await f(0);
            WriteNumber(result);
        }).Wait();
    }

    public static int Main(string[] argv)
    {
        RunMaybeApproach();
        RunTaskApproach();

        return 0;
    }
}
