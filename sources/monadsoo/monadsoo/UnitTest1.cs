using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace monadsoo
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var list1 = new[] { 1, 2, 3 };

            var sum1 = new Sum1();
            var list2 = new Map<int, int>().Invoke(sum1, list1).ToArray();

            Assert.AreEqual(2, list2[0]);
            Assert.AreEqual(3, list2[1]);
            Assert.AreEqual(4, list2[2]);

            var zippedList = new Zip<int, int>().Invoke(list1, list2).ToArray();

            Assert.AreEqual(1, zippedList[0].Item1);
            Assert.AreEqual(2, zippedList[0].Item2);

            var foldrSum = new Foldr<int, int>().Invoke(new Sum(), 0, list1);
            Assert.AreEqual(6, foldrSum);

            var foldlSum = new Foldl<int, int>().Invoke(new Sum(), 0, list1);
            Assert.AreEqual(6, foldlSum);

            var concatList = new Concat<int>().Invoke(new[] { list1, list2 }).ToArray();
            Assert.AreEqual(6, concatList.Count());
            Assert.AreEqual(1, concatList[0]);
            Assert.AreEqual(4, concatList[5]);

            var reversedConcatList = new Reverse<int>().Invoke(concatList).ToList();
            Assert.AreEqual(4, reversedConcatList[0]);
            Assert.AreEqual(1, reversedConcatList[5]);
            ////var counter = new Counter<int>();

            ////var monad = ListMonad.Create(list);
            ////var result = monad.Last(counter);

            ////Assert.AreEqual(3, result);

            //var sum1 = new Sum1();
            //Assert.AreEqual(2, sum1.Invoke(1));

            //var maybeSum1 = Maybe.Create(sum1);
            //var some1 = Some.Create(1);
            //maybeSum1.Invoke(some1).If(Lambda.Create((int x) =>
            //{
            //    Assert.AreEqual(2, x);
            //    return false;
            //}));

            //var assertMaybe = Lambda.Create()


            //var result = maybeSum1.Invoke(Maybe.Null);
            //Assert.IsInstanceOfType(result, typeof(Null<int>));
        }
    }

    //public class A { }
    //public class B : A { }

    //public abstract class IMaybe<T>
    //{
    //    public abstract IMaybe<TR> If<TR>(IFunc<T, TR> success);
    //    //public static implicit operator IMaybe<T>(T item)
    //    //{
    //    //    return new Maybe<T>(item);
    //    //}

    //    //public static implicit operator T(IMaybe<T> item)
    //    //{
    //    //    return item.GetValue();
    //    //}
    //}

    //public struct Some
    //{
    //    public static Maybe<T> Create<T>(T item)
    //    {
    //        return new Maybe<T>(item);
    //    }
    //}

    //public struct Maybe
    //{
    //    public static Null<object> Null => new Null<object>();

    //    class MaybeImpl<T, TR> : IFunc<IMaybe<T>, IMaybe<TR>>
    //    {
    //        IFunc<T, TR> Inner;

    //        public MaybeImpl(IFunc<T, TR> inner)
    //        {
    //            Inner = inner;
    //        }

    //        public IMaybe<TR> Invoke(IMaybe<T> item)
    //        {
    //            if (item == null || item is Null<T>)
    //            {
    //                return new Null<TR>();
    //            }
    //            else
    //            {
    //                return item.If(Inner);
    //            }
    //        }
    //    }

    //    public static IFunc<IMaybe<T>, IMaybe<TR>> Create<T, TR>(IFunc<T, TR> func)
    //    {
    //        return new MaybeImpl<T, TR>(func);
    //    }
    //}

    //public class Null<T> : IMaybe<T>
    //{
    //    public override IMaybe<TR> If<TR>(IFunc<T, TR> some)
    //    {
    //        return new Null<TR>();
    //    }
    //}

    //public class Maybe<T> : IMaybe<T>
    //{
    //    T Value;

    //    public Maybe(T value)
    //    {
    //        Value = value;
    //    }

    //    public override IMaybe<TR> If<TR>(IFunc<T, TR> some)
    //    {
    //        if (Value != null)
    //        {
    //            return new Maybe<TR>(some.Invoke(Value));
    //        }
    //        else
    //        {
    //            return new Null<TR>();
    //        }
    //    }
    //}

    //public class ListMonad
    //{
    //    public static ListMonad<T> Create<T>(IEnumerable<T> items)
    //    {
    //        return new ListMonad<T>(items);
    //    }
    //}

    //public class ListMonad<T>
    //{
    //    IEnumerable<T> List;

    //    public ListMonad(IEnumerable<T> list)
    //    {
    //        List = list;
    //    }

    //    public ListMonad<TR> Bind<TR>(IFunc<T, TR> bind)
    //    {
    //        var newMonad = new List<TR>();

    //        foreach (var item in List)
    //        {
    //            var result = bind.Invoke(item);
    //            newMonad.Add(result);
    //        }

    //        return new ListMonad<TR>(newMonad);
    //    }

    //    public TR Last<TR>(IFunc<T, TR> cata)
    //    {
    //        var agg = default(TR);

    //        foreach (var item in List)
    //        {
    //            agg = cata.Invoke(item);
    //        }

    //        return agg;
    //    }
    //}

    public class Sum1 : IFunc<int, int>
    {
        public int Invoke(int item)
        {
            return item + 1;
        }
    }

    public class Sum : IFunc<int, int, int>
    {
        public int Invoke(int a, int b)
        {
            return a + b;
        }
    }

    public class Counter<T> : IFunc<T, int>
    {
        int Count = 0;

        public int Invoke(T item)
        {
            return ++Count;
        }
    }

    public interface IAction { void Invoke(); }
    public interface IAction<T1> { void Invoke(T1 a); }
    public interface IAction<T1, T2> { void Invoke(T1 a, T2 b); }
    public interface IAction<T1, T2, T3> { void Invoke(T1 a, T2 b, T3 c); }
    public interface IAction<T1, T2, T3, T4> { void Invoke(T1 a, T2 b, T3 c, T4 d); }
    public interface IAction<T1, T2, T3, T4, T5> { void Invoke(T1 a, T2 b, T3 c, T4 d, T5 e); }

    public interface IFunc<TR> { TR Invoke(); }
    public interface IFunc<T1, TR> { TR Invoke(T1 a); }
    public interface IFunc<T1, T2, TR> { TR Invoke(T1 a, T2 b); }
    public interface IFunc<T1, T2, T3, TR> { TR Invoke(T1 a, T2 b, T3 c); }

    public class LambdaFunc<T1, TR> : IFunc<T1, TR>
    {
        Func<T1, TR> Func;

        public LambdaFunc(Func<T1, TR> func)
        {
            Func = func;
        }

        public TR Invoke(T1 a)
        {
            return Func.Invoke(a);
        }
    }

    public class Lambda
    {
        public static IFunc<T1, TR> Create<T1, TR>(Func<T1, TR> func)
        {
            return new LambdaFunc<T1, TR>(func);
        }
    }

    public class Map<TA, TB> : IFunc<IFunc<TA, TB>, IEnumerable<TA>, IEnumerable<TB>>
    {
        public IEnumerable<TB> Invoke(IFunc<TA, TB> a, IEnumerable<TA> b)
        {
            foreach (var item in b)
            {
                yield return a.Invoke(item);
            }
        }
    }

    public class ZipWith<TA, TB, TC> : IFunc<IFunc<TA, TB, TC>, IEnumerable<TA>, IEnumerable<TB>, IEnumerable<TC>>
    {
        public IEnumerable<TC> Invoke(IFunc<TA, TB, TC> a, IEnumerable<TA> b, IEnumerable<TB> c)
        {
            return b.Zip(c, (bb, cc) => a.Invoke(bb, cc));
        }
    }

    public class TupleCreation<TA, TB> : IFunc<TA, TB, Tuple<TA, TB>>
    {
        public Tuple<TA, TB> Invoke(TA a, TB b)
        {
            return Tuple.Create(a, b);
        }
    }

    public class Zip<TA, TB> : IFunc<IEnumerable<TA>, IEnumerable<TB>, IEnumerable<Tuple<TA, TB>>>
    {
        public IEnumerable<Tuple<TA, TB>> Invoke(IEnumerable<TA> b, IEnumerable<TB> c)
        {
            return new ZipWith<TA, TB, Tuple<TA, TB>>().Invoke(new TupleCreation<TA, TB>(), b, c);
        }
    }

    public class Foldr<TA, TB> : IFunc<IFunc<TA, TB, TB>, TB, IEnumerable<TA>, TB>
    {
        public TB Invoke(IFunc<TA, TB, TB> a, TB b, IEnumerable<TA> c)
        {
            var stack = new Stack<TA>(c);

            var accum = b;

            while (stack.Count > 0)
            {
                var current = stack.Pop();
                accum = a.Invoke(current, accum);
            }

            return accum;
        }
    }

    public class Foldl<TA, TB> : IFunc<IFunc<TB, TA, TB>, TB, IEnumerable<TA>, TB>
    {
        public TB Invoke(IFunc<TB, TA, TB> a, TB b, IEnumerable<TA> c)
        {
            var stack = new Stack<TA>(c);

            var accum = b;

            foreach (var item in c)
            {
                accum = a.Invoke(accum, item);
            }

            return accum;
        }
    }

    public class PlusPlus<TA> : IFunc<IEnumerable<TA>, IEnumerable<TA>, IEnumerable<TA>>
    {
        public IEnumerable<TA> Invoke(IEnumerable<TA> a, IEnumerable<TA> b)
        {
            foreach (var item in a)
            {
                yield return item;
            }

            foreach (var item in b)
            {
                yield return item;
            }
        }
    }

    public class Concat<TA> : IFunc<IEnumerable<IEnumerable<TA>>, IEnumerable<TA>>
    {
        public IEnumerable<TA> Invoke(IEnumerable<IEnumerable<TA>> a)
        {
            return new Foldr<IEnumerable<TA>, IEnumerable<TA>>().Invoke(
                new PlusPlus<TA>(), Enumerable.Empty<TA>(), a);
        }
    }

    public class ReverseOperation<TA> : IFunc<IEnumerable<TA>, TA, IEnumerable<TA>>
    {
        public IEnumerable<TA> Invoke(IEnumerable<TA> a, TA b)
        {
            yield return b;

            foreach (var item in a)
            {
                yield return item;
            }
        }
    }

    public class Reverse<TA> : IFunc<IEnumerable<TA>, IEnumerable<TA>>
    {
        public IEnumerable<TA> Invoke(IEnumerable<TA> a)
        {
            return new Foldl<TA, IEnumerable<TA>>().Invoke(
                new ReverseOperation<TA>(), Enumerable.Empty<TA>(), a);
        }
    }
}
