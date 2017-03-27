using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Collections.Concurrent;

[TestClass]
public class Tests
{
    [TestMethod]
    public void ParseChar()
    {
        var pA = Parsers.Char('A');

        var result = pA.Parse("A");

        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("A", result.Value.ToString());
    }

    [TestMethod]
    public void ParseTwoCharsOperator()
    {
        var pA = Parsers.Char('A');
        var pB = Parsers.Char('B');
        var pAB = pA & pB;

        var result = pAB.Parse("AB");

        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("AB", result.Value.ToString());
    }

    [TestMethod]
    public void ParseString()
    {
        var pAB = Parsers.String("AB");

        var result = pAB.Parse("AB");

        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("AB", result.Value.ToString());
    }

    [TestMethod]
    public void ParseOrElse()
    {
        var pA = Parsers.Char('A');
        var pB = Parsers.Char('B');
        var pAorB = Parsers.OrElse(pA, pB);

        var result = pAorB.Parse("A");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("A", result.Value.ToString());

        result = pAorB.Parse("B");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("B", result.Value.ToString());

        result = pAorB.Parse("C");
        Assert.AreEqual(ParserResultType.Failure, result.Type);
        Assert.IsNull(result.Value);
    }

    [TestMethod]
    public void ParseChoice()
    {
        var pA = Parsers.Char('A');
        var pB = Parsers.Char('B');
        var pC = Parsers.Char('C');
        var pAorBorC = Parsers.Choice(new[] { pA, pB, pC });

        var result = pAorBorC.Parse("A");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("A", result.Value.ToString());

        result = pAorBorC.Parse("B");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("B", result.Value.ToString());

        result = pAorBorC.Parse("C");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("C", result.Value.ToString());

        result = pAorBorC.Parse("D");
        Assert.AreEqual(ParserResultType.Failure, result.Type);
    }

    [TestMethod]
    public void ParseAnyOf()
    {
        var pAorBorC = Parsers.AnyOf("ABC");

        var result = pAorBorC.Parse("A");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("A", result.Value.ToString());

        result = pAorBorC.Parse("B");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("B", result.Value.ToString());

        result = pAorBorC.Parse("C");
        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual("C", result.Value.ToString());

        result = pAorBorC.Parse("D");
        Assert.AreEqual(ParserResultType.Failure, result.Type);
    }

    [TestMethod]
    public void ParseStringToInt()
    {
        var p123 = Parsers.String("123");
        var result = p123.Parse("123");

        var pInt = p123.Map(x => int.Parse(x.ToString()));
        var resultInt = pInt.Parse("123");

        Assert.AreEqual(123, resultInt.Value);
    }

    [TestMethod]
    public void LiftedSumTest()
    {
        var sum = new Func<int, int, int>((l, r) => l + r);
        var toInt = new Func<StringBuilder, int>(x => int.Parse(x.ToString()));

        var p1 = Parsers.Char('1').Map(toInt);
        var sumParser = Parsers.Lift2(sum, p1, p1);

        var result = sumParser.Parse("11");

        Assert.AreEqual(ParserResultType.Success, result.Type);
        Assert.AreEqual(2, result.Value);
    }

    interface IA
    {

    }

    public class A : IA
    {

    }

    public class B : A
    {

    }


    [TestMethod]
    public void OpenMethodsTestAction1()
    {
        var builder = new StringBuilder();
        OpenMethods.AddAction("PrintIA", (IA a) => builder.AppendLine("IA"));
        OpenMethods.AddAction("PrintIA", (A a) => builder.AppendLine("A"));
        OpenMethods.AddAction("PrintIA", (B a) => builder.AppendLine("B"));

        var printIA = OpenMethods.GetAction1<IA>("PrintIA");
        printIA(new A());
        printIA(new B());
    }

    [TestMethod]
    public void OpenMethodsTestFunc2()
    {
        var builder = new StringBuilder();
        OpenMethods.AddFunc("Sum", (int a, int b) => a + b);
        OpenMethods.AddFunc("Parse", (string a) => int.Parse(a));

        var f = OpenMethods.GetFunc2<int, int, int>("Sum");
        var p = OpenMethods.GetFunc1<string, int>("Parse");

        var r = f(p("1"), p("2"));
        Assert.AreEqual(3, r);

        r = ParseSum("1", "2", "3", "4");
        Assert.AreEqual(10, r);
    }

    int ParseSum(params string[] strings)
    {
        return OpenMethods.MapReduce<string, int>("Parse", "Sum")(strings);
    }
}

public static class MonadParser<T, TValue>
{
    /// <summary>
    /// \eta \colon 1_{C}\to T (where {\displaystyle 1_{C}} 1_{C} denotes the identity functor on {\displaystyle C} C)
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    /// <returns></returns>
    public static Parser<T, TValue> Eta()
    {
        return Parser<T, TValue>.Nothing;
    }

    /// <summary>
    /// \mu \colon T^{2}\to T (where {\displaystyle T^{2}} T^{2} is the functor {\displaystyle T\circ T} T\circ T from {\displaystyle C} C to {\displaystyle C} C).
    /// </summary>
    /// <param name="l"></param>
    /// <param name="r"></param>
    /// <returns></returns>
    public static Parser<T, TValue> Mu(Parser<T, TValue> l, Parser<T, TValue> r)
    {
        return l.AndThen(r);
    }
}

public static class Parsers
{
    public static Parser<T, T> Func<T>(Func<T, bool> isValid)
    {
        return new Parser<T, T>(result =>
        {
            var current = result.Stream.Current;
            if (isValid(current))
            {
                result.Stream.MoveNext();
                result.Type = ParserResultType.Success;
                result.Value = current;
            }
            else
            {
                result.Type = ParserResultType.Failure;
                result.Messages.Add($"Not expecting {current}.");
            }
        });
    }

    public static Parser<T, TValue> Func<T, TValue>(Func<T, bool> isValid, Func<TValue, T, TValue> map) where TValue : class, new()
    {
        return new Parser<T, TValue>(result =>
        {
            var current = result.Stream.Current;
            if (isValid(current))
            {
                result.Stream.MoveNext();
                result.Type = ParserResultType.Success;
                result.Value = map(result.Value ?? new TValue(), current);
            }
            else
            {
                result.Type = ParserResultType.Failure;
                result.Messages.Add($"Not expecting {current}.");
            }
        });
    }

    public static Parser<char, StringBuilder> Char(char c)
    {
        return Func<char, StringBuilder>(current => current == c, (builder, newChar) => builder.Append(newChar));
    }

    public static Parser<char, StringBuilder> AnyOf(string chars)
    {
        return Choice(chars.Select(Parsers.Char));
    }

    public static Parser<char, StringBuilder> String(string str)
    {
        return str
            .Select(Parsers.Char)
            .Reduce(AndThen);
    }

    public static Parser<T, TValue> AndThen<T, TValue>(this Parser<T, TValue> l, Parser<T, TValue> r)
    {
        l = l ?? Parser<T, TValue>.Nothing;
        r = r ?? Parser<T, TValue>.Nothing;
        return new Parser<T, TValue>(x =>
        {
            l.ParseFunction(x);
            if (x.Type == ParserResultType.Success) r.ParseFunction(x);
        });
    }

    public static Parser<T, TC> AndThen<T, TA, TB, TC>(this Parser<T, TA> l, Parser<T, TB> r, Func<TA, TB, TC> map)
    {
        l = l ?? Parser<T, TA>.Nothing;
        r = r ?? Parser<T, TB>.Nothing;
        return new Parser<T, TC>(x =>
        {
            var pr1 = new ParserResult<T, TA>() { Stream = x.Stream };

            l.ParseFunction(pr1);
            if (pr1.Type == ParserResultType.Success)
            {
                var pr2 = new ParserResult<T, TB>() { Stream = x.Stream };
                r.ParseFunction(pr2);

                if (pr2.Type == ParserResultType.Success)
                {
                    x.Type = ParserResultType.Success;
                    x.Value = map(pr1.Value, pr2.Value);
                }
            }
        });
    }

    public static Parser<T, TValue> OrElse<T, TValue>(this Parser<T, TValue> l, Parser<T, TValue> r)
    {
        l = l ?? Parser<T, TValue>.Fail;
        r = r ?? Parser<T, TValue>.Fail;
        return new Parser<T, TValue>(x =>
        {
            l.ParseFunction(x);
            if (x.Type == ParserResultType.Failure)
            {
                x.Messages.RemoveAt(x.Messages.Count - 1);
                r.ParseFunction(x);
            }
        });
    }

    public static Parser<T, TValue> Choice<T, TValue>(this IEnumerable<Parser<T, TValue>> parsers)
    {
        return parsers.Reduce(OrElse);
    }

    //public static Func<Parser<T, TValue>, Parser<T, TMappedValue>> Map<T, TValue, TMappedValue>(Func<TValue, TMappedValue> map)
    //{
    //    return new Parser<T, TMappedValue>(x =>
    //    {
    //        var pr = ParserResult<T, TValue>.Success(x.Stream);
    //        parser.ParseFunction(pr);

    //        x.Type = pr.Type;
    //        if (pr.Type == ParserResultType.Success)
    //        {
    //            x.Value = map(pr.Value);
    //            x.Messages.AddRange(pr.Messages);
    //            x.Stream = pr.Stream;
    //        }
    //    });
    //}

    public static Parser<T, TMappedValue> Map<T, TValue, TMappedValue>(this Parser<T, TValue> parser, Func<TValue, TMappedValue> map)
    {
        return new Parser<T, TMappedValue>(x =>
        {
            var pr = ParserResult<T, TValue>.Success(x.Stream);
            parser.ParseFunction(pr);

            x.Type = pr.Type;
            if (pr.Type == ParserResultType.Success)
            {
                x.Value = map(pr.Value);
                x.Messages.AddRange(pr.Messages);
                x.Stream = pr.Stream;
            }
        });
    }

    /// <summary>
    /// val returnP : 'a -> Parser<'a>
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    /// <param name="value"></param>
    /// <returns></returns>
    public static Parser<T, TValue> Return<T, TValue>(TValue value)
    {
        return new Parser<T, TValue>(x =>
        {
            x.Type = ParserResultType.Success;
            x.Value = value;
        });
    }

    /// <summary>
    /// val applyP : Parser<('a -> 'b)> -> Parser<'a> -> Parser<'b>
    /// </summary>
    public static Parser<T, TB> Apply<T, TA, TB>(this Parser<T, Func<TA, TB>> f, Parser<T, TA> pa)
    {
        //let applyP fP xP =
        //    // create a Parser containing a pair (f,x)
        //    (fP.>>.xP)
        //    // map the pair by applying f to x
        //    |> mapP(fun(f, x)->f x)

        return f.AndThen(pa, (l, r) => l(r));
    }

    /// <summary>
    /// f:('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c>
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <typeparam name="TA"></typeparam>
    /// <typeparam name="TB"></typeparam>
    /// <returns></returns>
    public static Parser<T, TC> Lift2<T, TA, TB, TC>(Func<TA, TB, TC> f, Parser<T, TA> l, Parser<T, TB> r)
    {
        return Return<T, Func<TA, TB, TC>>(f)
            .AndThen(l, Tuple.Create)
            .AndThen(r, (tuple, rv) => tuple.Item1(tuple.Item2, rv));
    }
}

public class Parser<TStream, TValue>
{
    public Action<ParserResult<TStream, TValue>> ParseFunction;

    public Parser(Action<ParserResult<TStream, TValue>> parse)
    {
        ParseFunction = parse;
    }

    public ParserResult<TStream, TValue> Parse(IEnumerable<TStream> stream)
    {
        var enumerator = stream.GetEnumerator();

        var result = new ParserResult<TStream, TValue>()
        {
            Type = ParserResultType.Success,
            Stream = enumerator
        };

        if (enumerator.MoveNext() == false)
        {
            result.Type = ParserResultType.Failure;
            result.Messages.Add("End of Stream.");
        }

        ParseFunction(result);

        return result;
    }

    public static Parser<TStream, TValue> operator &(Parser<TStream, TValue> l, Parser<TStream, TValue> r)
    {
        return Parsers.AndThen(l, r);
    }

    public static Parser<TStream, TValue> Nothing
    {
        get
        {
            return new Parser<TStream, TValue>(x =>
            {
            });
        }
    }

    public static Parser<TStream, TValue> Fail
    {
        get
        {
            return new Parser<TStream, TValue>(x =>
            {
                x.Type = ParserResultType.Failure;
                x.Messages.Add("");
            });
        }
    }
}

public enum ParserResultType
{
    Success,
    Failure
}

public class ParserResult<T, TValue>
{
    public static ParserResult<T, TValue> Success(IEnumerator<T> stream)
    {
        return new ParserResult<T, TValue>()
        {
            Type = ParserResultType.Success,
            Stream = stream
        };
    }

    public ParserResultType Type { get; set; }
    public IEnumerator<T> Stream { get; set; }

    public List<String> Messages { get; set; }

    public TValue Value { get; set; }

    public ParserResult()
    {
        Messages = new List<string>();
    }
}

public static class Curry
{
    public static Func<TB, Func<TA, TC>> Inverse<TA, TB, TC>(Func<TA, TB, TC> f1)
    {
        return new Func<TB, Func<TA, TC>>(b => new Func<TA, TC>(a => f1(a, b)));
    }
}

public static class LinqExtensions
{
    /// <summary>
    /// foldl :: (a -> b -> a) -> a -> [b] -> a
    /// http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:foldl
    /// </summary>
    /// <typeparam name="T1"></typeparam>
    /// <typeparam name="T2"></typeparam>
    /// <param name="items"></param>
    /// <param name="binaryOperator"></param>
    /// <returns></returns>
    public static TA FoldLeft<TA, TB>(this IEnumerable<TB> items, Func<TA, TB, TA> binaryOperator, TA seed = default(TA))
    {
        return items.Aggregate(seed, binaryOperator, x => x);
    }

    //public static T1 FoldRight<T1>(this IEnumerable<T1> items, Func<T1, T1, T1> binaryOperator)
    //{
    //    return FoldLeft(items.Reverse(), binaryOperator);
    //}

    public static TA Reduce<TA>(this IEnumerable<TA> items, Func<TA, TA, TA> binaryOperator)
    {
        return FoldLeft(items, binaryOperator);
    }

    public static Func<IEnumerable<TA>, TA> Reduce<TA>(Func<TA, TA, TA> binaryOperator)
    {
        return new Func<IEnumerable<TA>, TA>(x => Reduce(x, binaryOperator));
    }
}

public class ParserAndThen<TA, TB, TC> : IFunc<TA, TB, TC>
{
    public TA LeftIdentity
    {
        get { return default(TA); }
    }

    public TC Run(TA a, TB b)
    {
        throw new NotImplementedException();
    }
}

public interface IFunc<TA, TB>
{
    TB Run(TA a);
}

public interface IFunc<TA, TB, TC>
{
    TC Run(TA a, TB b);
}

public interface IBinaryOperator<TA, TB, TC> : IFunc<TA, TB, TC>
{

}

public class Return<TA, TB> : IFunc<TA, TB>
{
    public TB Run(TA a)
    {
        throw new NotImplementedException();
    }
}

///// <summary>
///// f:('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c>
///// </summary>
///// <typeparam name="TA"></typeparam>
///// <typeparam name="TB"></typeparam>
//public class Lift<TA, TB, TC, TA2, TB2, TC2> : IFunc<IBinaryOperator<TA, TB, TC>, IBinaryOperator<TA2, TB2, TC2>>
//{
//    public IBinaryOperator<TA2, TB2, TC2> Run(IBinaryOperator<TA, TB, TC> a)
//    {
//        return new Return.Run(a)
//            .AndThen(l, Tuple.Create)
//            .AndThen(r, (tuple, rv) => tuple.Item1(tuple.Item2, rv));
//    }
//}

//public class Sum : IBinaryOperator<int, int, int>
//{
//    public int Run(int a, int b)
//    {
//        return a + b;
//    }
//}

public static class OpenMethods
{
    static Dictionary<string, List<object>> Actions1 = new Dictionary<string, List<object>>();
    static Dictionary<string, List<object>> Func1 = new Dictionary<string, List<object>>();
    static Dictionary<string, List<object>> Func2 = new Dictionary<string, List<object>>();

    public static void AddAction<TA>(string name, Action<TA> f)
    {
        List<object> actions = null;

        if (Actions1.TryGetValue(name, out actions) == false)
        {
            actions = new List<object>();
            Actions1.Add(name, actions);
        }

        actions.Add(f);
    }

    public static void AddFunc<TA, TB>(string name, Func<TA, TB> f)
    {
        List<object> actions = null;

        if (Func1.TryGetValue(name, out actions) == false)
        {
            actions = new List<object>();
            Func1.Add(name, actions);
        }

        actions.Add(f);
    }

    public static void AddFunc<TA, TB, TC>(string name, Func<TA, TB, TC> f)
    {
        List<object> actions = null;

        if (Func2.TryGetValue(name, out actions) == false)
        {
            actions = new List<object>();
            Func2.Add(name, actions);
        }

        actions.Add(f);
    }

    public static Action<T> GetAction1<T>(string name)
    {
        return new Action<T>(arg =>
        {
            var candidates = Actions1[name].Select(x => new
            {
                Delegate = x,
                Score = GetScore(arg, x)
            }).OrderByDescending(x => x.Score);

            var best = candidates.FirstOrDefault();

            if (best != null)
            {
                (best.Delegate as Delegate).DynamicInvoke(arg);
            }
            else
            {
                throw new Exception();
            }
        });
    }

    public static Func<TA, TB> GetFunc1<TA, TB>(string name)
    {
        return new Func<TA, TB>((a) =>
        {
            var candidates = Func1[name].Select(x => new
            {
                Delegate = x,
                Score = GetScore(a, x)
            }).OrderByDescending(x => x.Score);

            var best = candidates.FirstOrDefault();

            if (best != null)
            {
                return (TB)(best.Delegate as Delegate).DynamicInvoke(a);
            }
            else
            {
                throw new Exception();
            }
        });
    }

    public static Func<TA, TB, TC> GetFunc2<TA, TB, TC>(string name)
    {
        return new Func<TA, TB, TC>((a, b) =>
        {
            var candidates = Func2[name].Select(x => new
            {
                Delegate = x,
                Score = GetScore(a, b, x)
            }).OrderByDescending(x => x.Score);

            var best = candidates.FirstOrDefault();

            if (best != null)
            {
                return (TC)(best.Delegate as Delegate).DynamicInvoke(a, b);
            }
            else
            {
                throw new Exception();
            }
        });
    }

    internal static Func<IEnumerable<T1>, T2> MapReduce<T1, T2>(string map, string reduce)
    {
        var mapf = GetFunc1<T1, T2>(map);
        var reducef = GetFunc2<T2, T2, T2>(reduce);

        return new Func<IEnumerable<T1>, T2>((ls) => ls.Select(lsi => mapf(lsi)).Reduce(reducef));
    }

    private static float GetScore<T>(T a, object x)
    {
        var arg1 = x.GetType().GetGenericArguments()[0];
        if (a.GetType() == arg1)
        {
            return 1.0f;
        }
        else
        {
            return 0.0f;
        }
    }

    private static float GetScore<TA, TB>(TA a, TB b, object x)
    {
        var arg1 = x.GetType().GetGenericArguments()[0];
        var arg2 = x.GetType().GetGenericArguments()[2];
        if ((a.GetType() == arg1) && (b.GetType() == arg2))
        {
            return 1.0f;
        }
        else
        {
            return 0.0f;
        }
    }
}
