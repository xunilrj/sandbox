﻿//using System;
//using Microsoft.VisualStudio.TestTools.UnitTesting;
//using StompMessages;
//using System.Diagnostics;
//using static FParsec.CharParsers;
//using System.Text;
//using static StompTests.UnitTest1;
//using System.Collections.Generic;
//using System.Linq;

//namespace StompTests
//{
//    [TestClass]
//    public class UnitTest1
//    {
//        [TestMethod]
//        public void ParseUsingFParsec()
//        {
//            var result = StompParser.StompParser.Parse(GetMessage());
//            var success = result as FParsec.CharParsers.ParserResult<StompMessage, Microsoft.FSharp.Core.Unit>.Success;
//            var msg = success.Item1;

//            Assert.AreEqual("COMMAND", msg.Command);
//            Assert.AreEqual("value1", msg.Headers["headerA"]);
//            Assert.AreEqual("value2", msg.Headers["headerB"]);
//            Assert.AreEqual(GetJson(), msg.Body);
//        }

//        [TestMethod]
//        public void ParseUsingCsharp()
//        {
//            var message = GetMessage();
//            Parse<string>(message);
//            //TODO PARSE
//            var msg = new StompMessage("COMMAND", new[] { Tuple.Create("headerA", "value1"), Tuple.Create("headerB", "value2") }, GetJson());

//            Assert.AreEqual("COMMAND", msg.Command);
//            Assert.AreEqual("value1", msg.Headers["headerA"]);
//            Assert.AreEqual("value2", msg.Headers["headerB"]);
//            Assert.AreEqual(GetJson(), msg.Body);
//        }

//        [TestMethod]
//        public void ParseOneChar()
//        {
//            var parserA = pchar('A');

//            Assert.IsTrue(parserA.Run("A").MatchAsBool());
//            Assert.IsFalse(parserA.Run("B").MatchAsBool());
//        }

//        [TestMethod]
//        public void ParseOneCharAndThenAnotherChar()
//        {
//            var parserA = pchar('A');
//            var parserB = pchar('B');

//            var parserAB = parserA & parserB;

//            Assert.IsTrue(parserAB.Run("AB").MatchAsBool());
//            Assert.IsFalse(parserAB.Run("AC").MatchAsBool());
//            Assert.IsFalse(parserAB.Run("CB").MatchAsBool());
//            Assert.IsFalse(parserAB.Run("CC").MatchAsBool());
//        }

//        [TestMethod]
//        public void ParseOneCharOrElseAnotherChar()
//        {
//            var parserA = pchar('A');
//            var parserB = pchar('B');

//            var parserAB = parserA | parserB;

//            Assert.IsTrue(parserAB.Run("A").MatchAsBool());
//            Assert.IsTrue(parserAB.Run("B").MatchAsBool());
//            Assert.IsFalse(parserAB.Run("C").MatchAsBool());
//        }

//        [TestMethod]
//        public void ParseChoice()
//        {
//            var parserA = pchar('A');
//            var parserB = pchar('B');

//            var parserAB = Parser.Choice(parserA, parserB);

//            Assert.IsTrue(parserAB.Run("A").MatchAsBool());
//            Assert.IsTrue(parserAB.Run("B").MatchAsBool());
//            Assert.IsFalse(parserAB.Run("C").MatchAsBool());
//        }

//        [TestMethod]
//        public void ParseDigit()
//        {
//            var parserDigit = Parser.Choice("0123456789");

//            Assert.IsTrue(parserDigit.Run("0").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("1").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("2").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("3").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("4").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("5").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("6").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("7").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("8").MatchAsBool());
//            Assert.IsTrue(parserDigit.Run("9").MatchAsBool());
//            Assert.IsFalse(parserDigit.Run("A").MatchAsBool());
//        }

//        [TestMethod]
//        public void SumExpression()
//        {
//            //TODO allow numbers of more one digit
//            var parserAppend = pmap(pappend);
//            var parserInt = pmap(pint);
//            var parserDigit = pstring<char>("0123456789");
//            var parserNumberString = parserAppend(parserDigit);
//            var parserNumber = parserNumberString.Map(pint);
//            //var parserSumSymbol = pchar<char>('+');
//            //var parserSum = parserDigit & parserSumSymbol & parserDigit;

//            var digit = parserNumber.Run("1");
//            //var exp = parserSum.Run("1+1");
//        }

//        public StompMessage Parse<T>(string message)
//        {
//            //let inputABC = "ABC"
//            //run parseA inputABC  // Success ('A', "BC")

//            var input = "ABC";
//            var parseA = pchar('A');
//            var parseB = pchar('B');
//            var parseC = pchar('C');

//            var result = ((parseA | parseB) & parseB & parseC).Run(input);

//            return null;
//            //var pZero = pchar('\0');
//            //var pCR = pchar('\r');
//            //var pLF = pchar('\n');
//            //var pDash = pchar('-');
//            //var pAsciiLetter = pFunc(Char.IsLetter);
//            //var pIDENTIFIER = pChoice(pAsciiLetter, pDash);

//            //var result = pIDENTIFIER(message, 0);

//            //return null;
//        }

//        public abstract class ParserResult<T>
//        {
//            public T Value { get; set; }
//            public string Input { get; set; }
//            public int Position { get; set; }

//            public TR Match<TR>(Func<Success<T>, TR> onSuccess = null, Func<Failure<T>, TR> onFailure = null)
//            {
//                if (this is Success<T>)
//                {
//                    if (onSuccess != null) { return onSuccess(this as Success<T>); }
//                    return default(TR);
//                }
//                else
//                {
//                    if (onFailure != null) { return onFailure(this as Failure<T>); }
//                    return default(TR);
//                }
//            }

//            public ParserResult<T> Match(Func<Success<T>, ParserResult<T>> onSuccess = null, Func<Failure<T>, ParserResult<T>> onFailture = null)
//            {
//                if (this is Success<T>)
//                {
//                    if (onSuccess != null) { return onSuccess(this as Success<T>); }
//                    return this;
//                }
//                else
//                {
//                    if (onFailture != null) { return onFailture(this as Failure<T>); }
//                    return this;
//                }
//            }
//        }

//        public class Success<T> : ParserResult<T>
//        {
//        }

//        public class Failure<T> : ParserResult<T>
//        {
//            public string Message { get; set; }
//        }

//        //type Parser<'T> = Parser -of (string -> Result<'T* string>)
//        public class Parser<T1, T2>
//        {
//            protected Func<ParserResult<T1>, ParserResult<T2>> F;

//            public Parser(Func<ParserResult<T1>, ParserResult<T2>> f)
//            {
//                F = f;
//            }

//            public Parser<T1, T3> Map<T3>(Func<T2, T3> map)
//            {
//                return Lift<T3>(map, this);
//            }

//            public ParserResult<T2> Run(string input)
//            {
//                return F(new Success<T1>()
//                {
//                    Input = input,
//                    Position = 0
//                });
//            }

//            public static Parser<T1, T2> operator &(Parser<T1, T2> l, Parser<T2, T2> r)
//            {
//                return AndThen(l, r);
//            }

//            public static Parser<T1, T2> AndThen(Parser<T1, T2> l, Parser<T2, T2> r)
//            {
//                return new Parser<T1, T2>(x => r.F(l.F(x)));
//            }

//            public static Parser<T1, T2> operator |(Parser<T1, T2> l, Parser<T1, T2> r)
//            {
//                return OrElse(l, r);
//            }

//            public static Parser<T1, T2> OrElse(Parser<T1, T2> l, Parser<T1, T2> r)
//            {
//                return new Parser<T1, T2>(x =>
//                 {
//                     return l.F(x).Match(onFailture: _ => r.F(x));
//                 });
//            }

//            public static Parser<T1, T3> Lift<T3>(Func<T2, T3> map, Parser<T1, T2> p)
//            {
//                return new Parser<T1, T3>(r => p.F(r).Match<ParserResult<T3>>(
//                     onSuccess: s => new Success<T3>()
//                     {
//                         Input = s.Input,
//                         Position = s.Position,
//                         Value = map(s.Value)
//                     },
//                     onFailure: f => new Failure<T3>()
//                     {
//                         Input = f.Input,
//                         Position = f.Position,
//                         Value = map(f.Value)
//                     }));
//            }
//        }

//        public class Parser
//        {
//            public static Parser<T1, T2> Choice<T1, T2>(IEnumerable<Parser<T1, T2>> parsers)
//            {
//                return parsers.FoldLeft(Parser<T1, T2>.OrElse);
//            }

//            public static Parser<T1, T2> Choice<T1, T2>(params Parser<T1, T2>[] parsers)
//            {
//                return Choice(parsers as IEnumerable<Parser<T1, T2>>);
//            }

//            public static Parser<char, char> Choice(IEnumerable<char> characters)
//            {
//                var parsers = characters.Select(pchar<char>);
//                return Choice(parsers);
//            }

//            public static Parser<char, char> Choice(string characters)
//            {
//                return Choice(characters as IEnumerable<char>);
//            }


//        }

//        public static Parser<T, T> p<T>(Func<char, bool> check, Func<ParserResult<T>, ParserResult<T>> onSuccess, Func<ParserResult<T>, ParserResult<T>> onFailure)
//        {
//            return new Parser<T, T>(new Func<ParserResult<T>, ParserResult<T>>((result) => result.Match(onSuccess: r => check(r.Input[r.Position]) ? onSuccess(r) : onFailure(r))));
//        }

//        public static Parser<char, char> pchar(char c)
//        {
//            return p<char>(x => x == c, x => new Success<char>()
//            {
//                Input = x.Input,
//                Position = x.Position + 1,
//                Value = x.Input[x.Position]
//            }, x => new Failure<char>()
//            {
//                Input = x.Input,
//                Position = x.Position + 1,
//                Message = $"Expecting {c}. Got {x.Input[x.Position]}."
//            });
//        }

//        public static Parser<T, T> pchoice<T>(Parser<T, T> l, Parser<T, T> r)
//        {
//            return Parser.Choice(l, r);
//        }

//        public static Parser<string, string> pstring(string str)
//        {
//            var ap = pappend;
//            var parsers = str.Skip(1).Select(pchar).Aggregate(pchar(str.First()).Map(ap), (l, r) => l & r, x => x);
//        }

//        public static Func<Parser<T, T>, Parser<T, TR>> pmap<T, TR>(Func<T, TR> map)
//        {
//            return new Func<Parser<T, T>, Parser<T, TR>>(l => Parser<T, T>.Lift<TR>(map, l));
//        }

//        public static Func<string, int> pint
//        {
//            get
//            {
//                return new Func<string, int>(x => int.Parse(x));
//            }
//        }

//        public static Func<char, string> pappend
//        {
//            get
//            {
//                var builder = new StringBuilder();
//                return new Func<char, string>(x =>
//                {
//                    builder.Append(x);
//                    return builder.ToString();
//                });
//            }
//        }

//        public static Func<string, int, bool> pFunc(Func<char, bool> isChar)
//        {
//            return new Func<string, int, bool>((str, pos) => isChar(str[pos]));
//        }

//        public static Func<string, int, bool> pChoice(Func<string, int, bool> p1, Func<string, int, bool> p2)
//        {
//            return new Func<string, int, bool>((str, pos) => p1(str, pos) || p2(str, pos));
//        }

//        [TestMethod]
//        public void ParserPerformance()
//        {
//            var sw1 = Stopwatch.StartNew();
//            for (int i = 0; i < 100000; i++)
//            {
//                ParseUsingFParsec();
//            }
//            sw1.Stop();
//            Console.WriteLine(sw1.Elapsed);

//            sw1 = Stopwatch.StartNew();
//            for (int i = 0; i < 100000; i++)
//            {
//                ParseUsingCsharp();
//            }
//            sw1.Stop();
//            Console.WriteLine(sw1.Elapsed);
//        }

//        string GetJson()
//        {
//            return @"{""glossary"":{""title"":""example glossary"",""GlossDiv"":{""title"":""S"",""GlossList"":{""GlossEntry"":{""ID"":""SGML"",""SortAs"":""SGML"",""GlossTerm"":""Standard Generalized Markup Language"",""Acronym"":""SGML"",""Abbrev"":""ISO 8879:1986"",""GlossDef"":{""para"":""A meta-markup language, used to create markup languages such as DocBook."",""GlossSeeAlso"":[""GML"",""XML""]},""GlossSee"":""markup""}}}}}";
//        }

//        string GetMessage()
//        {
//            return $@"COMMAND
//headerA: value1
//headerB: value2

//{GetJson()}" + "\0";
//        }
//    }

//    public static class LinqExtensions
//    {
//        public static T1 FoldLeft<T1>(this IEnumerable<T1> items, Func<T1, T1, T1> binaryOperator)
//        {
//            return items.Skip(1).Aggregate(items.First(), binaryOperator, x => x);
//        }

//        public static T1 FoldRight<T1>(this IEnumerable<T1> items, Func<T1, T1, T1> binaryOperator)
//        {
//            return FoldLeft(items.Reverse(), binaryOperator);
//        }
//    }


//    public static class ParserResultExtensions
//    {
//        public static bool MatchAsBool<T>(this ParserResult<T> pr)
//        {
//            return pr.Match<bool>(x => true, x => false);
//        }
//    }
//}


using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Linq;

[TestClass]
public class Tests
{
    [TestMethod]
    public void ParseChar()
    {
        var pA = Parsers.Char('A');
        var result = pA.Parse("A");
        Assert.IsFalse(result.HasError);
        result.Match(onSuccess: s => Assert.AreEqual('A', s.Value), onFailure: f => Assert.Fail());
    }

    [TestMethod]
    public void ParseTwoCharsOperator()
    {
        var pA = Parsers.Char('A');
        var pB = Parsers.Char('B');
        var pAB = pA & pB;

        var result = pAB.Parse("AB");
        Assert.IsFalse(result.HasError);
        result.Match(
            onSuccess: s => { Assert.AreEqual('A', s.Value.Item1); Assert.AreEqual('B', s.Value.Item2); },
            onFailure: f => Assert.Fail());
    }

    [TestMethod]
    public void ParseString()
    {
        var pAB = Parsers.String("AB");

        var result = pAB.Parse("AB");

        Assert.IsFalse(result.HasError);
        result.Match(
            onSuccess: s => { Assert.AreEqual("AB", s.Value); },
            onFailure: f => Assert.Fail());
    }

    [TestMethod]
    public void ParseOrElse()
    {
        var pA = Parsers.Char('A');
        var pB = Parsers.Char('B');
        var pAorB = Parsers.OrElse(pA, pB);

        var result = pAorB.Parse("A");
        Assert.IsFalse(result.HasError);
        result.Match(
            onSuccess: s => { Assert.AreEqual('A', s.Value); },
            onFailure: f => Assert.Fail());

        result = pAorB.Parse("B");
        Assert.IsFalse(result.HasError);
        result.Match(
            onSuccess: s => { Assert.AreEqual('B', s.Value); },
            onFailure: f => Assert.Fail());

        result = pAorB.Parse("C");
        Assert.IsTrue(result.HasError);
    }
}

public class Parsers
{
    public static Parser<char, char> Char(char c)
    {
        return Func<char>(current => current == c);
    }

    public static Parser<char, string> String(string str)
    {
        var builder = new StringBuilder();
        return str.Select(Parsers.Char)
            .FoldLeft<Parser<char, string>, Parser<char, char>>((l, r) => Parsers.AndThen(l, r, (valuel, valuer) =>
            {
                builder.Append(valuer);
                return builder.ToString();
            }));
    }

    public static Parser<T, T> Func<T>(Func<T, bool> isValid)
    {
        return new Parser<T, T>(stream =>
        {
            var current = stream.Current;
            if (isValid(current))
            {
                stream.MoveNext();
                return new Success<T, T>()
                {
                    Stream = stream,
                    Value = current
                };
            }
            else
            {
                return new Failure<T, T>()
                {
                    Stream = stream,
                    Message = $"Not expecting {current}."
                };
            }
        });
    }

    public static Parser<T, TValueFinal> AndThen<T, TValueL, TValueR, TValueFinal>(Parser<T, TValueL> l, Parser<T, TValueR> r, Func<TValueL, TValueR, TValueFinal> map)
    {
        if (l == null)
        {
            return new Parser<T, TValueFinal>(stream =>
            {
                return r.ParseFunction(stream).Map<ParserResult<T, TValueFinal>>(
                        onSuccess: s2 => new Success<T, TValueFinal>()
                        {
                            Stream = s2.Stream,
                            Value = map(default(TValueL), s2.Value)
                        },
                        onFailure: x => Failure<T, TValueFinal>.Propagate(x));
            });
        }

        if (r == null)
        {
            return new Parser<T, TValueFinal>(stream =>
            {
                return l.ParseFunction(stream).Map<ParserResult<T, TValueFinal>>(
                        onSuccess: s1 => new Success<T, TValueFinal>()
                        {
                            Stream = s1.Stream,
                            Value = map(s1.Value, default(TValueR))
                        },
                        onFailure: x => Failure<T, TValueFinal>.Propagate(x));
            });
        }

        return new Parser<T, TValueFinal>(stream =>
        {
            return l.ParseFunction(stream).Map(
                onSuccess: s1 => r.ParseFunction(s1.Stream).Map<ParserResult<T, TValueFinal>>(
                    onSuccess: s2 => new Success<T, TValueFinal>()
                    {
                        Stream = s2.Stream,
                        Value = map(s1.Value, s2.Value)
                    },
                    onFailure: x => Failure<T, TValueFinal>.Propagate(x)),
                onFailure: x => Failure<T, TValueFinal>.Propagate(x));
        });
    }

    public static Parser<T, TValue> OrElse<T, TValue>(Parser<T, TValue> l, Parser<T, TValue> r)
    {
        if (l == null)
        {
            return r;
        }

        if (r == null)
        {
            return l;
        }

        return new Parser<T, TValue>(stream =>
        {
            return l.ParseFunction(stream).Map(
                onSuccess: s1 => s1,
                onFailure: f1 => r.ParseFunction(stream));
        });
    }
}

public class Parser<TStream, TValue>
{
    public Func<IEnumerator<TStream>, ParserResult<TStream, TValue>> ParseFunction;
    public Queue<TStream> Queue;

    public Parser(Func<IEnumerator<TStream>, ParserResult<TStream, TValue>> parse)
    {
        ParseFunction = parse;
        Queue = new Queue<TStream>();
    }

    public ParserResult<TStream, TValue> Parse(IEnumerable<TStream> stream)
    {
        var enumerator = stream.GetEnumerator();

        if (enumerator.MoveNext() == false)
        {
            return new Failure<TStream, TValue>()
            {
                Message = "End of Stream."
            };
        }

        return ParseFunction(enumerator);
    }

    public static Parser<TStream, Tuple<TValue, TValue>> operator &(Parser<TStream, TValue> l, Parser<TStream, TValue> r)
    {
        return Parsers.AndThen(l, r, Tuple.Create);
    }
}

public class ParserResult<T, TValue>
{
    public IEnumerator<T> Stream { get; set; }
    public bool HasError { get; set; }

    public void Match(Action<Success<T, TValue>> onSuccess = null, Action<Failure<T, TValue>> onFailure = null)
    {
        if (this is Success<T, TValue>)
        {
            onSuccess(this as Success<T, TValue>);
        }
        else if (this is Failure<T, TValue>)
        {
            onFailure(this as Failure<T, TValue>);
        }
    }

    public TResult Map<TResult>(Func<Success<T, TValue>, TResult> onSuccess = null, Func<Failure<T, TValue>, TResult> onFailure = null)
    {
        if (this is Success<T, TValue>)
        {
            return onSuccess(this as Success<T, TValue>);
        }
        else
        {
            return onFailure(this as Failure<T, TValue>);
        }
    }
}

public class Success<T, TValue> : ParserResult<T, TValue>
{
    public TValue Value { get; set; }

    public Success()
    {
        HasError = false;
    }
}

public class Failure<T, TValue> : ParserResult<T, TValue>
{
    public static Failure<T, TValue> Propagate<TValue2>(Failure<T, TValue2> f)
    {
        return new Failure<T, TValue>()
        {
            Stream = f.Stream,
            Message = f.Message
        };
    }

    public string Message { get; set; }

    public Failure()
    {
        HasError = true;
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

public interface IFunc<TA, TB, TC>
{
    TC Run(TA a, TB b);
}