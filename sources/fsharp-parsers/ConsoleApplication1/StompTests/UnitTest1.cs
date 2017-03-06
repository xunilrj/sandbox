using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using StompMessages;
using System.Diagnostics;
using static FParsec.CharParsers;
using System.Text;
using static StompTests.UnitTest1;
using System.Collections.Generic;
using System.Linq;

namespace StompTests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void ParseUsingFParsec()
        {
            var result = StompParser.StompParser.Parse(GetMessage());
            var success = result as FParsec.CharParsers.ParserResult<StompMessage, Microsoft.FSharp.Core.Unit>.Success;
            var msg = success.Item1;

            Assert.AreEqual("COMMAND", msg.Command);
            Assert.AreEqual("value1", msg.Headers["headerA"]);
            Assert.AreEqual("value2", msg.Headers["headerB"]);
            Assert.AreEqual(GetJson(), msg.Body);
        }

        [TestMethod]
        public void ParseUsingCsharp()
        {
            var message = GetMessage();
            Parse<string>(message);
            //TODO PARSE
            var msg = new StompMessage("COMMAND", new[] { Tuple.Create("headerA", "value1"), Tuple.Create("headerB", "value2") }, GetJson());

            Assert.AreEqual("COMMAND", msg.Command);
            Assert.AreEqual("value1", msg.Headers["headerA"]);
            Assert.AreEqual("value2", msg.Headers["headerB"]);
            Assert.AreEqual(GetJson(), msg.Body);
        }

        [TestMethod]
        public void ParseOneChar()
        {
            var parserA = pchar<bool>('A');

            Assert.IsTrue(parserA.Run("A").MatchAsBool());
            Assert.IsFalse(parserA.Run("B").MatchAsBool());
        }

        [TestMethod]
        public void ParseOneCharAndThenAnotherChar()
        {
            var parserA = pchar<bool>('A');
            var parserB = pchar<bool>('B');

            var parserAB = parserA & parserB;

            Assert.IsTrue(parserAB.Run("AB").MatchAsBool());
            Assert.IsFalse(parserAB.Run("AC").MatchAsBool());
            Assert.IsFalse(parserAB.Run("CB").MatchAsBool());
            Assert.IsFalse(parserAB.Run("CC").MatchAsBool());
        }

        [TestMethod]
        public void ParseOneCharOrElseAnotherChar()
        {
            var parserA = pchar<bool>('A');
            var parserB = pchar<bool>('B');

            var parserAB = parserA | parserB;

            Assert.IsTrue(parserAB.Run("A").MatchAsBool());
            Assert.IsTrue(parserAB.Run("B").MatchAsBool());
            Assert.IsFalse(parserAB.Run("C").MatchAsBool());
        }

        [TestMethod]
        public void ParseChoice()
        {
            var parserA = pchar<bool>('A');
            var parserB = pchar<bool>('B');

            var parserAB = Parser.Choice(parserA, parserB);

            Assert.IsTrue(parserAB.Run("A").MatchAsBool());
            Assert.IsTrue(parserAB.Run("B").MatchAsBool());
            Assert.IsFalse(parserAB.Run("C").MatchAsBool());
        }

        [TestMethod]
        public void ParseDigit()
        {
            var parserDigit = Parser.Choice("0123456789");

            Assert.IsTrue(parserDigit.Run("0").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("1").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("2").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("3").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("4").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("5").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("6").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("7").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("8").MatchAsBool());
            Assert.IsTrue(parserDigit.Run("9").MatchAsBool());
            Assert.IsFalse(parserDigit.Run("A").MatchAsBool());
        }

        [TestMethod]
        public void SumExpression()
        {
            //TODO allow numbers of more one digit
            var parserAppend = pmap(pappend);
            var parserInt = pmap(pint);
            var parserDigit = pstring<char>("0123456789");
            var parserNumberString = parserAppend(parserDigit);
            var parserNumber = parserNumberString.Map(pint);
            //var parserSumSymbol = pchar<char>('+');
            //var parserSum = parserDigit & parserSumSymbol & parserDigit;

            var digit = parserNumber.Run("1");
            //var exp = parserSum.Run("1+1");
        }

        public StompMessage Parse<T>(string message)
        {
            //let inputABC = "ABC"
            //run parseA inputABC  // Success ('A', "BC")

            var input = "ABC";
            var parseA = pchar<T>('A');
            var parseB = pchar<T>('B');
            var parseC = pchar<T>('C');

            var result = ((parseA | parseB) & parseB & parseC).Run(input);

            return null;
            //var pZero = pchar('\0');
            //var pCR = pchar('\r');
            //var pLF = pchar('\n');
            //var pDash = pchar('-');
            //var pAsciiLetter = pFunc(Char.IsLetter);
            //var pIDENTIFIER = pChoice(pAsciiLetter, pDash);

            //var result = pIDENTIFIER(message, 0);

            //return null;
        }

        public abstract class ParserResult<T>
        {
            public T Value { get; set; }
            public string Input { get; set; }
            public int Position { get; set; }

            public TR Match<TR>(Func<Success<T>, TR> onSuccess = null, Func<Failure<T>, TR> onFailure = null)
            {
                if (this is Success<T>)
                {
                    if (onSuccess != null) { return onSuccess(this as Success<T>); }
                    return default(TR);
                }
                else
                {
                    if (onFailure != null) { return onFailure(this as Failure<T>); }
                    return default(TR);
                }
            }

            public ParserResult<T> Match(Func<Success<T>, ParserResult<T>> onSuccess = null, Func<Failure<T>, ParserResult<T>> onFailture = null)
            {
                if (this is Success<T>)
                {
                    if (onSuccess != null) { return onSuccess(this as Success<T>); }
                    return this;
                }
                else
                {
                    if (onFailture != null) { return onFailture(this as Failure<T>); }
                    return this;
                }
            }
        }

        public class Success<T> : ParserResult<T>
        {
        }

        public class Failure<T> : ParserResult<T>
        {
            public string Message { get; set; }
        }

        //type Parser<'T> = Parser -of (string -> Result<'T* string>)
        public class Parser<T1, T2>
        {
            protected Func<ParserResult<T1>, ParserResult<T2>> F;

            public Parser(Func<ParserResult<T1>, ParserResult<T2>> f)
            {
                F = f;
            }

            public Parser<T1, T3> Map<T3>(Func<T2, T3> map)
            {
                return Lift<T3>(map, this);
            }

            public ParserResult<T2> Run(string input)
            {
                return F(new Success<T1>()
                {
                    Input = input,
                    Position = 0
                });
            }

            public static Parser<T1, T2> operator &(Parser<T1, T2> l, Parser<T2, T2> r)
            {
                return AndThen(l, r);
            }

            public static Parser<T1, T2> AndThen(Parser<T1, T2> l, Parser<T2, T2> r)
            {
                return new Parser<T1, T2>(x => r.F(l.F(x)));
            }

            public static Parser<T1, T2> operator |(Parser<T1, T2> l, Parser<T1, T2> r)
            {
                return OrElse(l, r);
            }

            public static Parser<T1, T2> OrElse(Parser<T1, T2> l, Parser<T1, T2> r)
            {
                return new Parser<T1, T2>(x =>
                 {
                     return l.F(x).Match(onFailture: _ => r.F(x));
                 });
            }

            public static Parser<T1, T3> Lift<T3>(Func<T2, T3> map, Parser<T1, T2> p)
            {
                return new Parser<T1, T3>(r => p.F(r).Match<ParserResult<T3>>(
                     onSuccess: s => new Success<T3>()
                     {
                         Input = s.Input,
                         Position = s.Position,
                         Value = map(s.Value)
                     },
                     onFailure: f => new Failure<T3>()
                     {
                         Input = f.Input,
                         Position = f.Position,
                         Value = map(f.Value)
                     }));
            }
        }

        public class Parser
        {
            public static Parser<T1, T2> Choice<T1, T2>(IEnumerable<Parser<T1, T2>> parsers)
            {
                return parsers.FoldLeft(Parser<T1, T2>.OrElse);
            }

            public static Parser<T1, T2> Choice<T1, T2>(params Parser<T1, T2>[] parsers)
            {
                return Choice(parsers as IEnumerable<Parser<T1, T2>>);
            }

            public static Parser<char, char> Choice(IEnumerable<char> characters)
            {
                var parsers = characters.Select(pchar<char>);
                return Choice(parsers);
            }

            public static Parser<char, char> Choice(string characters)
            {
                return Choice(characters as IEnumerable<char>);
            }


        }

        public static Parser<T, T> p<T>(Func<char, bool> check, Func<ParserResult<T>, ParserResult<T>> onSuccess, Func<ParserResult<T>, ParserResult<T>> onFailure)
        {
            return new Parser<T, T>(new Func<ParserResult<T>, ParserResult<T>>((result) => result.Match(onSuccess: r => check(r.Input[r.Position]) ? onSuccess(r) : onFailure(r))));
        }

        public static Parser<T, T> pchar<T>(char c)
        {
            return p<T>(x => x == c, x => new Success<T>() {
                Input = x.Input,
                Position = x.Position + 1,
                Value = (T)(object)x.Input[x.Position]
            }, x => new Failure<T>() {
                Input = x.Input,
                Position = x.Position + 1,
                Message = $"Expecting {c}. Got {x.Input[x.Position]}."
            });
        }

        public static Parser<T, T> pchoice<T>(Parser<T, T> l, Parser<T, T> r)
        {
            return Parser.Choice(l, r);
        }

        public static Parser<T, T> pstring<T>(string str)
        {
            return str.Select(pchar<T>).FoldLeft(pchoice<T>);
        }

        public static Func<Parser<T, T>, Parser<T, TR>> pmap<T, TR>(Func<T, TR> map)
        {
            return new Func<Parser<T, T>, Parser<T, TR>>(l => Parser<T,T>.Lift<TR>(map, l));
        }

        public static Func<string, int> pint
        {
            get
            {
                return new Func<string, int>(x => int.Parse(x));
            }
        }

        public static Func<char, string> pappend
        {
            get
            {
                var builder = new StringBuilder();
                return new Func<char, string>(x =>
                {
                    builder.Append(x);
                    return builder.ToString();
                });
            }
        }

        public static Func<string, int, bool> pFunc(Func<char, bool> isChar)
        {
            return new Func<string, int, bool>((str, pos) => isChar(str[pos]));
        }

        public static Func<string, int, bool> pChoice(Func<string, int, bool> p1, Func<string, int, bool> p2)
        {
            return new Func<string, int, bool>((str, pos) => p1(str, pos) || p2(str, pos));
        }

        [TestMethod]
        public void ParserPerformance()
        {
            var sw1 = Stopwatch.StartNew();
            for (int i = 0; i < 100000; i++)
            {
                ParseUsingFParsec();
            }
            sw1.Stop();
            Console.WriteLine(sw1.Elapsed);

            sw1 = Stopwatch.StartNew();
            for (int i = 0; i < 100000; i++)
            {
                ParseUsingCsharp();
            }
            sw1.Stop();
            Console.WriteLine(sw1.Elapsed);
        }

        string GetJson()
        {
            return @"{""glossary"":{""title"":""example glossary"",""GlossDiv"":{""title"":""S"",""GlossList"":{""GlossEntry"":{""ID"":""SGML"",""SortAs"":""SGML"",""GlossTerm"":""Standard Generalized Markup Language"",""Acronym"":""SGML"",""Abbrev"":""ISO 8879:1986"",""GlossDef"":{""para"":""A meta-markup language, used to create markup languages such as DocBook."",""GlossSeeAlso"":[""GML"",""XML""]},""GlossSee"":""markup""}}}}}";
        }

        string GetMessage()
        {
            return $@"COMMAND
headerA: value1
headerB: value2

{GetJson()}" + "\0";
        }
    }

    public static class LinqExtensions
    {
        public static T1 FoldLeft<T1>(this IEnumerable<T1> items, Func<T1, T1, T1> binaryOperator)
        {
            return items.Skip(1).Aggregate(items.First(), binaryOperator, x => x);
        }

        public static T1 FoldRight<T1>(this IEnumerable<T1> items, Func<T1, T1, T1> binaryOperator)
        {
            return FoldLeft(items.Reverse(), binaryOperator);
        }
    }


    public static class ParserResultExtensions
    {
        public static bool MatchAsBool<T>(this ParserResult<T> pr)
        {
            return pr.Match<bool>(x => true, x => false);
        }
    }
}
