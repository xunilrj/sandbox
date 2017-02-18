using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using StompMessages;
using System.Diagnostics;
using static FParsec.CharParsers;
using System.Text;
using static StompTests.UnitTest1;

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

            Assert.IsTrue(parserA.Run("A").MathAsBool());
            Assert.IsFalse(parserA.Run("B").MathAsBool());
        }

        [TestMethod]
        public void ParseOneCharAndThenAnotherChar()
        {
            var parserA = pchar<bool>('A');
            var parserB = pchar<bool>('B');

            var parserAB = parserA & parserB;

            Assert.IsTrue(parserAB.Run("AB").MathAsBool());
            Assert.IsFalse(parserAB.Run("AC").MathAsBool());
            Assert.IsFalse(parserAB.Run("CB").MathAsBool());
            Assert.IsFalse(parserAB.Run("CC").MathAsBool());
        }

        [TestMethod]
        public void ParseOneCharOrElseAnotherChar()
        {
            var parserA = pchar<bool>('A');
            var parserB = pchar<bool>('B');

            var parserAB = parserA | parserB;

            Assert.IsTrue(parserAB.Run("A").MathAsBool());
            Assert.IsTrue(parserAB.Run("B").MathAsBool());
            Assert.IsFalse(parserAB.Run("C").MathAsBool());
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

            public TR Match<TR>(Func<Success<T>, TR> onSuccess = null, Func<Failure<T>, TR> onFailture = null)
            {
                if (this is Success<T>)
                {
                    if (onSuccess != null) { return onSuccess(this as Success<T>); }
                    return default(TR);
                }
                else
                {
                    if (onFailture != null) { return onFailture(this as Failure<T>); }
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

        //type Parser<'T> = Parser of (string -> Result<'T* string>)
        public class Parser<T>
        {
            Func<ParserResult<T>, ParserResult<T>> F;

            public Parser(Func<ParserResult<T>, ParserResult<T>> f)
            {
                F = f;
            }

            public ParserResult<T> Run(string input)
            {
                return F(new Success<T>()
                {
                    Input = input,
                    Position = 0
                });
            }

            public static Parser<T> operator &(Parser<T> l, Parser<T> r)
            {
                return AndThen(l, r);
            }

            public static Parser<T> AndThen(Parser<T> l, Parser<T> r)
            {
                return new Parser<T>(x => r.F(l.F(x)));
            }

            public static Parser<T> operator |(Parser<T> l, Parser<T> r)
            {
                return OrElse(l, r);
            }

            public static Parser<T> OrElse(Parser<T> l, Parser<T> r)
            {
                return new Parser<T>(x =>
                {
                    return l.F(x).Match(onFailture: _ => r.F(x));
                });
            }
        }

        Parser<T> p<T>(Func<char, bool> check, Func<ParserResult<T>, ParserResult<T>> onSuccess, Func<ParserResult<T>, ParserResult<T>> onFailure)
        {
            return new Parser<T>(new Func<ParserResult<T>, ParserResult<T>>((result) => result.Match(onSuccess: r => check(r.Input[r.Position]) ? onSuccess(r) : onFailure(r))));
        }

        Parser<T> pchar<T>(char c)
        {
            return p<T>(x => x == c, x => new Success<T>() { Input = x.Input, Position = x.Position + 1 }, x => new Failure<T>() { Input = x.Input, Position = x.Position + 1, Message = $"Expecting {c}. Got {x.Input[x.Position]}." });
        }

        Func<string, int, bool> pFunc(Func<char, bool> isChar)
        {
            return new Func<string, int, bool>((str, pos) => isChar(str[pos]));
        }

        Func<string, int, bool> pChoice(Func<string, int, bool> p1, Func<string, int, bool> p2)
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

    public static class ParserResultExtensions
    {
        public static bool MathAsBool<T>(this ParserResult<T> pr)
        {
            return pr.Match<bool>(x => true, x => false);
        }
    }
}
