using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using StompMessages;
using System.Diagnostics;

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
            Parse(message);
            //TODO PARSE
            var msg = new StompMessage("COMMAND", new[] { Tuple.Create("headerA", "value1"), Tuple.Create("headerB", "value2") }, GetJson());

            Assert.AreEqual("COMMAND", msg.Command);
            Assert.AreEqual("value1", msg.Headers["headerA"]);
            Assert.AreEqual("value2", msg.Headers["headerB"]);
            Assert.AreEqual(GetJson(), msg.Body);
        }

        public StompMessage Parse(string message)
        {
            var pZero = pchar('\0');
            var pCR = pchar('\r');
            var pLF = pchar('\n');
            var pDash = pchar('-');
            var pAsciiLetter = pFunc(Char.IsLetter);
            var pIDENTIFIER = pChoice(pAsciiLetter, pDash);

            var result = pIDENTIFIER(message, 0);

            return null;
        }

        Func<string, int, bool> pchar(char c)
        {
            return new Func<string, int, bool>((str, pos) => str[pos] == c);
        }

        Func<string, int, bool> pFunc(Func<char, bool> isChar)
        {
            return new Func<string, int, bool>((str, pos) => isChar(str[pos]));
        }

        Func<string, int, bool> pChoice(Func<string, int, bool> p1, Func<string, int, bool> p2)
        {
            return new Func<string, int, bool>((str, pos) => p1(str,pos) || p2(str,pos));
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
}
