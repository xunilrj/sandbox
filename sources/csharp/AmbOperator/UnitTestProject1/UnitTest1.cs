using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Threading;
using System.Reflection;
using OOFunctional;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        static bool Point1 = false;
        static bool Point2 = false;
        static bool Point3 = false;

        void Reset()
        {
            Point1 = false;
            Point2 = false;
            Point3 = false;
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull1()
        {
            Reset();

            var result = await Run();

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.IsNull(result);

            async Task<string> Run()
            {
                Point1 = true;
                var a = await GetAlwaysNull();
                //This line will never run because of the Maybe
                Point2 = true;
                return a;
            }
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull2()
        {
            Reset();

            var result = await Run();

            Assert.IsTrue(Point1);
            Assert.IsTrue(Point2);
            Assert.IsFalse(Point3);
            Assert.IsNull(result);

            async Task<string> Run()
            {
                Point1 = true;
                var a = await GetNeverNull();
                Point2 = true;
                var b = await GetAlwaysNull();
                //This line will never run because of the Maybe
                Point3 = true;
                return a + b;
            }
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull3()
        {
            Reset();

            var result = await Run();

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.IsNull(result);

            async Task<string> Run()
            {
                Point1 = true;
                var a = await GetNeverNull().Map((string x) => x == "x");
                //This line will never run because of the Maybe
                Point2 = true;
                var b = await GetNeverNull();
                return a + b;
            }
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull4()
        {
            Reset();

            var result = await Run();

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.AreEqual("default string", result);

            async Task<string> Run()
            {
                Point1 = true;
                var a = await GetNeverNull().Map((string x) => x == "x", () => "default string");
                //This line will never run because of the Maybe
                Point2 = true;
                var b = await GetNeverNull();
                return a + b;
            }
        }

        [TestMethod]
        public async Task MustWork()
        {
            var result = await Run(); 

            Assert.AreEqual("xx", result);

            async Task<string> Run()
            {
                var a = await GetNeverNull();
                var b = await GetNeverNull();
                return a + b;
            }
        }

        [TestMethod]
        public async Task EitherInFunctionalStyle()
        {
            var result = await await Task.Factory.StartNew(async () =>
            {
                var (getContent, toString) = F.New(
                    (HttpResponse x) => x.Value,
                    (object x) => x.ToString());

                var a = await GetNeverNull();
                var b = await GetValueAlwaysOK().Either(
                    getContent.Then(toString),
                    ~toString);
                return a + b;
            });

            Assert.AreEqual("x11", result);
        }

        MonadicTask<string> GetAlwaysNull()
        {
            return Run();
            async Task<string> Run()
            {
                //First await lift string to Task<string>
                //Second await unwrap the string to return it
                return await await (string)null;
            }
        }

        MonadicTask<string> GetNeverNull()
        {
            return Run();
            async Task<string> Run()
            {
                //First await lift string to Task<string>
                //Second await unwrap the string to return it
                return await await "x";
            }
        }
 
        public enum StatusCode
        {
            OK,
            NotFound,
            InternalError
        }

        public class HttpResponse
        {
            public StatusCode Status { get; set; }
            public object Value { get; set; }
        }

        async Task<HttpResponse> GetValueAlwaysOK()
        {
            return await await new HttpResponse()
            {
                Status = StatusCode.OK,
                Value = 11,
            };
        }
    }
}
