using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Threading;
using System.Reflection;
using OOFunctional;

using CustomerId = System.Int32;
using CustomerEmail = System.String;

namespace UnitTestProject1
{
    /// <summary>
    /// Another interesting approach
    /// https://github.com/louthy/csharp-monad
    /// but it demands a different mindset. I would like
    /// to develop as close as possible to vanilla c# code
    /// </summary>
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

        //Same as above but using internal methods with a switch
        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull5()
        {
            Reset();

            var result = await Run();

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.AreEqual("default string", result);

            async Task<string> Run()
            {
                Point1 = true;
                var a = await GetNeverNull().Map(IsNotValidValue, ReturnDefaultValue);
                //The following line will never execute because GetNeverNull
                //returns "x" and the MonadicTask will ask IsNotValidValue if "x" is valid or not
                //and will immediatly abort this async method returning the value of ReturnDefaultValue
                Point2 = true;
                var b = await GetNeverNull();
                return a + b;
            }
            bool IsNotValidValue(string str)
            {
                switch (str)
                {
                    case null:
                    case "x":
                        return true;
                    default:
                        return false;
                }
            }
            string ReturnDefaultValue() => "default string";
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
        public async Task EitherWithInternalMethods()
        {
            var result = await await Task.Factory.StartNew(async () =>
            {
                var (getContent, toString) = F.New((HttpResponse x) => x.Content, ObjectF.ToString);

                var a = await GetNeverNull();
                var b = await GetValueAlwaysOK().Either(
                    getContent.Then(toString),
                    ObjectF.ToString);
                return a + b;
            });

            Assert.AreEqual("x11", result);
        }

        [TestMethod]
        public async Task EitherInFunctionalStyle()
        {
            var result = await await Task.Factory.StartNew(async () =>
            {
                var (getContent, toString) = F.New(
                    (HttpResponse x) => x.Content,
                    (object x) => x.ToString());

                var a = await GetNeverNull();
                var b = await GetValueAlwaysOK().Either(
                    getContent.Then(toString),
                    ~toString);
                return a + b;
            });

            Assert.AreEqual("x11", result);
        }

        /// <summary>
        /// as in https://fsharpforfunandprofit.com/posts/elevated-world-3/
        /// type CustomerId = CustomerId of int
        /// type EmailAddress = EmailAddress of string
        /// type CustomerInfo = {
        /// id: CustomerId
        /// email: EmailAddress
        ///}
        /// </summary>
        /// <returns></returns>
        [TestMethod]
        public async Task ValidationExample()
        {
            var (badCustomer, e1) = await CreateCustomerResultA(0, "").Wrap();
            Assert.AreEqual(0, badCustomer.Id);
            Assert.IsNull(badCustomer.Email);
            Assert.AreEqual(2, e1.InnerExceptions.Count);
            Assert.AreEqual("customerId", (e1.InnerExceptions[0] as ArgumentOutOfRangeException).ParamName);
            Assert.AreEqual("customerEmail", (e1.InnerExceptions[1] as ArgumentException).ParamName);

            var (goodCustomer, e2) = await CreateCustomerResultA(1, "good@email.com").Wrap();
            Assert.AreEqual(1, goodCustomer.Id);
            Assert.AreEqual("good@email.com", goodCustomer.Email);
            Assert.IsNull(e2);

            //let createCustomerId id =
            //  if id > 0 then
            //    Success(CustomerId id)
            //  else
            //    Failure["CustomerId must be positive"]
            MaybeTask<CustomerId> createCustomerId(int customerId)
            {
                if (customerId > 0) return customerId;
                else return new ArgumentOutOfRangeException(nameof(customerId), "CustomerId must be positive");
            }
            //let createEmailAddress str =
            //  if System.String.IsNullOrEmpty(str) then
            //      Failure["Email must not be empty"]
            //  elif str.Contains("@") then
            //      Success(EmailAddress str)
            //  else
            //      Failure["Email must contain @-sign"]
            MaybeTask<CustomerEmail> createEmailAddress(string customerEmail)
            {
                if (string.IsNullOrEmpty(customerEmail)) return new ArgumentException("Email must not be empty", nameof(customerEmail));
                else if (customerEmail.Contains("@")) return customerEmail;
                else return new ArgumentException("Email must contain @", nameof(customerEmail));
            }
            //let createCustomer customerId email =
            //    { id=customerId; email = email }
            CustomerInfo createCustomer(CustomerId id, CustomerEmail email)
            {
                return new CustomerInfo()
                {
                    Id = id,
                    Email = email
                };
            }
            //let createCustomerResultA id email =
            //  let idResult = createCustomerId id
            //  let emailResult = createEmailAddress email
            //  createCustomer <!> idResult <*> emailResult
            async Task<CustomerInfo> CreateCustomerResultA(int id, string email)
            {
                var idResult = createCustomerId(id);
                var emailResult = createEmailAddress(email);
                var (validId, validEmail) = await Maybe.Join(idResult, emailResult);
                return createCustomer(validId, validEmail);
            }
        }

        public struct CustomerInfo { public CustomerId Id; public CustomerEmail Email; };

        MaybeTask<string> GetAlwaysNull()
        {
            return Run();
            async Task<string> Run()
            {
                //First await lift string to Task<string>
                //Second await unwrap the string to return it
                return await await (string)null;
            }
        }

        MaybeTask<string> GetNeverNull()
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
            public object Content { get; set; }
        }

        async Task<HttpResponse> GetValueAlwaysOK()
        {
            return await await new HttpResponse()
            {
                Status = StatusCode.OK,
                Content = 11,
            };
        }
    }
}
