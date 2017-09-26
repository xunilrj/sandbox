using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace CSharpFeatures
{
    [TestClass]
    public class Tuples
    {
        [TestMethod]
        public async Task TupleTests()
        {
            var t1 = Tuple.Create(1, 2);

            Assert.AreEqual(1, t1.Item1);
            Assert.AreEqual(2, t1.Item2);

            var t2 = (1, 2);

            Assert.AreEqual(1, t2.Item1);
            Assert.AreEqual(2, t2.Item2);

            var (a, b) = t1;
            Assert.AreEqual(1, a);
            Assert.AreEqual(2, b);

            var (a2, _) = t1;
            var (_, a3) = t1;

            Assert.AreEqual(1, a2);
            Assert.AreEqual(2, a3);

            var t3 = Method1();
            var (a4, b4) = Method1();

            var d1 = Method2();
            var (a5, b5) = Method2();
            //Does not support Deconstructing to one-tuple
            //var (a5_2,) = Method2();

            var d2 = Method3();
            var (a6, b6) = Method3();

            var (value, error) = GetValueAsync();
            Assert.AreEqual(1, value);
            Assert.IsNull(error);

            var (value2, error2) = GetValueAsyncFail();
            Assert.AreEqual(0, value); //default int value
            Assert.IsNotNull(error2);

            Assert.ThrowsException<NotSupportedException>(() =>
            {
                var (value3, error3) = RealAsync();
            });

            var (value4, error4) = await RealAsync().Wrap();
            Assert.AreEqual(1, value4);
            Assert.IsNull(error4);
        }

        (int, int) Method1()
        {
            return (1, 2);
        }

        public class Dto1
        {
            public int A { get; set; }
            public int B { get; set; }

            //Does not work
            public void Deconstruct(out int a)
            {
                a = A;
            }

            public void Deconstruct(out int a, out int b)
            {
                a = A;
                b = B;
            }
        }

        Dto1 Method2()
        {
            return new Dto1()
            {
                A = 1,
                B = 2
            };

        }

        public class Dto2
        {
            public int A { get; set; }
            public int B { get; set; }
        }

        Dto2 Method3()
        {
            return new Dto2()
            {
                A = 1,
                B = 2
            };
        }

        Task<int> GetValueAsync()
        {
            return Task.FromResult(1);
        }

        Task<int> GetValueAsyncFail()
        {
            return Task.FromException<int>(new Exception());
        }

        async Task<int> RealAsync()
        {
            await Task.Yield();
            return 1;
        }


        [TestMethod]
        public async Task A()
        {
            var ids = Task.FromResult(new[] { 1, 2, 3 });
            var needPermissions = await ids.SelectAsync(async x => new
            {
                Id = x,
                Can = await canView(x)
            }).WhereAsync(x => x.Can == false);

            foreach (var item in needPermissions)
            {
                await givePermission(item.Id);
            }

            Task<bool> canView(int id)
            {
                return Task.FromResult(false);
            }
            Task givePermission(int id)
            {
                return Task.FromResult(0);
            }
        }
    }

    public static class AsyncLinqExtensions
    {
        public static async Task<IEnumerable<T1>> WhereAsync<T1>(this Task<IEnumerable<T1>> items, Func<T1, bool> f)
        {
            var list = new List<T1>();

            foreach (var item in (await items))
            {
                if (f(item))
                {
                    list.Add(item);
                }
            }

            return list;
        }

        public static async Task<IEnumerable<T2>> SelectAsync<T1, T2>(this Task<T1[]> items, Func<T1, Task<T2>> f)
        {
            var list = new List<T2>();

            foreach (var item in (await items))
            {
                list.Add(await f(item));
            }

            return list;
        }
    }

    public static class Dto2Extensions
    {
        public static void Deconstruct(this Tuples.Dto2 item, out int a, out int b)
        {
            a = item.A;
            b = item.B;
        }
    }

    public static class TaskExtenstions
    {
        public static void Deconstruct<T>(this Task<T> task, out T result, out Exception e)
        {
            if (!task.IsCompleted)
            {
                throw new NotSupportedException();
            }

            if (task.IsFaulted)
            {
                result = default(T);
                e = task.Exception;
            }
            else
            {
                result = task.Result;
                e = null;
            }
        }

        public static async Task<Task<T>> Wrap<T>(this Task<T> t)
        {
            return Task.FromResult(await t);
        }
    }
}
