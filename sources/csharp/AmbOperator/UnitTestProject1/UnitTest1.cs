using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Threading;
using System.Reflection;

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

            var result = await await Task.Factory.StartNew(async () =>
            {
                Point1 = true;
                var a = await GetAlwaysNull().Maybe();
                //This line will never run because of the Maybe
                Point2 = true;
                return a;
            });

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.IsNull(result);
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull2()
        {
            Reset();

            var result = await await Task.Factory.StartNew(async () =>
            {
                Point1 = true;
                var a = await GetNeverNull().Maybe();
                Point2 = true;
                var b = await GetAlwaysNull().Maybe();
                //This line will never run because of the Maybe
                Point3 = true;
                return a + b;
            });

            Assert.IsTrue(Point1);
            Assert.IsTrue(Point2);
            Assert.IsFalse(Point3);
            Assert.IsNull(result);
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull3()
        {
            Reset();

            var result = await await Task.Factory.StartNew(async () =>
            {
                Point1 = true;
                var a = await GetNeverNull().Maybe(x => x == "x");
                //This line will never run because of the Maybe
                Point2 = true;
                var b = await GetNeverNull().Maybe();
                return a + b;
            });

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.IsNull(result);
        }

        [TestMethod]
        public async Task MaybeMustBreakFunctionWhenNull4()
        {
            Reset();

            var result = await await Task.Factory.StartNew(async () =>
            {
                Point1 = true;
                var a = await GetNeverNull().Maybe(x => x == "x", () => "default string");
                //This line will never run because of the Maybe
                Point2 = true;
                var b = await GetNeverNull().Maybe();
                return a + b;
            });

            Assert.IsTrue(Point1);
            Assert.IsFalse(Point2);
            Assert.AreEqual("default string", result);
        }

        [TestMethod]
        public async Task MustWork()
        {
            var result = await await Task.Factory.StartNew(async () =>
            {
                var a = await GetNeverNull().Maybe();
                var b = await GetNeverNull().Maybe();
                return a + b;
            });

            Assert.AreEqual("xx", result);
        }

        async Task<string> GetAlwaysNull()
        {
            //First await lift string to Task<string>
            //Second await unwrap the string to return it
            return await await (string)null;
        }

        async Task<string> GetNeverNull()
        {
            //First await lift string to Task<string>
            //Second await unwrap the string to return it
            return await await "x";
        }
    }

    public static class AwaitLiftExtensions
    {
        public static TaskAwaiter<Task<T>> GetAwaiter<T>(this T value)
        {
            return Task.FromResult(Task.FromResult(value)).GetAwaiter();
        }
    }

    public static class MaybeTaskExtensions
    {
        public static MonadicTask<T> Maybe<T>(this Task<T> task)
        {
            return new MonadicTask<T>(task);
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when)
        {
            return new MonadicTask<T>(task, x => when(x.Result));
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<T> returns)
        {
            return new MonadicTask<T>(task, x => when(x.Result), x => returns());
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<T, T> returns)
        {
            return new MonadicTask<T>(task, x => when(x.Result), x => returns(x.Result));
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<T> returns)
        {
            return new MonadicTask<T>(task, when, x => returns());
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<T, T> returns)
        {
            return new MonadicTask<T>(task, when, x => returns(x.Result));
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<Task<T>, T> returns)
        {
            return new MonadicTask<T>(task, x => when(x.Result), returns);
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<Task<T>, T> returns)
        {
            return new MonadicTask<T>(task, when, returns);
        }
    }

    public class MonadicTask<T>
    {
        private readonly Task<T> _task;
        private readonly Func<Task<T>, bool> _when;
        private readonly Func<Task<T>, T> _returns;

        public MonadicTask(Task<T> task, Func<Task<T>, bool> when = null, Func<Task<T>, T> returns = null)
        {
            _task = task;
            _when = when;
            _returns = returns;
        }

        public MonadicAwaiter<T> GetAwaiter()
        {
            return new MonadicAwaiter<T>(_task, _when, _returns);
        }
    }

    public class MonadicAwaiter<T> : INotifyCompletion, ICriticalNotifyCompletion
    {
        public bool IsCompleted => false;

        Task<T> _task;
        Func<Task<T>, bool> _when;
        Func<Task<T>, T> _returns;

        public MonadicAwaiter(Task<T> task, Func<Task<T>, bool> when, Func<Task<T>, T> returns)
        {
            _task = task;
            //compiler does not accept x == default(T)
            _when = when ?? (x => EqualityComparer<T>.Default.Equals(x.Result, default(T)));
            _returns = returns ?? ((x) => default(T));
        }

        public T GetResult()
        {
            return _task.Result;
        }

        void INotifyCompletion.OnCompleted(Action continuation)
        {
            throw new NotImplementedException();
        }

        void ICriticalNotifyCompletion.UnsafeOnCompleted(Action continuation)
        {
            var stateMachine = GetStateMachine(continuation);

            if (stateMachine == null)
            {
                _task.ContinueWith(t =>
                {
                    continuation();
                });
            }
            else
            {
                _task.ContinueWith(t =>
                {
                    if (_when(t))
                    {
                        var result = _returns(t);
                        stateMachine.SetResult<T>(result);
                    }
                    else
                    {
                        continuation();
                    }
                });
            }
        }

        private IAsyncStateMachine GetStateMachine(Action continuation)
        {
            var target = continuation.Target;
            object moveNextRunner = target;

            if (target.GetType().Name == "ContinuationWrapper")
            {
                var continurationActionField = target?.GetType()
                ?.GetField("m_continuation", BindingFlags.NonPublic | BindingFlags.Instance);
                var continuationAction = continurationActionField?.GetValue(target) as Action;
                moveNextRunner = continuationAction?.Target;
            }

            var stateMachine = (IAsyncStateMachine)moveNextRunner?.GetType()
                ?.GetField("m_stateMachine", BindingFlags.NonPublic | BindingFlags.Instance)
                .GetValue(moveNextRunner);

            return stateMachine;
        }
    }

    public static class AsyncStateMachineExtensions
    {
        public static void SetResult<T>(this IAsyncStateMachine stateMachine, T result)
        {
            stateMachine.GetType()
                .GetField("<>1__state", BindingFlags.Public | BindingFlags.Instance)
                .SetValue(stateMachine, -2);
            var builder = (AsyncTaskMethodBuilder<T>)stateMachine.GetType()
                .GetField("<>t__builder", BindingFlags.Public | BindingFlags.Instance)
                .GetValue(stateMachine);

            builder.SetResult(result);
        }
    }
}
