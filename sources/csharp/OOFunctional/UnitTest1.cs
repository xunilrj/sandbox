using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace OOFunctional
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var add11 = F.New(() => 1 + 1);
            var add1 = F.New((int x) => x + 1);
            var add = F.New((int a, int b) => a + b);

            //Simple Invokes
            //!f is the same as f.Invoke()
            Assert.AreEqual(2, add11.Invoke());
            Assert.AreEqual(2, !add11);

            Assert.AreEqual(2, add1.Invoke(1));

            Assert.AreEqual(2, add.Invoke(1, 1));

            //Partial Apply using + operator
            Assert.AreEqual(2, (add + 1).Invoke(1));
            Assert.AreEqual(2, (add + 1 + 1).Invoke());
            Assert.AreEqual(2, !(add + 1 + 1));

            //Partial Apply using index operator
            Assert.AreEqual(2, add[1].Invoke(1));
            Assert.AreEqual(2, add[1, 1].Invoke());
            Assert.AreEqual(2, !add[1, 1]);

            Assert.AreEqual(2, (add[1][1]).Invoke());
            Assert.AreEqual(2, !add[1][1]);

            //Complete Apply
            Assert.AreEqual(2, !(add + Tuple.Create(1, 1)));
            Assert.AreEqual(2, !add[Tuple.Create(1, 1)]);

            //Functional composition
            var times2 = new F<int, int>(x => x * 2);
            var addthentimes2 = add | times2;
            Assert.AreEqual(4, !addthentimes2[1, 1]);

            System.Func<int, int, int> addthentimes2func = add | (x => x * 2);
            Assert.AreEqual(4, addthentimes2func(1, 1));

            var times = new F<int, int, int>((a, b) => a * b);
            addthentimes2 = add | times[2];
            Assert.AreEqual(4, !addthentimes2[1, 1]);

            //There is no way to make this composition using operator
            //because we cant inject a new generic type in a operator
            //definition.
            var timesf = F.New((int a, int b) => (float)(a * b));
            var g = add.Then(timesf[2]);
            Assert.AreEqual(4, !g[1, 1]);

            //var genericadd = G.New((g a, g b) => a + b);
            //Assert.AreEqual(2, !genericadd[1, 1]);

            ActionExceptions.Throw().StartHandle().When(x => 1).When(x => 2).End();
            var hhh = new ActionExceptions();
            hhh
                .When((NullReferenceException x) => { })
                .When((DivideByZeroException x) => { })
                .End();
        }

        public class ActionExceptions : ApplicationException<ActionExceptions, 
            DivideByZeroException,
            NullReferenceException> { }
    }


    public static class F
    {
        public static F<TR> New<TR>(Func<TR> f) => new F<TR>(f);
        public static F<T1, TR> New<T1, TR>(Func<T1, TR> f) => new F<T1, TR>(f);
        public static F<T1, TR> New1<T1, TR>(Func<T1, TR> f) => new F<T1, TR>(f);
        public static F<T1, T2, TR> New<T1, T2, TR>(Func<T1, T2, TR> f) => new F<T1, T2, TR>(f);

        public static (F<T1, TR1>, F<T2, TR2>) New<T1, TR1, T2, TR2>(Func<T1, TR1> f1, Func<T2, TR2> f2)
        {
            return (f1, f2);
        }

        public static (F<T1, TR1>, F<T2, TR2>, F<T3, TR3>) New<T1, TR1, T2, TR2, T3, TR3>(Func<T1, TR1> f1, Func<T2, TR2> f2, Func<T3, TR3> f3)
        {
            return (f1, f2, f3);
        }

        public static (F<T1, TR1>, F<T2, TR2>, F<T3, TR3>, F<T4, TR4>) New<T1, TR1, T2, TR2, T3, TR3, T4, TR4>(Func<T1, TR1> f1, Func<T2, TR2> f2, Func<T3, TR3> f3, Func<T4, TR4> f4)
        {
            return (f1, f2, f3, f4);
        }

        public static Func<T1, TR2> Compose<T1, TR1, TR2>(Func<T1, TR1> f1, Func<TR1, TR2> f2)
        {
            var (ff1, ff2) = F.New(f1, f2);
            return ff1.Then(ff2);
        }

        public static Func<T1, TR3> Compose<T1, TR1, TR2, TR3>(Func<T1, TR1> f1, Func<TR1, TR2> f2, Func<TR2, TR3> f3)
        {
            var (ff1, ff2, ff3) = F.New(f1, f2, f3);
            return ff1.Then(ff2).Then(ff3);
        }

        public static Func<T1, TR4> Compose<T1, TR1, TR2, TR3, TR4>(Func<T1, TR1> f1, Func<TR1, TR2> f2, Func<TR2, TR3> f3, Func<TR3, TR4> f4)
        {
            var (ff1, ff2, ff3, ff4) = F.New(f1, f2, f3, f4);
            return ff1.Then(ff2).Then(ff3).Then(ff4);
        }
    }

    public static class G
    {
        //public static G<TR> New<TR>(Func<TR> f) => new F<TR>(f);
        //public static F<T1, TR> New<T1, TR>(Func<T1, TR> f) => new F<T1, TR>(f);
        public static G2<TR> New<TR>(Expression<Func<g, g, TR>> f) => new G2<TR>(f);
    }

    public class g
    {
        public static g operator +(g l, g r)
        {
            throw new NotImplementedException();
        }
    }

    public class gc
    {
        object Value;

        public gc(object value)
        {
            Value = value;
        }

        public static gc operator +(gc l, gc r)
        {
            throw new NotImplementedException();
        }
    }

    public interface IF<out TR>
    {

    }

    public class F<TR> : IF<TR>
    {
        System.Func<TR> Func;

        public F()
        {
            Func = () => default(TR);
        }

        public F(System.Func<TR> f)
        {
            Func = f;
        }

        public TR Invoke()
        {
            return Func();
        }

        public static TR operator !(F<TR> f)
        {
            return f.Func();
        }

        public static Func<TR> operator ~(F<TR> f)
        {
            return f.Func;
        }

        public static implicit operator System.Func<TR>(F<TR> f)
        {
            return f.Func;
        }
    }

    public interface IF<in T1, out TR>
    {
        TR Invoke(T1 a);
    }

    public class F<T1, TR> : IF<T1, TR>
    {
        System.Func<T1, TR> Func;

        public F()
        {
            Func = (x) => default(TR);
        }

        public F(System.Func<T1, TR> f)
        {
            Func = f;
        }

        public F<TR> this[T1 a]
        {
            get
            {
                return this + a;
            }
        }

        public TR Invoke(T1 a)
        {
            return Func(a);
        }

        public F<T1, TR2> Then<TR2>(IF<TR, TR2> f)
        {
            return F.New((T1 a) => f.Invoke(Func(a)));
        }

        public static implicit operator System.Func<T1, TR>(F<T1, TR> f)
        {
            return f.Func;
        }

        public static implicit operator F<T1, TR>(System.Func<T1, TR> f)
        {
            return new F<T1, TR>(f);
        }

        public static Func<T1, TR> operator ~(F<T1, TR> f)
        {
            return f.Func;
        }

        public static F<TR> operator +(F<T1, TR> f, T1 a)
        {
            return new F<TR>(() => f.Func(a));
        }
    }

    public class F<T1, T2, TR>
    {
        System.Func<T1, T2, TR> Func;

        public F()
        {
            Func = (a, b) => default(TR);
        }

        public F(System.Func<T1, T2, TR> f)
        {
            Func = f;
        }

        public TR Invoke(T1 a, T2 b)
        {
            return Func(a, b);
        }

        public F<T2, TR> this[T1 a]
        {
            get
            {
                return this + a;
            }
        }

        public F<TR> this[T1 a, T2 b]
        {
            get
            {
                return this + a + b;
            }
        }

        public F<TR> this[Tuple<T1, T2> t]
        {
            get
            {
                return this + t.Item1 + t.Item2;
            }
        }

        public F<T1, T2, TR2> Then<TR2>(F<TR, TR2> f)
        {
            return F.New((T1 a, T2 b) => f.Invoke(Func(a, b)));
        }

        public static Func<T1, T2, TR> operator ~(F<T1, T2, TR> f)
        {
            return f.Func;
        }

        public static implicit operator System.Func<T1, T2, TR>(F<T1, T2, TR> f)
        {
            return f.Func;
        }

        public static implicit operator F<T1, T2, TR>(System.Func<T1, T2, TR> f)
        {
            return new F<T1, T2, TR>(f);
        }

        public static F<T2, TR> operator +(F<T1, T2, TR> f, T1 a)
        {
            return new F<T2, TR>(x => f.Func(a, x));
        }

        public static F<T2, TR> operator +(F<T1, T2, TR> f, Tuple<T1> a)
        {
            return new F<T2, TR>(x => f.Func(a.Item1, x));
        }

        public static F<TR> operator +(F<T1, T2, TR> f, Tuple<T1, T2> all)
        {
            return new F<TR>(() => f.Func(all.Item1, all.Item2));
        }

        public static F<T1, T2, TR> operator |(F<T1, T2, TR> l, F<TR, TR> r)
        {
            return new F<T1, T2, TR>((a, b) => r.Invoke(l.Invoke(a, b)));
        }

        public static F<T1, T2, TR> operator |(F<T1, T2, TR> l, IF<TR, TR> r)
        {
            return new F<T1, T2, TR>((a, b) => r.Invoke(l.Invoke(a, b)));
        }

        public static F<T1, T2, TR> operator |(F<T1, T2, TR> l, System.Func<TR, TR> r)
        {
            return new F<T1, T2, TR>((a, b) => r(l.Invoke(a, b)));
        }
    }

    public class G0<TR>
    {
        Expression<Func<TR>> Exp;

        public G0(Expression<Func<TR>> exp)
        {
            Exp = exp;
        }

        public static TR operator !(G0<TR> f)
        {
            var cf = f.Exp.Compile();
            return cf();
        }
    }

    public class G1<TR>
    {
        Expression<Func<g, TR>> Exp;

        public G1(Expression<Func<g, TR>> exp)
        {
            Exp = exp;
        }

        public G0<TR> this[object a]
        {
            get
            {
                return this + a;
            }
        }

        public static G0<TR> operator +(G1<TR> f, object a)
        {
            var visitor = new CurryExpressionVisitor(a);
            var newExpression = (Expression<Func<TR>>)visitor.Visit(f.Exp);

            return new G0<TR>(newExpression);
        }
    }

    public class G2<TR>
    {
        Expression<Func<g, g, TR>> Exp;

        public G2(Expression<Func<g, g, TR>> exp)
        {
            Exp = exp;
        }

        public G1<TR> this[object a]
        {
            get
            {
                return this + a;
            }
        }

        public G0<TR> this[object a, object b]
        {
            get
            {
                return this + a + b;
            }
        }

        public static G1<TR> operator +(G2<TR> f, object a)
        {
            var visitor = new CurryExpressionVisitor(a);
            var newExpression = (Expression<Func<g, TR>>)visitor.Visit(f.Exp);

            return new G1<TR>(newExpression);
        }
    }

    public class ContainerExpressionVisitor : ExpressionVisitor
    {
        protected override Expression VisitParameter(ParameterExpression node)
        {
            throw new NotImplementedException();
            //if (node.Type == typeof(g))
            //{
            //    return Expression.Parameter(typeof(gc), )
            //}
            //return base.VisitParameter(node);
        }
    }

    public class CurryExpressionVisitor : ExpressionVisitor
    {
        object NewValue;
        Func<ParameterExpression, bool> IsFirst;

        public CurryExpressionVisitor(object newValue)
        {
            NewValue = newValue;
        }

        //Remove first parameter
        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            IsFirst = x => x == node.Parameters[0];

            var newBody = Visit(node.Body);
            var newParameters = node.Parameters.Skip(1);
            return Expression.Lambda(newBody, newParameters);
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            if (IsFirst(node)) return Expression.Constant(new gc(NewValue));
            return node;
        }
    }

    public struct EitherTask<TLeft, TRight>
    {
        bool IsLeft;
        TLeft LValue;
        TRight RValue;

        public EitherTask(TLeft value)
        {
            IsLeft = true;
            LValue = value;
            RValue = default(TRight);
        }

        public EitherTask(TRight value)
        {
            IsLeft = false;
            LValue = default(TLeft);
            RValue = value;
        }

        public T Match<T>(Func<TLeft, T> left, Func<TRight, T> right)
        {
            if (IsLeft)
            {
                return left(LValue);
            }
            else
            {
                return right(RValue);
            }
        }

        public static implicit operator EitherTask<TLeft, TRight>(TLeft left)
        {
            return new EitherTask<TLeft, TRight>();
        }

        public static implicit operator EitherTask<TLeft, TRight>(TRight right)
        {
            return new EitherTask<TLeft, TRight>();
        }
    }

    public static class Either
    {
        public static EitherTask<(T1, T2), TR> Join<T1, T2, TR>(EitherTask<T1, TR> l, EitherTask<T2, TR> r)
        {
            return l.Match(
                left: (ll) =>
                {
                    return r.Match(left: (rl) =>
                    {
                        return new EitherTask<(T1, T2), TR>((ll, rl));
                    }, right: (rr) =>
                    {
                        return new EitherTask<(T1, T2), TR>(rr);
                    });
                },
                right: (lr) =>
                {
                    return r.Match(left: (rl) =>
                    {
                        return new EitherTask<(T1, T2), TR>(lr);
                    }, right: (rr) =>
                     {
                         //TODO
                         return new EitherTask<(T1, T2), TR>(lr);
                     });
                }
            );
        }
    }

    //public static class EitherExtensions
    //{
    //    public static Task<TR> Either<T, TR>(this Task<T> task, Func<T, TR> left, Func<Exception, TR> right)
    //    {
    //        var either = new EitherTask<TR, T, Exception>();
    //        either.Bind(left, right);

    //        var ct = task.ContinueWith(t =>
    //        {
    //            if (t.IsCanceled) return either.Run(t.Exception);
    //            if (t.IsFaulted) return either.Run(t.Exception);
    //            return either.Run(t.Result);
    //        });

    //        return ct;
    //    }
    //}


    public static class ExceptionsExtensions
    {
        public static IEnumerable<Exception> GetNonAggregated(this Exception agg)
        {
            var e = agg as AggregateException;

            while (e != null)
            {
                if (e.InnerExceptions.Count == 1 && e.InnerExceptions[0] is AggregateException ae)
                {
                    e = ae;
                }
                else break;
            }

            return e?.InnerExceptions.AsEnumerable() ?? new Exception[] { };
        }
    }

    public static class AwaitLiftExtensions
    {
        public static TaskAwaiter<Task<T>> GetAwaiter<T>(this T value)
        {
            return Task.FromResult(Task.FromResult(value)).GetAwaiter();
        }

        public static void Deconstruct<T>(this Task<T> task, out T result, out AggregateException e)
        {
            if (!task.IsCompleted)
            {
                throw new NotSupportedException();
            }

            if (task.IsFaulted)
            {
                result = default(T);
                e = new AggregateException(task.Exception.GetNonAggregated());
            }
            else
            {
                result = task.Result;
                e = null;
            }
        }

        public static Task<Task<T>> Wrap<T>(this Task<T> t)
        {
            return t.ContinueWith(ct =>
            {
                return t;
            });
        }
    }

    public static class MaybeTaskExtensions
    {
        public static MaybeTask<T> Maybe<T>(this Task<T> task)
        {
            return new MaybeTask<T>(task);
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when)
        {
            return new MaybeTask<T>(task, x => when(x.Result));
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<T> returns)
        {
            return new MaybeTask<T>(task).Map(when, returns);
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<T, T> returns)
        {
            return new MaybeTask<T>(task, x => when(x.Result), x => returns(x.Result));
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<T> returns)
        {
            return new MaybeTask<T>(task, when, x => returns());
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<T, T> returns)
        {
            return new MaybeTask<T>(task, when, x => returns(x.Result));
        }

        public static MaybeTask<T> Maybe<T>(this Task<T> task, Func<T, bool> when, Func<Task<T>, T> returns)
        {
            return new MaybeTask<T>(task).Map(when, returns);
        }

        public static MonadicTask<T> Maybe<T>(this Task<T> task, Func<Task<T>, bool> when, Func<Task<T>, T> returns)
        {
            return new MaybeTask<T>(task, when, returns);
        }
    }

    public static class Maybe
    {
        public static MaybeTask<(T1, T2)> Join<T1, T2>(MaybeTask<T1> a, MaybeTask<T2> b)
        {
            var ta = unwrap(a);
            var tb = unwrap(b);
            var twa = Task.WhenAll(ta, tb);
            return twa.ContinueWith<MaybeTask<(T1, T2)>>(ct =>
            {
                if (ta.IsFaulted && tb.IsFaulted) return new AggregateException(ta.Exception.GetNonAggregated().Union(tb.Exception.GetNonAggregated()));
                if (ta.IsFaulted) return ta.Exception;
                if (tb.IsFaulted) return tb.Exception;
                return (ta.Result, tb.Result);
            }).Result;
            async Task<T> unwrap<T>(MaybeTask<T> m) => await m;
        }
    }


    public class MaybeTask<T> : MonadicTask<T>
    {
        public MaybeTask(Task<T> task) : base(task)
        {
        }

        public MaybeTask(Task<T> task, Func<Task<T>, bool> when = null, Func<Task<T>, T> returns = null) : base(task, when, returns)
        {
        }

        public MaybeTask<T> Map(Func<Task<T>, bool> when, Func<Task<T>, T> returns = null)
        {
            _when = when;
            _returns = returns;
            return this;
        }

        public MaybeTask<T> Map(Func<T, bool> when, Func<Task<T>, T> returns)
        {
            _when = x => !x.IsCanceled && !x.IsFaulted && when(x.Result);
            if (returns != null) _returns = returns;
            return this;
        }

        public MaybeTask<T> Map(Func<T, bool> when, Func<T> returns = null)
        {
            _when = x => !x.IsCanceled && !x.IsFaulted && when(x.Result);
            if (returns != null) _returns = x => returns();
            return this;
        }

        public static MaybeTask<(T, T)> operator +(MaybeTask<T> l, MaybeTask<T> r)
        {
            return Maybe.Join(l, r);
        }

        public static implicit operator MaybeTask<T>(Task<T> value)
        {
            return new MaybeTask<T>(value);
        }

        public static implicit operator MaybeTask<T>(T value)
        {
            return new MaybeTask<T>(Task.FromResult(value));
        }

        public static implicit operator MaybeTask<T>(Exception value)
        {
            return new MaybeTask<T>(Task.FromException<T>(value));
        }
    }


    public class MonadicTask<T>
    {
        private readonly Task<T> _task;
        protected Func<Task<T>, bool> _when;
        protected Func<Task<T>, T> _returns;

        public MonadicTask(Task<T> task, Func<Task<T>, bool> when = null, Func<Task<T>, T> returns = null)
        {
            _task = task;
            _when = when;
            _returns = returns;
        }

        public MonadicTask(Task<T> task)
        {
            _task = task;
        }


        public MonadicAwaiter<T> GetAwaiter()
        {
            return new MonadicAwaiter<T>(_task, _when, _returns);
        }

        public static implicit operator MonadicTask<T>(Task<T> t)
        {
            return new MonadicTask<T>(t);
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
            _when = when ?? (x => !x.IsCanceled && !x.IsFaulted && System.Collections.Generic.EqualityComparer<T>.Default.Equals(x.Result, default(T)));
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

    public static class ObjectF
    {
        public new static Func<object, string> ToString
        {
            get
            {
                return x => x.ToString();
            }
        }
    }

    public class HandleApplicationException<TR, TMe, T1>
           where TMe : ApplicationException<TMe, T1>
           where T1 : Exception
    {
        public HandleApplicationException(ApplicationException<TMe, T1> app)
        {
        }

        public EndClass When(Func<T1> handle)
        {
            return new EndClass();
        }

        public class EndClass
        {
            public TR End()
            {
                return default(TR);
            }
        }
    }

    public class HandleApplicationException<TR, TMe, T1, T2>
        where TMe : ApplicationException<TMe, T1, T2>
        where T1 : Exception
        where T2 : Exception
    {
        ApplicationException<TMe, T1, T2> App;

        public HandleApplicationException(ApplicationException<TMe, T1, T2> app)
        {
            App = app;
        }

        public ContinueT2 When(Func<T1, TR> handle)
        {
            return new ContinueT2();
        }

        public class ContinueT2
        {
            public EndClass When(Func<T2, TR> handle)
            {
                return new EndClass();
            }
        }

        public class EndClass
        {
            public TR End()
            {
                return default(TR);
            }
        }
    }



    public class ApplicationException<TMe, T1> : Exception
        where TMe : ApplicationException<TMe, T1>
        where T1 : Exception
    {
        protected ApplicationException(Exception e) : base("", e)
        {
        }

        public HandleApplicationException<TR, TMe, T1> StartHandle<TR>()
        {
            return new HandleApplicationException<TR, TMe, T1>(this);
        }

        public static implicit operator ApplicationException<TMe, T1>(T1 e)
        {
            return new ApplicationException<TMe, T1>(e);
        }

        public static TMe Throw(T1 exception)
        {
            return (TMe)Activator.CreateInstance(typeof(TMe), new object[] { exception });
        }

    }

    public class ApplicationException<TMe, T1, T2> : Exception
        where TMe : ApplicationException<TMe, T1, T2>
        where T1 : Exception
        where T2 : Exception
    {
        protected ApplicationException(Exception e) : base("", e)
        {
        }

        public HandleApplicationException<TR, TMe, T1, T2> StartHandle<TR>()
        {
            return new HandleApplicationException<TR, TMe, T1, T2>(this);
        }

        public static implicit operator ApplicationException<TMe, T1, T2>(T1 e)
        {
            return new ApplicationException<TMe, T1, T2>(e);
        }

        public static implicit operator ApplicationException<TMe, T1, T2>(T2 e)
        {
            return new ApplicationException<TMe, T1, T2>(e);
        }

        public static TMe Throw(T1 exception)
        {
            return (TMe)Activator.CreateInstance(typeof(TMe), new object[] { exception });
        }

        public static TMe Throw(T2 exception)
        {
            return (TMe)Activator.CreateInstance(typeof(TMe), new object[] { exception });
        }
    }
}
