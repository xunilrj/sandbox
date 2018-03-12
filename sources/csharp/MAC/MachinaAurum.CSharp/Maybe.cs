using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace MachinaAurum.CSharp
{
    public interface IMaybeProvider<T>
    {
        Maybe<T> Maybe { get; }
    }

    internal enum State { NotReady, Empty, Filled };

    public static class Maybe
    {
        /// <summary>
        /// Returns a Empty Maybe.
        /// </summary>
        public static Unit None => Unit.Instance;
    }

    [AsyncMethodBuilder(typeof(MaybeBuilder<>))]
    public struct Maybe<T> : IMaybeProvider<T>
    {
        /// <summary>
        /// For later support for real async Maybe functions;
        /// </summary>
        /// <returns></returns>
        static internal Maybe<T> NotReady()
        {
            var m = new Maybe<T>(Unit.Instance);
            m._State = State.NotReady;
            return m;
        }

        private State _State;
        private T _Value;

        public Maybe(T v)
        {
            _State = State.NotReady;
            _Value = default;

            Value = v;
        }

        private Maybe(Unit u)
        {
            _State = State.Empty;
            _Value = default;
        }

        public Maybe<T2> Bind<T2>(Func<T, Maybe<T2>> some = null, Func<Maybe<T2>> none = null)
        {
            if (_State == State.NotReady) throw new InvalidProgramException();

            if (some == null) some = (x) => default;
            if (_State == State.Empty)
            {
                if (none == null) return Unit.Instance;
                else return none();
            }
            else return some(Value);
        }

        public MaybeAwaiter<T> GetAwaiter()
        {
            return new MaybeAwaiter<T>(this);
        }

        public static implicit operator Maybe<T>(T v) => new Maybe<T>(v);
        public static implicit operator Maybe<T>(Unit v) => new Maybe<T>(v);
        Maybe<T> IMaybeProvider<T>.Maybe => this;

        internal bool IsNotReady => _State == State.NotReady;

        /// <summary>
        /// Only used to mutate the internal state by the async/await Builder
        /// mechanism.
        /// </summary>
        internal T Value
        {
            get => _Value;
            set
            {
                if (value != null)
                {
                    _State = State.Filled;
                    _Value = value;
                }
                else
                {
                    Clear();
                }
            }
        }

        /// <summary>
        /// Only used to mutate the internal state by the async/await Builder
        /// mechanism.
        /// </summary>
        internal void Clear()
        {
            _State = State.Empty;
            _Value = default;
        }
    }

    internal interface IMaybeAwaiter
    {
        void ReturnEmpty();
    }

    public class MaybeAwaiter<T> : INotifyCompletion, IMaybeProvider<T>, IMaybeAwaiter
    {
        List<Action> Cont = new List<Action>();
        IMaybeProvider<T> Maybe;

        public bool IsCompleted { get; set; }
        public T GetResult() => Maybe.Maybe.Value;

        public MaybeAwaiter(IMaybeProvider<T> m)
        {
            IsCompleted = false;
            Maybe = m;
        }

        void INotifyCompletion.OnCompleted(Action continuation)
        {
            if (Maybe.Maybe.IsNotReady) Cont.Add(continuation);
            else
            {
                TaskExtensions.KillTaskFromContinuation(continuation);
            }
        }

        public void ReturnEmpty() => Cont.ForEach(c => c());
        Maybe<T> IMaybeProvider<T>.Maybe => Maybe.Maybe;
    }

    public static class MaybeExtensions
    {
        public static Maybe<T3> Bind<T1, T2, T3>(this (Maybe<T1> Left, Maybe<T2> Right) item, Func<T1, T2, T3> f)
            => item.Left.Bind(xl => item.Right.Bind(xr => f(xl, xr)));

        public static Maybe<T3> Bind<T1, T2, T3>(this (T1 Left, Maybe<T2> Right) item, Func<T1, T2, T3> f)
            => new Maybe<T1>(item.Left).Bind(xl => item.Right.Bind(xr => f(xl, xr)));

        public static Maybe<T> Bind<T>(this Maybe<T> m, Action<T> some = null, Action none = null)
            => m.Bind(new Func<T, Maybe<T>>(x => { some?.Invoke(x); return x; }),
                () => { none?.Invoke(); return Unit.Instance; });

        public static Maybe<T2> Bind<T, T2>(this Maybe<T> m, Func<T, T2> some = null, Func<T2> none = null)
            => m.Bind(new Func<T, Maybe<T2>>(x =>
            {
                some = some ?? new Func<T, T2>((_) => default);
                return some(x);
            }),
                () => { none?.Invoke(); return Unit.Instance; });

        public static Maybe<T> AsMaybe<T>(this T value, Func<T, bool> f)
        {
            var r = f(value);
            if (!r) return value;
            else return Unit.Instance;
        }
    }

    public class MaybeBuilder<T>
    {
        public static MaybeBuilder<T> Create() => new MaybeBuilder<T>();

        Maybe<T> Maybe;

        public MaybeBuilder()
        {
            Maybe = Maybe<T>.NotReady();
        }

        public void Start<TStateMachine>(ref TStateMachine stateMachine) where TStateMachine : IAsyncStateMachine
        {
            stateMachine.MoveNext();
        }
        public void SetStateMachine(IAsyncStateMachine stateMachine)
        {
        }
        public void SetResult(T result) => Maybe.Value = result;
        public void SetException(Exception exception)
        {
            Maybe.Clear();
        }
        public ref Maybe<T> Task => ref Maybe;
        public void AwaitOnCompleted<TAwaiter, TStateMachine>(ref TAwaiter awaiter, ref TStateMachine stateMachine)
            where TAwaiter : INotifyCompletion where TStateMachine : IAsyncStateMachine
        {
            if (awaiter is IMaybeProvider<T> p)
            {
                var localStateMachine = stateMachine;
                p.Maybe.Bind((_) => localStateMachine.MoveNext(), () =>
                {
                    Maybe.Clear();
                });
            }
        }
        public void AwaitUnsafeOnCompleted<TAwaiter, TStateMachine>(ref TAwaiter awaiter, ref TStateMachine stateMachine)
            where TAwaiter : ICriticalNotifyCompletion where TStateMachine : IAsyncStateMachine
        {
        }
    }
}
