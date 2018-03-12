using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace MachinaAurum.CSharp
{
    internal static class TaskExtensions
    {
        public static void KillTaskFromContinuation(Action continuation)
        {
            System.Diagnostics.Debug.WriteLine(continuation.Target.GetType());
            //.NET Task.
            //We will hijack its StateMachine and kill the task.
            // Unfortunately we cannot access the TaskBuilder to do it
            // more cleanly.
            // That is why we have our Task<T> class.
            var target = continuation.Target;
            if (target.GetType().Namespace.StartsWith("System"))
            {
                var machineState = GetStateMachine(continuation);
                machineState.Finish();
            }
            else
            {
                continuation();
            }
        }

        private static IAsyncStateMachine GetStateMachine(Action continuation)
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

        public static void Finish(this IAsyncStateMachine stateMachine)
        {
            stateMachine.GetType()
                .GetField("<>1__state", BindingFlags.Public | BindingFlags.Instance)
                .SetValue(stateMachine, -2);
            var builder = stateMachine.GetType()
                .GetField("<>t__builder", BindingFlags.Public | BindingFlags.Instance)
                .GetValue(stateMachine);

            var taskTypes = builder.GetType().GetGenericArguments();
            if (taskTypes.Length == 0)
            {
                ((AsyncTaskMethodBuilder)builder).SetResult();
            }
            else
            {
                typeof(TaskExtensions)
                    .GetMethod(nameof(SetDefault))
                    .MakeGenericMethod(taskTypes[0])
                    .Invoke(null, new object[] { builder });
            }
        }

        public static void SetDefault<T>(AsyncTaskMethodBuilder<T> builder)
        {
            builder.SetResult(default(T));
        }
    }
}
