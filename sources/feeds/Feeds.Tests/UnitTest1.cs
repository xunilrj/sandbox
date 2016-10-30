using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Linq;
using System.Reactive.Subjects;
using System.Collections.Concurrent;
using System.Threading;
using Metrics;
using Metrics.Core;
using System.Threading.Tasks.Dataflow;
using System.Diagnostics;
using System.Reflection;
using System.Dynamic;
using Castle.DynamicProxy;

namespace Feeds.Tests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public async Task TestMethod1()
        {
            //var gateway = new GoodGateway();
            //var trader = new AutomaticTrader(gateway);

            //trader.Start("A", Operation.Buy, 10, 100);

            //var t1 = gateway.Simulate(new FeedUpdateEventArgs()
            //{
            //    Symbol = "A",
            //    Operation = Operation.Sell,
            //    Price = 9,
            //    Quantity = 100
            //});
            //var t2 = gateway.Simulate(new FeedUpdateEventArgs()
            //{
            //    Symbol = "A",
            //    Operation = Operation.Sell,
            //    Price = 9.1m,
            //    Quantity = 100
            //});

            //await Task.WhenAll(t1, t2);

            //gateway.Print();
        }

        [TestMethod]
        public void ConsoleWhiskey()
        {

        }

        [TestMethod]
        public void ThrouputCalculator()
        {
            var gateway = new GoodGateway();
            var trader = new AutomaticTrader(gateway);

            trader.Start("A", Operation.Buy, 10, 10);

            //ThreadPool.SetMaxThreads(20, 20);

            Parallel.For(0, 10000, x =>
            {
                gateway.Simulate(new FeedUpdateEventArgs()
                {
                    Symbol = "A",
                    Operation = Operation.Sell,
                    Price = 9,
                    Quantity = 1
                });
            });

            Thread.Sleep(5000);

            var times = GoodGateway.Times.OrderBy(x => x).ToList();
            var total = GoodGateway.Times.Sum();
            var count = GoodGateway.Times.Count();
            var p99 = times.Quantile(1000, 999);
            var max = times.Max();
            var min = times.Min();

            Console.WriteLine("Maximum:" + max.ToString("N10"));
            Console.WriteLine("Minimum:" + min.ToString("N10"));
            Console.WriteLine("Quantile 999/1000:" + p99.ToString("N10"));
            Console.WriteLine("Mean:" + (total / count).ToString("N10"));

            //var value = GoodGateway.timer.GetValue();
            //value = value.Scale(TimeUnit.Milliseconds, TimeUnit.Milliseconds);

            //Console.WriteLine("Count:" + value.Histogram.SampleSize);
            //Console.WriteLine("Min:".PadRight(15) + value.Histogram.Min.ToString("N6"));
            //Console.WriteLine("Median:".PadRight(15) + value.Histogram.Median.ToString("N6"));
            //Console.WriteLine("Percentile75:".PadRight(15) + value.Histogram.Percentile75.ToString("N6"));
            //Console.WriteLine("Percentile95:".PadRight(15) + value.Histogram.Percentile95.ToString("N6"));
            //Console.WriteLine("Percentile99:".PadRight(15) + value.Histogram.Percentile99.ToString("N6"));
            //Console.WriteLine("Percentile999:".PadRight(15) + value.Histogram.Percentile999.ToString("N6"));
            //Console.WriteLine("Max:".PadRight(15) + value.Histogram.Max.ToString("N6"));
            gateway.Print();
        }

        [TestMethod]
        public void MyTestMethod()
        {
            var watch = new Stopwatch();
            double total = 0;

            foreach (var item in Enumerable.Range(0, 1000000))
            {
                watch.Reset();
                watch.Start();
                watch.Stop();

                total += watch.ElapsedTicks;
            }

            Console.WriteLine(total);
        }

        [TestMethod]
        public void MyTestMethod2()
        {
            double total = 0;

            foreach (var item in Enumerable.Range(0, 1000000))
            {
                var start = Stopwatch.GetTimestamp();
                var stop = Stopwatch.GetTimestamp();

                total += stop - start;
            }

            Console.WriteLine(total);
        }

        [TestMethod]
        public async Task DDDS()
        {
            for (int i = 0; i < 10; ++i)
            {
                var scheduler = new Scheduler();

                var incrementer = new Incrementer();

                var pg = new ProxyGenerator();
                var proxy = pg.CreateInterfaceProxyWithoutTarget<IIncrementer>(new ScheduleInterceptor(scheduler));

                var result = Parallel.For(0, 10000, new ParallelOptions()
                {
                    MaxDegreeOfParallelism = 4
                }, x =>
                {
                    proxy.Increment();
                });

                scheduler.Stop();

                await scheduler.Completion;

                Console.WriteLine(incrementer.Value);
            }
        }
    }

    public interface IFeeds
    {
        IEnumerable<string> GetSymbols();
    }

    public class FeedUpdateEventArgs : EventArgs
    {
        public string Symbol { get; set; }
        public Operation Operation { get; set; }
        public decimal Price { get; set; }
        public decimal Quantity { get; set; }
    }

    public interface ITradeGateway
    {
        IObservable<FeedUpdateEventArgs> Subscribe(string symbol);
        void Unsubscribe(string symbol);

        bool Order(Operation op, string symbol, decimal price, decimal quantity);
    }

    public class GoodGateway : ITradeGateway
    {
        ConcurrentQueue<string> Queue = new ConcurrentQueue<string>();

        public void Print()
        {
            foreach (var item in Queue)
            {
                Console.WriteLine(item);
            }
        }

        public bool Order(Operation op, string symbol, decimal price, decimal quantity)
        {
            Queue.Enqueue($"{DateTime.UtcNow} {Thread.CurrentThread.ManagedThreadId} Order {op} {symbol} {price} {quantity}");
            return true;
        }

        Dictionary<string, Subject<FeedUpdateEventArgs>> Subjects = new Dictionary<string, Subject<FeedUpdateEventArgs>>();

        public IObservable<FeedUpdateEventArgs> Subscribe(string symbol)
        {
            Subject<FeedUpdateEventArgs> s = null;
            if (Subjects.TryGetValue(symbol, out s) == false)
            {
                s = new Subject<FeedUpdateEventArgs>();
                Subjects.Add(symbol, s);
            }

            return s;
        }

        public static TimerMetric timer = (TimerMetric)Metric.Timer("HTTP Requests", Unit.Requests);

        ActionBlock<FeedUpdateEventArgs> Block;

        public GoodGateway()
        {
            //Block = new ActionBlock<FeedUpdateEventArgs>(e =>
            //{
            //using (timer.NewContext())
            //{
            //    Subjects[e.Symbol].OnNext(e);
            //}
            //}, new ExecutionDataflowBlockOptions()
            //{
            //    MaxDegreeOfParallelism = 4
            // }
            //);
        }

        int count = 0;
        ManualResetEvent mre = new ManualResetEvent(false);

        public Task Simulate(FeedUpdateEventArgs e)
        {
            var r = Interlocked.Increment(ref count);

            if (r > 5000)
            {
                mre.Set();
            }

            return Task.Factory.StartNew(() =>
            {
                mre.WaitOne();

                Stopwatch watch = new Stopwatch();
                watch.Reset();
                watch.Start();

                Subjects[e.Symbol].OnNext(e);

                watch.Stop();

                var ms = watch.Elapsed.TotalMilliseconds;
                Times.Add(ms);
            });
        }

        public static ConcurrentBag<double> Times = new ConcurrentBag<double>();

        public void Unsubscribe(string symbol)
        {
            Queue.Enqueue($"{DateTime.UtcNow} {Thread.CurrentThread.ManagedThreadId} Unsubscribe {symbol}");
        }
    }

    public enum Operation
    {
        Buy,
        Sell
    };

    public class AutomaticTrader
    {
        ITradeGateway Gateway;

        string Symbol;
        decimal TargetPrice;
        decimal RemainingQuantity;

        object sync = new object();

        public AutomaticTrader(ITradeGateway gateway)
        {
            Gateway = gateway;
        }

        public void Start(string symbol, Operation op, decimal price, decimal quantity)
        {
            Symbol = symbol;
            TargetPrice = price;
            RemainingQuantity = quantity;

            var updates = Gateway.Subscribe(symbol);
            updates.Subscribe(Handle);
        }

        private void Handle(FeedUpdateEventArgs e)
        {
            //lock (sync)
            {
                if (RemainingQuantity == 0)
                {
                    return;
                }

                bool result = false;

                if (e.Symbol == e.Symbol)
                {
                    if (e.Price <= TargetPrice)
                    {
                        var bestPrice = Math.Min(TargetPrice, e.Price);
                        var bestQuantity = Math.Min(RemainingQuantity, e.Quantity);

                        result = Gateway.Order(Operation.Buy, Symbol, bestPrice, bestQuantity);

                        if (result)
                        {
                            RemainingQuantity -= bestQuantity;
                        }
                    }
                }

                if (RemainingQuantity <= 0)
                {
                    Gateway.Unsubscribe(Symbol);
                }
            }
        }
    }

    public static class StatiticsLinq
    {
        public static T Percentile<T>(this List<T> items, int q)
        {
            return items[q * items.Count * (int)Math.Round(1.0 / 100.0)];
        }

        public static T Quantile<T>(this List<T> items, int size, int q)
        {
            return items[q * items.Count * (int)Math.Round(1.0 / (double)size)];
        }
    }

    //public interface IIncrementer
    //{
    //    void Increment();
    //    Task IncrementAsync();
    //}

    public class TaskSchedulerSynchronizationContext : SynchronizationContext
    {
        TaskFactory Factory;

        public TaskSchedulerSynchronizationContext(TaskScheduler scheduler)
        {
            Factory = new TaskFactory(scheduler);
        }

        public override void Post(SendOrPostCallback d, object state)
        {
            Factory.StartNew(() =>
            {
                d.Invoke(state);
            });
        }
    }

    //public class ActiveObjectProxyIncrementer : IIncrementer
    //{
    //    TaskFactory Factory;
    //    Scheduler Scheduler;
    //    IIncrementer Servant;

    //    public ActiveObjectProxyIncrementer(Scheduler scheduler, IIncrementer servant)
    //    {
    //        Scheduler = scheduler;
    //        Factory = new TaskFactory(Scheduler);

    //        Servant = servant;
    //    }

    //    public void Increment()
    //    {
    //        Scheduler.Capture(() =>
    //            {
    //                Servant.IncrementAsync();
    //            });
    //    }

    //    public Task IncrementAsync()
    //    {
    //        throw new NotImplementedException();
    //    }
    //}

    public class Scheduler : TaskScheduler
    {
        public BlockingCollection<Task> Activations = new BlockingCollection<Task>();

        TaskCompletionSource<bool> TCS;

        public Task Completion { get { return TCS.Task; } }

        TaskSchedulerSynchronizationContext Sync;

        public Scheduler()
        {
            Sync = new TaskSchedulerSynchronizationContext(this);
            TCS = new TaskCompletionSource<bool>();

            Task.Factory.StartNew(() =>
            {
                while (true)
                {
                    Task invocation = null;
                    var takeSucceed = Activations.TryTake(out invocation);

                    if (takeSucceed)
                    {
                        TryExecuteTask(invocation);
                    }
                    else
                    {
                        if (Activations.IsCompleted)
                        {
                            break;
                        }
                    }
                }

                TCS.SetResult(true);
            });
        }

        public void Stop()
        {
            Activations.CompleteAdding();
        }

        public void Capture(Action action)
        {
            var old = SynchronizationContext.Current;

            try
            {
                SynchronizationContext.SetSynchronizationContext(Sync);

                action();
            }
            finally
            {
                SynchronizationContext.SetSynchronizationContext(old);
            }
        }

        protected override void QueueTask(Task task)
        {
            Activations.Add(task);
        }

        protected override bool TryExecuteTaskInline(Task task, bool taskWasPreviouslyQueued)
        {
            return false;
        }

        protected override IEnumerable<Task> GetScheduledTasks()
        {
            return Activations;
        }
    }

    public class MethodInvocation
    {
        public Action Invoke { get; set; }
        public Func<bool> Guard { get; set; }

        public MethodInvocation()
        {
            Guard = () => true;
        }
    }

    public interface IIncrementer
    {
        void Increment();
    }

    public class Incrementer : IIncrementer
    {
        public int Value;

        public void Increment()
        {
            Value++;
        }
    }

    public class DynamicProxy : DynamicObject
    {
        object Instance;
        TaskFactory Factory;

        public DynamicProxy(object instance, Scheduler scheduler)
        {
            Instance = instance;
            Factory = new TaskFactory(scheduler);
        }
        
        public override bool TryInvokeMember(InvokeMemberBinder binder, object[] args, out object result)
        {
            result = null;

            Factory.StartNew(() =>
            {                
                var type = Instance.GetType();
                var methodName = binder.Name;
                
                type.InvokeMember(methodName,
                        BindingFlags.Public |
                        BindingFlags.Instance |
                        BindingFlags.InvokeMethod,
                        null,
                        Instance,
                        args);
            });

            return true;
        }
    }

    public class ScheduleInterceptor : IInterceptor
    {
        TaskFactory Factory;
        object Instance;

        public ScheduleInterceptor(Scheduler scheduler, object instance)
        {
            Factory = new TaskFactory(scheduler);
            Instance = instance;
        }

        public void Intercept(IInvocation invocation)
        {
            var methodName = invocation.Method.Name;
            var args = invocation.Arguments;

            Factory.StartNew(() =>
            {
                var type = Instance.GetType();

                type.InvokeMember(methodName,
                        BindingFlags.Public |
                        BindingFlags.Instance |
                        BindingFlags.InvokeMethod,
                        null,
                        Instance,
                        args);
            });
        }
    }
}
