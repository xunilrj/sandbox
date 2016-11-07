# Problem

We have a list of feeds.

- Subscribe to each feed;
- Unsubscribe to each feed

- Process each feed event


References:

http://www.cse.wustl.edu/~schmidt/PDF/Act-Obj.pdf  
http://www.cse.wustl.edu/~schmidt/PDF/monitor.pdf


# Implementation

Our first task is to implement a AutomaticTrader.
This automatic trader subscribe itself in a Feed source.
And if its receive an update with an update from someone who wants to sell
to a lower price that your maximum when buying you want that the
automatic trader complete the transaction.

The first thing that we must do, it is to setup our environment.

    var trader = new AutomaticTrader(gateway);
    trader.Start("A", Operation.Buy, 10, 100);

This means that we want to buy 100 shares from company A with at maximun
price 10.

To simulate two very fast updates from the real source we can do:

    var t1 = gateway.Simulate(new FeedUpdateEventArgs()
    {
        Symbol = "A",
        Operation = Operation.Sell,
        Price = 9,
        Quantity = 100
    });
    var t2 = gateway.Simulate(new FeedUpdateEventArgs()
    {
        Symbol = "A",
        Operation = Operation.Sell,
        Price = 9.1m,
        Quantity = 100
    });

    await Task.WhenAll(t1, t2);

This simulate operation just insert a new event into the IObversable on its own Task.

    public Task Simulate(FeedUpdateEventArgs e)
    {
        return Task.Factory.StartNew(() =>
        {
            Subjects[e.Symbol].OnNext(e);
        });
    }

The naive implementation would do something like:

    private void Handle(FeedUpdateEventArgs e)
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

Each time that you run this code, you will get a different result. For example:

26/10/2016 18:48:32 Order Buy A 9.1 100
26/10/2016 18:48:32 Unsubscribe A
26/10/2016 18:48:32 Order Buy A 9 0
26/10/2016 18:48:32 Unsubscribe A

Events were processed in the reverse order. And the Order Processor received a order with quantity zero.

26/10/2016 18:49:06 Order Buy A 9 100
26/10/2016 18:49:06 Unsubscribe A

Everything went fine.

26/10/2016 18:49:18 Order Buy A 9.1 100
26/10/2016 18:49:18 Order Buy A 9 100
26/10/2016 18:49:18 Unsubscribe A
26/10/2016 18:49:18 Unsubscribe A

You did not paid the best price, have ordered more than the normal quantity and the gateway have received two unsubscribes.

The first obvious correction is:

    private void Handle(FeedUpdateEventArgs e)
    {
        lock (this)
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

It does not matter how many times did you ran this code.
All times the same result would appear.

26/10/2016 18:51:49 Order Buy A 9 100
26/10/2016 18:51:49 Unsubscribe A

So, what was our problem, and why this solutions soled the problem?
One way of describing the problem is:

    Many applications contain objects that are accessed concurrently
    by multiple client threads. For concurrent applications
    to execute correctly, therefore, it is often necessary to synchronize
    and schedule access to these objects.

So, one solution to this problem, is the Pattern know ans: Monitor Object:

http://www.cse.wustl.edu/~schmidt/PDF/monitor.pdf

The Monitor Pattern, solves this problem with three steps:

1. Synchronization boundaries should correspond to object methods;
2. Objects, not clients, should be responsible for their own method synchronization;
3. Objects should be able to schedule their methods cooperatively.

Althought this solved the problem, this solution created another problems:
1. lock(this) creates a problems because the locked object is easily accesible:
see:
https://msdn.microsoft.com/en-us/library/mt679037.aspx
    For example, lock(this) can be problematic if the instance can be accessed publicly,
    because code beyond your control may lock on the object as well. This could create
    deadlock situations where two or more threads wait for the release of the same object.
    Locking on a public data type, as opposed to an object, can cause problems for the same
    reason. Locking on literal strings is especially risky because literal strings are
    interned by the common language runtime (CLR). This means that there is one instance
    of any given string literal for the entire program, the exact same object represents
    the literal in all running application domains, on all threads. As a result, a lock
    placed on a string with the same contents anywhere in the application process locks
    all instances of that string in the application. As a result, it is best to lock a
    private or protected member that is not interned.

To understand why this is bad, frist we must understand hol the lock keyword is implemented:

The lock keyword is actually a suger to:

    lock(...)
    {
        ...
    }

is equal to

    Monitor.Enter(...);
    try
    {
        ...        
    }
    finally
    {
        Monitor.Exit(...);
    }

As always, to a response nearer the truth see:
https://blogs.msdn.microsoft.com/ericlippert/2009/03/06/locks-and-exceptions-do-not-mix/

But to understand what the Monitor.Enter and Monitor.Exit does, let us see the direct source code 
from the CLR.

https://github.com/dotnet/coreclr/blob/32f0f9721afb584b4a14d69135bea7ddc129f755/src/vm/syncblk.cpp#L1892

    void ObjHeader::EnterObjMonitor()
    {
        WRAPPER_NO_CONTRACT;
        GetSyncBlock()->EnterMonitor();
    }

https://github.com/dotnet/coreclr/blob/32f0f9721afb584b4a14d69135bea7ddc129f755/src/vm/syncblk.cpp#L2581

A simplified version of the code is:

    SyncBlock *ObjHeader::GetSyncBlock()
    {
        PTR_SyncBlock syncBlock = GetBaseObject()->PassiveGetSyncBlock();

        if (syncBlock)
            RETURN syncBlock;

        //Need to get it from the cache
        DWORD indx = SyncBlockCache::GetSyncBlockCache()->NewSyncBlockSlot(GetBaseObject());
        new (syncBlock) SyncBlock(indx);
        RETURN syncBlock;
    }

https://github.com/dotnet/coreclr/blob/32f0f9721afb584b4a14d69135bea7ddc129f755/src/vm/syncblk.h#L765

    class SyncBlock
    {
    protected:
        AwareLock  m_Monitor; 
    public:      
        void EnterMonitor()
        {
            WRAPPER_NO_CONTRACT;
            m_Monitor.Enter();
        }
    }

This code finds an empty index in the cache and allocates a new SyncBlock at that position.

https://github.com/dotnet/coreclr/blob/32f0f9721afb584b4a14d69135bea7ddc129f755/src/vm/syncblk.cpp#L2825

void AwareLock::Enter()
{
    Thread  *pCurThread = GetThread();
    for (;;) 
    {
        // Read existing lock state.
        LONG state = m_MonitorHeld.LoadWithoutBarrier();
        if (state == 0) 
        {
            // Common case: lock not held, no waiters. Attempt to acquire lock by
            // switching lock bit.
            if (FastInterlockCompareExchange((LONG*)&m_MonitorHeld, 1, 0) == 0)
            {
                break;
            }
        } 
        else
        {
            if (m_HoldingThread == pCurThread)
            {    
                //Recursion
            }

            // Attempt to increment this count of waiters then goto contention
            // handling code.
            if (FastInterlockCompareExchange((LONG*)&m_MonitorHeld, (state + 2), state) == state)
            {
                 //Must Wait
            }
        }
    }
}

Must wait will somehow call the function 

https://github.com/dotnet/coreclr/blob/32f0f9721afb584b4a14d69135bea7ddc129f755/src/vm/threads.h#L3501

The important part here is the comment of the method:
 // Either perform WaitForSingleObject or MsgWaitForSingleObject as appropriate.
    DWORD          DoAppropriateWait(int countHandles, HANDLE *handles, BOOL waitAll,
                                     DWORD millis, WaitMode mode,
                                     PendingSync *syncInfo = 0);


We can find the documentation for this method at:

https://msdn.microsoft.com/en-us/library/windows/desktop/ms687032(v=vs.85).aspx

For even more information see:

https://www.amazon.com/Windows-via-5th-Developer-Reference/dp/0735663777

OK!
Now we are in the following situation.

1. Every object have a syncblock in a shared cache.
2. Lock keyword in reality is just the Monitor.Enter
3. Monitor.Enter 
3.1. Make a fast interlocked increment if there is no one running.
3.2. Make a Windows API call if someone is already running.

To understand a little more about Interlocked* see
https://msdn.microsoft.com/en-us/library/windows/desktop/ee418650(v=vs.85).aspx

But even if this techniques, writing lock-free code is very hard:
http://www.drdobbs.com/cpp/lock-free-code-a-false-sense-of-security/210600279
or
http://www.talisman.org/~erlkonig/misc/herb+lock-free-code/p1-lock-free-code--a-false-sense-of-security.html

This article explains that common problems when trying to write lock-free code:
1. Non-atomic operations;
2. Not guaranteed write-release and read-acquire order.

So returning to the lock(this) problem. Now we know that every instance can have an
associated sync block.

Because of this everyone that have access to a reference can lock that reference.
Obviously both inside nad outside the class have access to the reference.
That is why is a bad practice to use lock this.

Another bad idea is to use a value type in the lock.
That would cause the value type to be boxed and the lock be created
in the new reference. But given that the box process will happen 
every time, you will never get the same lock twice. Which, off course,
is the same as not using lock at all.

So, our best approach for now is to do:

    public class AutomaticTrader
    {
       ...
        object sync = new object();
        ...
        private void Handle(FeedUpdateEventArgs e)
        {
            lock (sync)
            {
                ...
            }
        }
    }

Times:

No Multithread

    foreach (var item in Enumerable.Range(0, 10000))
    {
        var t1 = gateway.Simulate(new FeedUpdateEventArgs()
        {
            Symbol = "A",
            Operation = Operation.Sell,
            Price = 9,
            Quantity = 1
        });
    }

    public Task Simulate(FeedUpdateEventArgs e)
    {
        watch.Reset();
        watch.Start();

        Subjects[e.Symbol].OnNext(e);

        watch.Stop();
        
        var ms = watch.Elapsed.TotalMilliseconds;
        Total += ms;
        Count++;

        Minimun = Math.Min(Minimun, ms);
        Maximun = Math.Max(Maximun, ms);

        return Task.FromResult(true);
    }

Maximum:1.9171000000
Minimum:0.0006000000
Quantile 999/1000:0.0006000000
Mean:0.0010928314

The majority of the calls takes a fraction of millisecond. But one or two calls
takes more than one millisecond because their thread was preempted.
In a preempted OS that is nothing you can do.

# Incrementer

    public class Incrementer
    {
        public int Value;

        public void Increment()
        {
            Value++;
        }
    }

    for (int i = 0; i < 10; ++i)
    {
        Incrementer inc = new Incrementer();

        var result = Parallel.For(0, 10000, new ParallelOptions()
        {
            MaxDegreeOfParallelism = 2
        }, x =>
        {
            inc.Increment();
        });

        Console.WriteLine(inc.Value);
    }

Results

10000
9999
9999
9999
9997
9999
10000
9999
10000
9999

Changing this to 

    MaxDegreeOfParallelism = 1

10000
10000
10000
10000
10000
10000
10000
10000
10000
10000

OK. So we have a Race Condition.
One of the first solution to these kind of problems is to use the Active Object.

The first step is to create a interface to this class:

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



    In this phase, the client invokes a method on the Proxy

        Incrementer incrementer = new Incrementer();
        var scheduler = new Scheduler();
        var proxy = new ActiveObjectProxyIncrementer(scheduler, incrementer);
        var result = Parallel.For(0, 10000, new ParallelOptions(){MaxDegreeOfParallelism = 4},
            x =>
            {
                proxy.Increment();
            });

    This triggers the creation of a Method Request, which maintains the
    argument bindings to the method, as well as any other bindings
    required to execute the method and return its results.

        public class ActiveObjectProxyIncrementer : IIncrementer
        {
            Scheduler Scheduler;
            IIncrementer Servant;

            public ActiveObjectProxyIncrementer(Scheduler scheduler, IIncrementer servant)
            {
                Scheduler = scheduler;
                Servant = servant;
            }

            public void Increment()
            {
                var methodInvocation = new MethodInvocation()
                {
                    Instance = Servant,
                    Name = "Increment",
                    Parameters = new object[] { }
                };                
            }
        }

    The Proxy then passes the Method Request to the Scheduler

    public void Increment()
    {
        var methodInvocation = new MethodInvocation()
        {
            Instance = Servant,
            Name = "Increment",
            Parameters = new object[] { }
        };
        Scheduler.Enqueue(methodInvocation);
    }

    which enqueues it on the Activation Queue

        public class Scheduler
        {
            ConcurrentQueue<MethodInvocation> Activations = new ConcurrentQueue<MethodInvocation>();

            public void Enqueue(MethodInvocation inv)
            {
                Activations.Enqueue(inv);
            }
        }

    If the method is defined as a two-way [6], a binding to a Future is returned to
    the client that invoked the method. No Future is returned if a
    method is defined as a oneway, i.e., it has no return values.

        public interface IIncrementer
        {
            void Increment();
        }

    In this phase, the Scheduler runs continuously in a different thread than its clients

        public Scheduler()
        {
            Task.Factory.StartNew(() =>
            {
            });
        }

    Within this thread, the Scheduler monitors the Activation Queue and determines
    which Method Request(s) have become runnable,
    e.g., when their synchronization constraints are met

        public Scheduler()
        {
            Task.Factory.StartNew(() =>
            {
                while (true)
                {
                    var invocation = Activations.Take();

                    if(invocation.Guard())
                    {
                        //TODO Call the servant
                    }
                    else
                    {
                        //TODO Do something with this invocation
                    }
                }
            });
        }

    When a Method Request becomes runnable, the Scheduler dequeues
    it, binds it to the Servant, and dispatches the appropriate
    method on the Servant. When this method is called, it can
    access/update the state of its Servant and create its result(s).

        if (invocation.Guard())
        {
            var instance = invocation.Instance;
            var type = instance.GetType();
            var methodName = invocation.Name;
            var parameters = invocation.Parameters;

            var result = type.InvokeMember(methodName,
                    BindingFlags.Public |
                    BindingFlags.Instance |
                    BindingFlags.InvokeMethod,
                    null,
                    instance,
                    parameters);
        }

With this, our test

    for (int i = 0; i < 10; ++i)
    {
        Incrementer incrementer = new Incrementer();

        var scheduler = new Scheduler();
        var proxy = new ActiveObjectProxyIncrementer(scheduler, incrementer);

        var result = Parallel.For(0, 10000, new ParallelOptions()
        {
            MaxDegreeOfParallelism = 4
        }, x =>
        {
            proxy.Increment();
        });

        Thread.Sleep(1000);

        Console.WriteLine(incrementer.Value);
    } 

Already gives me the result 

10000
10000
10000
10000
10000
10000
10000
10000
10000
10000

Off course this works.
The scheduler call the servant using just a single thread.

    In the final phase, the results, if any, are
    stored in the Future and the Scheduler continues to monitor
    the Activation Queue for runnable Method Requests. After
    a two-way method completes, clients can retrieve its results
    by rendezvousing with the Future. In general, any clients
    that rendezvous with the Future can obtain its results. The
    Method Request and Future are deleted or garbage collected
    when they are no longer referenced.

Just for the sake of simplification, whe can implement this mechanism
and eliminate the Thread.Sleep with the folowwing code:

    for (int i = 0; i < 10; ++i)
    {
        Incrementer incrementer = new Incrementer();

        var scheduler = new Scheduler();
        var proxy = new ActiveObjectProxyIncrementer(scheduler, incrementer);

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

    ...

    public class Scheduler
    {
        public BlockingCollection<MethodInvocation> Activations = new BlockingCollection<MethodInvocation>();

        TaskCompletionSource<bool> TCS;

        public Task Completion { get { return TCS.Task; } }

        public Scheduler()
        {
            TCS = new TaskCompletionSource<bool>();

            Task.Factory.StartNew(() =>
            {
                while (true)
                {
                    MethodInvocation invocation = null;
                    var takeSucceed = Activations.TryTake(out invocation);

                    if (takeSucceed)
                    {
                        if (invocation.Guard())
                        {
                            ...
                        }
                        else
                        {
                            //TODO Do something with this invocation
                        }
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

        ...
    }

## Turning the code more C#-ish and .NET-ish

One possible improvement is change the invocation to a more-lambda-closure approach
and let the compiler generate all invocations DTO for you. This is also faster.

    public class MethodInvocation
    {
        public Action Invoke { get; set; }
        public Func<bool> Guard { get; set; }

        public MethodInvocation()
        {
            Guard = () => true;
        }
    }

    ...

    //In the Scheduler
    if (invocation.Guard())
    {
        invocation.Invoke();
    }

If we accept to ignore the Guard, we can forget the MethodInvocation
and use a Task instead.

    public class ActiveObjectProxyIncrementer : IIncrementer
    {
        Scheduler Scheduler;
        IIncrementer Servant;

        public ActiveObjectProxyIncrementer(Scheduler scheduler, IIncrementer servant)
        {
            Scheduler = scheduler;
            Servant = servant;
        }

        public void Increment()
        {
            var task = new Task(() =>
            {
                Servant.Increment();
            });
            
            Scheduler.Enqueue(task);
        }
    }

    ...
    //In the scheduler
    if (takeSucceed)
    {    
        invocation.RunSynchronously();
    }

In the end, that is why the TaskScheduler exist.

    public class Scheduler : TaskScheduler
    {
        public BlockingCollection<Task> Activations = new BlockingCollection<Task>();

        TaskCompletionSource<bool> TCS;

        public Task Completion { get { return TCS.Task; } }

        public Scheduler()
        {
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

But the C# already have a feature to create Tasks for methods. It is the async/await pattern.
So transforming the incrementer seems the perfect solution.

    //Servant
    public async Task IncrementAsync()
    {
        Value++;
    }

    //Proxy
    public Task Increment()
    {
        return Servant.IncrementAsync();
    }

The first problem here is off course that in an async method, every method
until the first await run synchronously on the caller thread. This breaks
one of the tenants of the Active Pattern.
One solution is:

    public async Task IncrementAsync()
    {
        await Task.Yield();
        Value++;
    }

The other problem now is that by the mechanism of the async/await the "Value++" is no
longer running on my Scheduler. This brings the problem back.

The solution is to create a SyncronizationContext that will capture every continuation
and schedule it back to our Scheduler.

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

But for this be usefull, we need to change the SynchronizationContext on the proxy.

    //Scheduler
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
    //Proxy
    public void Increment()
    {
        Scheduler.Capture(() =>
            {
                Servant.IncrementAsync();
            });
    }

With this we guarantee again that the code works.

Althought it is working now, there are several problems:
1 - We transformed the system to a single thread system;
2 - We created an uneeded interface.
3 - We created the proxy by hand.
4 - The Passive Object needed the "await Task.Yield" that is just magic.

Let us try to improve this.

First, I do not want to change my object model. I want to have just a single 
object: no extra interfaces, nor proxies;

So our Incremnter becomes:

    public class Incrementer
    {
        public int Value;

        public void Increment()
        {
            Value++;
        }        
    }

Off course the first step can be to create a generic Proxy.
Here we have two options again:
1 - Create a dynamic proxy object, like Mock frameworks do;
2 - Use the dynamic keyword.

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

So now I have a proxy that will work with any Passive Object.
Althought I have lost all my type safety.


# Model Counter With TLA+

---------------------- MODULE quickfind ----------------------
EXTENDS Naturals, TLC, Sequences

CONSTANT N

(* --algorithm SharedCounter {

variables
    SharedCounter = 0;
    
process (Thread \in 1..N)
variables privateCounter;
{
    step1: privateCounter := SharedCounter;
    step2: privateCounter := privateCounter + 1;
    step3: SharedCounter := privateCounter;
}

}*)

SharedCounterOK == (\A i \in 1..N: pc[i] = "Done") => SharedCounter = N
===================================================================

# TLC Trace

1: <Initial predicate>
/\ SharedCounter = 0
/\ privateCounter = <<defaultInitValue, defaultInitValue>>
/\ pc = <<"step1", "step1">>

2: <Action line 34, col 16 to line 37, col 41 of module quickfind>
/\ SharedCounter = 0
/\ privateCounter = <<defaultInitValue, 0>>
/\ pc = <<"step1", "step2">>

3: <Action line 39, col 16 to line 42, col 41 of module quickfind>
/\ SharedCounter = 0
/\ privateCounter = <<defaultInitValue, 1>>
/\ pc = <<"step1", "step3">>

4: <Action line 44, col 16 to line 47, col 42 of module quickfind>
/\ SharedCounter = 1
/\ privateCounter = <<defaultInitValue, 1>>
/\ pc = <<"step1", "Done">>

5: <Action line 34, col 16 to line 37, col 41 of module quickfind>
/\ SharedCounter = 1
/\ privateCounter = <<1, 1>>
/\ pc = <<"step2", "Done">>

6: <Action line 39, col 16 to line 42, col 41 of module quickfind>
/\ SharedCounter = 1
/\ privateCounter = <<2, 1>>
/\ pc = <<"step3", "Done">>

ERROR!
