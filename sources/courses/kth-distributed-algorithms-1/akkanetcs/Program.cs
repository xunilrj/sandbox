using System;
using Akka;
using Akka.Actor;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Threading;
using System.Linq;
using System.Collections;

namespace akkanetcs
{
    class Program
    {
        static void Main(string[] args)
        {
            var t = Task.Factory.StartNew(async () =>
            {
                var actorSystem = ActorSystem.Create("MySystem");
                var a1 = actorSystem.ActorOf<BankAccount>("account1");
                var a2 = actorSystem.ActorOf<BankAccount>("account2");

                a1.Tell(new TransferMoneyRequest()
                {
                    Ammount = 100,
                    From = a1,
                    To = a2
                });

                await actorSystem.WhenTerminated;
            }).Unwrap();

            t.Wait();


            var p1 = new ProcessInput();
            var p2 = new ProcessInput();

            Stubborn(p1, p2.Capture<DeliveredEvent>());
            FairLink(p2, x => { });
                
        }

        public static async Task FairLink(ProcessInput inputs, Action<DeliveredEvent> raiseDeliveredEvent)
        {
            Random random = new Random();
            double failure = 0.5;

            inputs.When<SendRequest>(env =>
                {
                    var rv = random.NextDouble();
                    if (rv > failure)
                    {
                        env.Target.Tell(env);
                        Console.WriteLine("FairLinkActor ok!");
                    }
                    else
                    {
                        Console.WriteLine("FairLinkActor failed!");
                    }
                })
                .When<DeliveredEvent>(e =>
                {
                    Console.WriteLine("FairLinkActor delivered.");
                    raiseDeliveredEvent(e);
                });
            await inputs.Finished;
        }

        public static async Task Stubborn(ProcessInput inputs, Action<DeliveredEvent> raiseDelivered)
        {
            var buffer = new Dictionary<Guid, SendRequest>();

            inputs.When<SendRequest>(request =>
            {
                buffer.Add(request.Id, request);
                //Next.Tell(env);
            })
            .When<DeliveredEvent>(e =>
            {
                Console.WriteLine("StubbornActor delivered.");
                buffer.Remove(e.Id);
                raiseDelivered(e);
            }).When(() => buffer.Count > 0, () =>
            {
                Console.WriteLine("StubbornActor buffer not empty. Sending again...");
                foreach (var item in buffer)
                {
                    //Next.Tell(item.Value);
                }
                Console.WriteLine("StubbornActor buffer not empty. Done.");
            });
            await inputs.Finished;
        }

        public static async Task AsyncJobHandler(ProcessInput inputs,
                    Action<AcceptedEvent> raiseAcceptedEvent,
                    Action<OkEvent> raiseOk)
        {
            Queue<SubmitJobRequest> jobs = new Queue<SubmitJobRequest>();

            inputs.When<SubmitJobRequest>(command =>
                {
                    jobs.Enqueue(command);
                    raiseAcceptedEvent(new AcceptedEvent()
                    {
                        Id = command.Id
                    });
                })
                .When<OkEvent>(raiseOk)
                .When(() => jobs.Count != 0, () =>
                {
                });
            await inputs.Finished;
        }
    }

    public class ProcessInput : IEnumerable<Task<object>>
    {
        public ProcessInput When(Func<bool> f, Action run)
        {
            return this;
        }

        public ProcessInput When<T>(Action<T> run)
        {
            return this;
        }

        public IEnumerator<Task<object>> GetEnumerator()
        {
            throw new NotImplementedException();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            throw new NotImplementedException();
        }

        public Action<T> Capture<T>()
        {
            return new Action<T>(x => { });
        }

        public Task Finished { get; }
    }

    public class BankAccountState
    {
        public bool? OperationPending { get; set; }
    }

    public class TransferMoneyRequest
    {
        public double Ammount { get; set; }

        public IActorRef From { get; set; }
        public IActorRef To { get; set; }

        public ReceiveMoneyRequest CreateReceive()
        {
            return new ReceiveMoneyRequest()
            {
                From = From,
                Ammount = Ammount
            };
        }
    }

    public class ReceiveMoneyRequest
    {
        public double Ammount { get; set; }

        public IActorRef From { get; set; }
    }

    public class BankAccount : UntypedActor
    {
        BankAccountState State { get; }

        public BankAccount()
        {
            State = new BankAccountState();
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case TransferMoneyRequest request:
                    State.OperationPending = true;
                    Context.Deliver(Self, request.To, request.CreateReceive());
                    break;
                //This actor should not know messages at this level
                case SendRequest request:
                    Sender.Tell(new DeliveredEvent()
                    {
                        Id = request.Id
                    });
                    break;
                case int x:
                    Console.WriteLine(x);
                    break;
            }
        }
    }

    public static class ContextExtensions
    {
        public static void Deliver(this IUntypedActorContext context, IActorRef self, IActorRef target, object message)
        {
            var m = new SendRequest()
            {
                Message = message,

                Source = self,
                Target = target
            };

            //
            // Stubborn -> PerfectLink -> FairLink
            //
            var link = Props.Create<FairLinkActor>();
            var perfectLink = Props.Create<PerfectLink>(link);
            var sender = context.ActorOf(Props.Create<StubbornActor>(perfectLink));
            sender.Tell(m);
        }
    }


    public class SendRequest
    {
        public Guid Id { get; }

        public object Message { get; set; }

        public IActorRef Source { get; set; }
        public IActorRef Target { get; set; }

        public SendRequest()
        {
            Id = Guid.NewGuid();
        }
    }

    public class DeliveredEvent
    {
        public Guid Id { get; set; }
    }

    public class TimeoutMessage
    {

    }

    public class PerfectLink : UntypedActor
    {
        HashSet<Guid> AlreadySent = new HashSet<Guid>();
        private IActorRef Next;

        public PerfectLink(Props next)
        {
            Next = Context.ActorOf(next);
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case SendRequest env:
                    if (AlreadySent.Contains(env.Id) == false)
                    {
                        Next.Tell(env);
                    }
                    break;
                case DeliveredEvent ack:
                    Console.WriteLine("PerfectLink delivered.");
                    AlreadySent.Add(ack.Id);
                    Context.Parent.Tell(ack);
                    break;
            }
        }
    }

    public class StubbornActor : UntypedActor
    {
        Dictionary<Guid, SendRequest> Buffer = new Dictionary<Guid, SendRequest>();
        IActorRef Next;
        ICancelable Cancel;

        public StubbornActor(Props next)
        {
            Next = Context.ActorOf(next);
            Cancel = new Cancelable(Context.System.Scheduler);
            Context.System.Scheduler.ScheduleTellRepeatedly(
                TimeSpan.FromSeconds(3),
                TimeSpan.FromSeconds(3),
                Self,
                new TimeoutMessage(),
                ActorRefs.NoSender,
                Cancel);
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case TimeoutMessage m:
                    CancelIfEmpty();
                    if (Buffer.Count > 0)
                    {
                        Console.WriteLine("StubbornActor buffer not empty. Sending again...");
                        foreach (var item in Buffer)
                        {
                            Next.Tell(item.Value);
                        }
                        Console.WriteLine("StubbornActor buffer not empty. Done.");
                    }
                    break;
                case SendRequest env:
                    Buffer.Add(env.Id, env);
                    Next.Tell(env);
                    break;
                case DeliveredEvent ack:
                    Console.WriteLine("StubbornActor delivered.");
                    Buffer.Remove(ack.Id);
                    Context.Parent.Tell(ack);
                    CancelIfEmpty();
                    break;
            }
        }

        void CancelIfEmpty()
        {
            if (Buffer.Count == 0)
            {
                Console.WriteLine("StubbornActor buffer empty. Stopping...");
                Cancel.Cancel();
                Context.Stop(Self);
            }
        }
    }

    public class FairLinkActor : UntypedActor
    {
        Random Random;
        double Failure;

        public FairLinkActor()
        {
            Random = new Random();
            Failure = 0.5;
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case SendRequest env:
                    var rv = Random.NextDouble();
                    if (rv > Failure)
                    {
                        env.Target.Tell(env);
                        Console.WriteLine("FairLinkActor ok!");
                    }
                    else
                    {
                        Console.WriteLine("FairLinkActor failed!");
                    }
                    break;
                case DeliveredEvent e:
                    Console.WriteLine("FairLinkActor delivered.");
                    Context.Parent.Tell(e);
                    break;
            }
        }
    }

    public class SubmitJobRequest
    {
        public Guid Id { get; set; }
        public string Command { get; set; }
        public string Args { get; set; }

        public SubmitJobRequest()
        {
            Id = Guid.NewGuid();
        }
    }

    public class AcceptedEvent
    {
        public Guid Id { get; set; }
    }

    public class OkEvent
    {
        public Guid Id { get; set; }
    }

    public class SynchronousJobHandler : UntypedActor
    {
        bool AutoKill;

        public SynchronousJobHandler(bool autoKill)
        {
            AutoKill = autoKill;
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case SubmitJobRequest command:
                    Context.Parent.Tell(new AcceptedEvent()
                    {
                        Id = command.Id
                    });

                    //RUN
                    //If the command run throws?
                    Thread.Sleep(5000);

                    Context.Parent.Tell(new OkEvent()
                    {
                        Id = command.Id
                    });

                    if (AutoKill) Context.Stop(Self);
                    break;
            }
        }
    }

    public class AsynchronousJobHandler : UntypedActor
    {
        Queue<SubmitJobRequest> Jobs = new Queue<SubmitJobRequest>();
        ICancelable Cancel;

        public AsynchronousJobHandler()
        {
            Cancel = new Cancelable(Context.System.Scheduler);
            Context.System.Scheduler.ScheduleTellRepeatedly(
                TimeSpan.FromSeconds(3),
                TimeSpan.FromSeconds(3),
                Self,
                new TimeoutMessage(),
                ActorRefs.NoSender,
                Cancel);
        }

        protected override void OnReceive(object message)
        {
            switch (message)
            {
                case TimeoutMessage t:
                    // Only one Job at a time
                    //need a better solution
                    if (Jobs.Count != 0 && Context.GetChildren().Count() == 0)
                    {
                        var job = Jobs.Dequeue();
                        var handler = Context.ActorOf(Props.Create<SynchronousJobHandler>(true));
                        handler.Tell(job);
                    }
                    break;
                case SubmitJobRequest command:
                    Jobs.Enqueue(command);
                    Context.Parent.Tell(new AcceptedEvent()
                    {
                        Id = command.Id
                    });
                    Self.Tell(new TimeoutMessage());
                    break;
                case OkEvent ok:
                    Context.Parent.Tell(ok);
                    break;
            }
        }


    }
}
