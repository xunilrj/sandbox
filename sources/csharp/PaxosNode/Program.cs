using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using static PaxosNode.Block;
using static PaxosNode.Program;

namespace PaxosNode
{
    class Program
    {
        static async Task Main(string[] args)
        {
            OpenBroadCastListener(int.Parse(args[0]));
            await PeriodicallyBroadcastSelf();

            var bus = new Bus();
            await StartPaxosProposer(bus);
            await StartPaxosLearner(bus);
            while (true)
            {
                Console.Write(">");
                var command = Console.ReadLine();
                var tokens = command.Split();
                switch (tokens[0].ToLower())
                {
                    case "propose":
                        {
                            break;
                        }
                    default:
                        {
                            Console.WriteLine("Unkown command!");
                            break;
                        }
                }
            }
        }

        static System.Collections.Concurrent.ConcurrentDictionary<string, IPEndPoint> Nodes =
            new System.Collections.Concurrent.ConcurrentDictionary<string, IPEndPoint>();

        public static class Messages
        {
            public static string AsBase64(object instance)
            {
                var str = JsonConvert.SerializeObject(instance);
                var array = System.Text.Encoding.UTF8.GetBytes(str);
                return Convert.ToBase64String(array);
            }
            public static string ControlAnnounce => "0:0";
            public static string BusBroadcast(object instance) => $"1:{instance.GetType().FullName}:{AsBase64(instance)}";

            public class UdpMessage
            {
                public int Topic { get; set; }
                public byte[] Content { get; set; }

                UdpMessage Parse(byte[] buffer)
                {
                    var message = Encoding.ASCII.GetString(buffer);
                    var tokens = message.Split(":");
                    return new UdpMessage()
                    {
                        Topic = int.Parse(tokens[0]),
                        MessageType = int.Parse(tokens)
                    };
                }
            }
        }

        static async Task PeriodicallyBroadcastSelf()
        {
            var Client = new UdpClient()
            {
                EnableBroadcast = true
            };

            var data = Encoding.ASCII.GetBytes(Messages.ControlAnnounce);
            var ip = new IPEndPoint(IPAddress.Any, 0);

            while (true)
            {
                await Client.SendAsync(data, data.Length, new IPEndPoint(IPAddress.Broadcast, 8888));
                await Client.SendAsync(data, data.Length, new IPEndPoint(IPAddress.Broadcast, 8889));
                await Client.SendAsync(data, data.Length, new IPEndPoint(IPAddress.Broadcast, 8890));
                await Client.SendAsync(data, data.Length, new IPEndPoint(IPAddress.Broadcast, 8891));
                await Client.SendAsync(data, data.Length, new IPEndPoint(IPAddress.Broadcast, 8892));
                await Task.Delay(1000);
            }
        }

        static async Task OpenBroadCastListener(int port)
        {
            var Server = new UdpClient(port);
            var ip = new IPEndPoint(IPAddress.Any, 0);
            while (true)
            {
                var result = await Server.ReceiveAsync();

                message.Split(":")
                var key = $"{result.RemoteEndPoint.Address}:{result.RemoteEndPoint.Port}";
                if (!Nodes.ContainsKey(key))
                {
                    Nodes.TryAdd(key, result.RemoteEndPoint);
                    System.Console.WriteLine($"New node {key}");
                }
            }
        }

        static async Task<bool> StartPaxosProposer(Bus bus)
        {
            int round = 0;
            var acceptors = bus.GetTopic("acceptors");
            var learners = bus.GetTopic("learners");

            while (true)
            {
                var proposeDialog = await bus.Wait<PaxosPropose>();
                var propose = proposeDialog.Message.Content as PaxosPropose;
                round = await Block.Run<int>(async control =>
                {
                    await acceptors.Broadcast(new PaxosPrepare { Round = round });
                    var prepareQuorum = await bus.AsObservable()
                        .Quorum(Nodes.Count, x => x is PaxosPrepared);

                    if (prepareQuorum.Achieved == false)
                    {
                        round = prepareQuorum.Rejecteds
                            .OfType<PaxosNack>()
                            .Max(x => x.Round);
                        return control.Retry();
                    }
                    else return round;
                });

                await acceptors.Broadcast(new PaxosAccept
                {
                    Round = round
                });

                var result = await bus.AsObservable()
                    .Quorum(Nodes.Count, x => x is PaxosAccepted);
                if (result.Achieved)
                    await learners.Broadcast(new PaxosDecided { Round = round, Value = propose.Value });

                return true;
            }
        }

        static async Task StartPaxosAcceptor(Bus network)
        {
            int PromisedRound = -1;
            while (true)
            {
                var dialog = await network.WaitDialog();
                var message = dialog.Message;
                switch (message.Content)
                {
                    case PaxosPrepare e:
                        {
                            if (PromisedRound > e.Round)
                                dialog.Answer(new PaxosNack { Round = PromisedRound + 1 });
                            else
                                dialog.Answer(new PaxosPrepared { Round = e.Round });
                            break;
                        }
                    case PaxosAccept e:
                        {
                            if (PromisedRound <= e.Round)
                                dialog.Answer(new PaxosAccepted { Round = e.Round });
                            else
                                dialog.Answer(new PaxosNack { Round = e.Round });
                            break;
                        }
                    default:
                        {
                            Console.WriteLine("ERROR");
                            break;
                        }
                }
            }
        }

        public static async Task StartPaxosLearner(Bus bus)
        {
            int round = -1;
            int? value = null;
            while (true)
            {
                var dialog = await bus.Wait<PaxosDecided>();
                var message = dialog.Message.Content;
                switch (message)
                {
                    case PaxosDecided e:
                        {
                            if (e.Round > round)
                            {
                                value = e.Value;
                            }
                            break;
                        }
                    case PaxosQuery e:
                        {
                            dialog.Answer(new PaxosQueryResponse { Value = value });
                            break;
                        }
                }
            }
        }

        public class Dialog
        {
            public Message Message { get; }
            public void Answer(object content)
            {

            }

            public Task<T> Wait<T>()
            {

            }
        }

        public class Message
        {
            public object Content { get; set; }

            public void Answer(object e)
            {
            }
        }

        public class PaxosPropose { public int Value; }
        public class PaxosNack { public int Round; }
        public class PaxosPrepare { public int Round; }
        public class PaxosPrepared { public int Round; }
        public class PaxosAccept { public int Round; }
        public class PaxosAccepted { public int Round; }
        public class PaxosDecided { public int Round; public int Value; }
        public class PaxosQuery { }
        public class PaxosQueryResponse { public int? Value; }
    }

    public static class ObservableExtensions
    {
        public class QuorumObserver<T> : IObserver<T>
        {
            private Func<T, bool>[] AcceptPredicates;
            int Accepteds;
            int Rejecteds;
            int Needed;
            TaskCompletionSource<QuorumResult> TaskCompletionSource;
            List<object> Accepted = new List<object>();
            List<object> Rejected = new List<object>();

            public Task<QuorumResult> Result => TaskCompletionSource.Task;

            public QuorumObserver(int N, params Func<T, bool>[] accept)
            {
                AcceptPredicates = accept;
                Needed = (N / 2) + 1;
                Accepteds = 0;
                Rejecteds = 0;
                TaskCompletionSource = new TaskCompletionSource<QuorumResult>();
            }

            public void OnCompleted()
            {
            }

            public void OnError(Exception error)
            {
            }

            public void OnNext(T value)
            {
                if (AcceptPredicates.Any(x => x(value)))
                {
                    Interlocked.Increment(ref Accepteds);
                    Accepted.Add(value);
                }
                else
                {
                    Interlocked.Increment(ref Rejecteds);
                    Rejected.Add(value);
                }

                if (Accepteds >= Needed) TaskCompletionSource.TrySetResult(new QuorumResult()
                {
                    Achieved = true,
                    Accepteds = Accepted,
                    Rejecteds = Rejected
                });
                if (Rejecteds >= Needed) TaskCompletionSource.TrySetResult(new QuorumResult()
                {
                    Achieved = false,
                    Accepteds = Accepted,
                    Rejecteds = Rejected
                });
            }
        }

        public static async Task<QuorumResult> Quorum<T>(this IObservable<T> obs, int N, params Func<T, bool>[] accept)
        {
            var observer = new QuorumObserver<T>(N, accept);
            var disposable = obs.Subscribe(observer);

            var result = await observer.Result;
            disposable.Dispose();

            return result;
        }
    }

    public static class Block
    {
        public static async Task<TR> Run<TR>(Func<Control<TR>, Task<Continuation<TR>>> f)
        {
            bool run = true;
            while (run)
            {
                var control = new Control<TR>();
                var result = await f(control);
                if (result.IsRetry)
                {
                    run = true;
                    continue;
                }
                else if (result.IsReturn)
                {
                    return result.Value;
                }
            }
        }

        public class Control<TR>
        {
            public Continuation<TR> Return(TR value)
            {
                return value;
            }

            internal Continuation<TR> Retry()
            {
                throw new NotImplementedException();
            }
        }

        public class Continuation<TR>
        {
            public bool IsRetry { get; internal set; }
            public bool IsReturn { get; internal set; }
            public object Value { get; internal set; }

            public static implicit operator Continuation<TR>(TR c)
            {

            }
        }
    }

    public class QuorumResult
    {
        public bool Achieved { get; set; }
        public IEnumerable<object> Accepteds { get; set; }
        public IEnumerable<object> Rejecteds { get; set; }
    }

    public class Bus
    {
        public Task<Dialog> WaitDialog()
        {

        }

        public async Task<Dialog> Wait<T>()
        {
            while (true)
            {
                var msg = await Listen();
                if (msg.Content is T x)
                {
                    return x;
                }
            }
        }

        public Task Broadcast(object content)
        {

        }

        public IObservable<object> AsObservable()
        {

        }

        internal Bus GetTopic(string v)
        {
            throw new NotImplementedException();
        }
    }
}
