using System;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using static PaxosNode.Program;

namespace PaxosNode
{
    class Program
    {
        static async Task Main(string[] args)
        {
            OpenBroadCastListener(int.Parse(args[0]));
            await PeriodicallyBroadcastSelf();

            var network = new Network();
            await StartPaxosProposer(network);
            while(true)
            {
                var command = Console.ReadLine();
                var tokens = command.Split();
                switch(tokens[0].ToLower())
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


        static async Task PeriodicallyBroadcastSelf()
        {
            var Client = new UdpClient()
            {
                EnableBroadcast = true
            };

            var data = Encoding.ASCII.GetBytes("SomeRequestData");
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
                var message = Encoding.ASCII.GetString(result.Buffer);
                var key = $"{result.RemoteEndPoint.Address}:{result.RemoteEndPoint.Port}";
                if (!Nodes.ContainsKey(key))
                {
                    Nodes.TryAdd(key, result.RemoteEndPoint);
                    System.Console.WriteLine($"New node {key}");
                }
            }
        }

        static async Task<bool> StartPaxosProposer(Network network)
        {
            int round = 0;

            while (true)
            {
                var propose = await network.Wait<PaxosPropose>();

                await network.Broadcast("acceptors", new PaxosPrepare { Round = round });
                var result = await network.AsObservable()
                    .Quorum(Nodes.Count, x => x is PaxosPromise);

                if (result.Achieved == false) return false;

                await network.Broadcast("acceptors", new PaxosAccept { Round = round, Value = propose.Value });

                result = await network.AsObservable()
                    .Quorum(Nodes.Count, x => x is PaxosAccepted);

                return true;
            }
        }

        static async Task StartPaxosAcceptor(Network network)
        {
            int PromisedRound = -1;
            while (true)
            {
                var message = await network.Listen();
                switch (message.Content)
                {
                    case PaxosPrepare e:
                        {
                            if (PromisedRound > e.Round)
                                message.Answer(new PaxosNack { Round = e.Round });
                            else
                                message.Answer(new PaxosPromise { Round = e.Round });
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
        public class PaxosPromise { public int Round; }
        public class PaxosAccept { public int Round; public int Value; }
        public class PaxosAccepted { public int Round; public int Value; }
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
                if (AcceptPredicates.Any(x => x(value))) Interlocked.Increment(ref Accepteds);
                else Interlocked.Increment(ref Rejecteds);

                if (Accepteds >= Needed) TaskCompletionSource.TrySetResult(new QuorumResult() { Achieved = true });
                if (Rejecteds >= Needed) TaskCompletionSource.TrySetResult(new QuorumResult() { Achieved = false });
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

    public class QuorumResult
    {
        public bool Achieved { get; set; }
    }

    public class Network
    {
        public Task<Message> Listen()
        {

        }

        public async Task<T> Wait<T>()
        {
            while(true)
            {
                var msg = await Listen();
                if(msg.Content is T x)
                {
                    return x;
                }
            }
        }

        public Task Broadcast(string roles, object content)
        {

        }

        public IObservable<object> AsObservable()
        {

        }
    }
}
