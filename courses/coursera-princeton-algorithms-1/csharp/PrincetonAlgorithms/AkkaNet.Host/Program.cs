using Akka.Actor;
using Akka.Configuration;
using AkkaNet.Algorithms.UnionFind;
using AkkaNet.Algorithms.UnionFind.Messages;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AkkaNet.Host
{
    class Program
    {
        static void Main(string[] args)
        {
            var config = ConfigurationFactory.ParseString(@"akka {
    persistence {
        snapshot-store {
            plugin = ""akka.persistence.snapshot-store.local""
            local {
                class = ""Akka.Persistence.Snapshot.LocalSnapshotStore, Akka.Persistence""
                plugin-dispatcher = ""akka.persistence.dispatchers.default-plugin-dispatcher""
                stream-dispatcher = ""akka.persistence.dispatchers.default-stream-dispatcher""
                dir = ""snapshots""
            }
        }
    }
}");
            var system = ActorSystem.Create("Algorithms", config);

            //var snap10 = system.ActorSelection("/user/snap10/1");
            //var result = snap10.Ask<bool>(new IsConnectedQuery(8, 2)).Result;
            //Console.WriteLine($"Connected:{result}");

            var uf = SnapshotUnionFindActor.Create(10);
            var snap10 = system.ActorOf(uf, "snap10");

            Console.WriteLine("Validating Initial State...");
            AssertAll(OnlyEqualPair(10), x => snap10.Ask<bool>(new IsConnectedQuery(x.Item1, x.Item2)).Result);
            AssertAll(NonEqualPair(10), x => snap10.Ask<bool>(new IsConnectedQuery(x.Item1, x.Item2)).Result == false);
            Console.WriteLine("OK!");

            //What Akka gives me here about race condition?
            snap10.Tell(new UnionCommand(2, 8));
            Debug.Assert(snap10.Ask<bool>(new IsConnectedQuery(2, 8)).Result);
            Debug.Assert(snap10.Ask<bool>(new IsConnectedQuery(8, 2)).Result);

            Console.ReadLine();
        }

        static IEnumerable<Tuple<int, int>> OnlyEqualPair(int N)
        {
            return from i in Enumerable.Range(0, N)
                   from j in Enumerable.Range(0, N)
                   where i == j
                   select Tuple.Create(i, j);
        }

        static void AssertAll<T>(IEnumerable<T> items, Func<T, bool> predicate)
        {
            var result = items.All(predicate);
            Debug.Assert(result == true);
        }

        static IEnumerable<Tuple<int, int>> NonEqualPair(int N)
        {
            return from i in Enumerable.Range(0, N)
                   from j in Enumerable.Range(0, N)
                   where i != j
                   select Tuple.Create(i, j);
        }
    }
}
