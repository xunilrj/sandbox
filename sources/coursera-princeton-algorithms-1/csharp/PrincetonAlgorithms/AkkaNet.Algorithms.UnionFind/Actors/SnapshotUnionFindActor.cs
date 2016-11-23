using Akka.Actor;
using Akka.Persistence;
using AkkaNet.Algorithms.UnionFind.Messages;
using Algorithms.UnionFind.Memory;
using Algorithms.UnionFind.UnionFind;
using System;

namespace AkkaNet.Algorithms.UnionFind
{
    public class SnapshotUnionFindActor : ReceivePersistentActor
    {
        public static Props Create(int size)
        {
            return Props.Create<SnapshotUnionFindActor>(size);
        }

        Guid Id;
        WeightedQuickUnionPathCompressionUF UF;

        public override string PersistenceId { get { return "1"; } }

        public SnapshotUnionFindActor(int size)
        {
            var memory = new MemoryReadWrite<int>(10);
            var sizes = new MemoryReadWrite<int>(10);
            UF = new WeightedQuickUnionPathCompressionUF(memory, sizes);

            Id = Guid.NewGuid();

            Recover<SnapshotOffer>(RecoverState);
            Command<UnionCommand>(HandleUnion);
            Command<IsConnectedQuery>(HandleIsConnected);
        }

        private bool RecoverState(SnapshotOffer offer)
        {
            UF = (WeightedQuickUnionPathCompressionUF)offer.Snapshot;

            return true;
        }

        private bool HandleIsConnected(IsConnectedQuery query)
        {
            var result = UF.IsConnected(query.Left, query.Right);
            Sender.Tell(result);

            return true;
        }

        private bool HandleUnion(UnionCommand command)
        {
            UF.Union(command.Left, command.Right);

            SaveSnapshot(UF);

            return true;
        }
    }
}
