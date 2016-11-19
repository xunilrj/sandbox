namespace Algorithms.UnionFind.Memory
{
    public interface IMemoryReport
    {
        long Reads { get; }
        long Writes { get; }

        void Reset();
    }
}
