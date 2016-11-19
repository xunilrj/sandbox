namespace Algorithms.UnionFind.UnionFind
{
    public interface IUnionFind
    {
        int Size { get; }

        bool IsConnected(int p, int q);
        void Union(int p, int q);
    }
}
