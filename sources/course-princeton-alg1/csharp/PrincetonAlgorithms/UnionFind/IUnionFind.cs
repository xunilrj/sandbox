namespace PrincetonAlgorithms.UnionFind
{
    public interface IUnionFind
    {
        int Size { get; }

        bool Connected(int p, int q);
        void Union(int p, int q);
    }
}
