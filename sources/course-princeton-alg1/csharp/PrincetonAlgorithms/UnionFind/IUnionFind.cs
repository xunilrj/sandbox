namespace PrincetonAlgorithms.UnionFind
{
    public interface IUnionFind
    {
        bool Connected(int p, int q);
        void Union(int p, int q);
    }
}
