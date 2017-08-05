namespace Algorithms.UnionFind.Memory
{
    public interface IRandomAccess<T> : IRandomReader<T>, IRandomWriter<T>, IFiniteMemory
    {
    }
}
