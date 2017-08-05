namespace Algorithms.UnionFind.Memory
{
    public interface IRandomWriter<T>
    {
        void Write(int index, T item);
    }
}