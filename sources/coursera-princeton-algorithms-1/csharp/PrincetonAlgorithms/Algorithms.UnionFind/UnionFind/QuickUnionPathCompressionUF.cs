using Algorithms.UnionFind.Memory;

namespace Algorithms.UnionFind.UnionFind
{
    public class QuickUnionPathCompressionUF : IUnionFind
    {
        IRandomAccess<int> Id;

        public int Size { get { return (int)Id.Size; } }

        public QuickUnionPathCompressionUF(IRandomAccess<int> id)
        {
            Id = id;

            for (int i = 0; i < Id.Size; i++)
            {
                Id.Write(i, i);
            }
        }

        private int root(int i)
        {
            while (i != Id.Read(i))
            {
                var grandparent = Id.Read(Id.Read(i));
                Id.Write(i, grandparent);
                i = Id.Read(i);
            }

            return i;
        }

        public bool IsConnected(int p, int q)
        {
            return root(p) == root(q);
        }

        public void Union(int p, int q)
        {
            int i = root(p);
            int j = root(q);
            Id.Write(i, j);
        }
    }

}
