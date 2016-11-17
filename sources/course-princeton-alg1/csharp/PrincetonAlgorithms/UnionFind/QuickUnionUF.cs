using PrincetonAlgorithms.Memory;

namespace PrincetonAlgorithms.UnionFind
{
    public class QuickUnionUF : IUnionFind
    {
        IRandomAccess<int> Id;

        public int Size { get { return (int)Id.Size; } }

        public QuickUnionUF(IRandomAccess<int> id)
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
                i = Id.Read(i);
            }

            return i;
        }

        public bool Connected(int p, int q)
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
