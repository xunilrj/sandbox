using PrincetonAlgorithms.Memory;

namespace PrincetonAlgorithms.UnionFind
{
    public class WeightedQuickUnionPathCompressionUF : IUnionFind
    {
        IRandomAccess<int> Id;
        IRandomAccess<int> Sizes;

        public int Size { get { return (int)Id.Size; } }

        public WeightedQuickUnionPathCompressionUF(IRandomAccess<int> id, IRandomAccess<int> sizes)
        {
            Id = id;
            Sizes = sizes;

            for (int i = 0; i < Id.Size; i++)
            {
                Id.Write(i, i);
                Sizes.Write(i, 1);
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

        public bool Connected(int p, int q)
        {
            return root(p) == root(q);
        }

        public void Union(int p, int q)
        {
            int proot = root(p);
            int qroot = root(q);

            if (proot == qroot) return;

            if (Sizes.Read(proot) < Sizes.Read(qroot))
            {
                Id.Write(proot, qroot);
                Sizes.Write(qroot, Sizes.Read(qroot) + Sizes.Read(proot));
            }
            else
            {
                Id.Write(qroot, proot);
                Sizes.Write(proot, Sizes.Read(proot) + Sizes.Read(qroot));
            }
        }
    }
}
