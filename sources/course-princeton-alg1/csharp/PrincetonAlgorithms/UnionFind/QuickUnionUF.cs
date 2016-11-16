namespace PrincetonAlgorithms.UnionFind
{
    public class QuickUnionUF : IUnionFind
    {
        private int[] id;

        public QuickUnionUF(int N)
        {
            id = new int[N];
            for (int i = 0; i < N; i++)

            { id[i] = i; }
        }

        private int root(int i)
        {
            while (i != id[i])
            {
                i = id[i];
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
            id[i] = j;
        }
    }

}
