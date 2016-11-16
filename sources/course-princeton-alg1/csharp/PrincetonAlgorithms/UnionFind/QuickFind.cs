namespace PrincetonAlgorithms.UnionFind
{
    public class QuickFindUF : IUnionFind
    {
        private readonly int[] id;

        public QuickFindUF(int N)
        {
            id = new int[N];

            for (int i = 0; i < N; i++)
            {
                id[i] = i;
            }
        }

        public bool Connected(int p, int q)
        {
            return id[p] == id[q];
        }

        public void Union(int p, int q)
        {
            int pid = id[p];
            int qid = id[q];

            for (int i = 0; i < id.Length; i++)
            {
                if (id[i] == pid)
                {
                    id[i] = qid;
                }
            }
        }
    }
}
