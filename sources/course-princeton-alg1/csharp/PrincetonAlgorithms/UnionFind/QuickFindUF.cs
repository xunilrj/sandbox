using PrincetonAlgorithms.Memory;

namespace PrincetonAlgorithms.UnionFind
{
    public class QuickFindUF : IUnionFind
    {
        private readonly IRandomAccess<int> Id;

        public int Size { get { return (int)Id.Size; } }

        public QuickFindUF(IRandomAccess<int> memory)
        {
            Id = memory;

            for (int i = 0; i < memory.Size; i++)
            {
                Id.Write(i, i);
            }
        }

        public bool Connected(int p, int q)
        {
            return Id.Read(p) == Id.Read(q);
        }

        public void Union(int p, int q)
        {
            int pid = Id.Read(p);
            int qid = Id.Read(q);

            for (int i = 0; i < Id.Size; i++)
            {
                if (Id.Read(i) == pid)
                {
                    Id.Write(i, qid);
                }
            }
        }
    }
}
