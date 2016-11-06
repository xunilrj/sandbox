using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Diagnostics.Contracts;

namespace PrincetonAlgorithms
{
    [TestClass]
    public class QuickFindUFTests
    {
        [TestMethod]
        public void SomeInitialTests()
        {
            var uf = new QuickFindUF(10);

            Assert.IsTrue(uf.Connected(0, 0));

            uf.Union(2, 8);

            Assert.IsTrue(uf.Connected(2, 8));

            uf.Union(1, 8);

            Assert.IsTrue(uf.Connected(1, 2));
        }
    }

    public class QuickFindUF
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
