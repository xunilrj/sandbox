using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;
using PrincetonAlgorithms.UnionFind;

namespace PrincetonAlgorithms
{
    [TestClass]
    public class UFTests
    {
        [TestMethod]
        public void QuickFind()
        {
            var uf = new QuickFindUF(10);

            AssertUF(uf);
        }

        [TestMethod]
        public void QuickUnion()
        {
            var uf = new QuickUnionUF(10);

            AssertUF(uf);
        }

        private void AssertUF(IUnionFind uf)
        {
            AssertAll(OnlyEqualPair(10), x => uf.Connected(x.Item1, x.Item2));
            AssertAll(NonEqualPair(10), x => uf.Connected(x.Item1, x.Item2) == false);

            uf.Union(2, 8);

            Assert.IsTrue(uf.Connected(2, 8));
            Assert.IsTrue(uf.Connected(8, 2));

            uf.Union(1, 8);

            Assert.IsTrue(uf.Connected(1, 8));
            Assert.IsTrue(uf.Connected(8, 1));
            Assert.IsTrue(uf.Connected(1, 2));
            Assert.IsTrue(uf.Connected(2, 1));
            Assert.IsTrue(uf.Connected(2, 8));
            Assert.IsTrue(uf.Connected(8, 2));
        }

        void AssertAll<T>(IEnumerable<T> items, Func<T, bool> predicate)
        {
            var result = items.All(predicate);
            Assert.IsTrue(result);
        }

        IEnumerable<Tuple<int, int>> OnlyEqualPair(int N)
        {
            return from i in Enumerable.Range(0, N)
                   from j in Enumerable.Range(0, N)
                   where i == j
                   select Tuple.Create(i, j);
        }

        IEnumerable<Tuple<int, int>> NonEqualPair(int N)
        {
            return from i in Enumerable.Range(0, N)
                   from j in Enumerable.Range(0, N)
                   where i != j
                   select Tuple.Create(i, j);
        }
    }
}
