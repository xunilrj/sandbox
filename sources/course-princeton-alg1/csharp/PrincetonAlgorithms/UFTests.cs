using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;
using PrincetonAlgorithms.UnionFind;
using PrincetonAlgorithms.Memory;

namespace PrincetonAlgorithms
{
    [TestClass]
    public class UFTests
    {
        [TestMethod]
        public void QuickFind()
        {
            Console.WriteLine("Size: 10");
            var memory = new MemoryReadWrite<int>(10);
            var uf = new QuickFindUF(memory);

            AssertUF(10, memory, uf);
        }

        [TestMethod]
        public void QuickUnion()
        {
            Console.WriteLine("Size: 10");
            var memory = new MemoryReadWrite<int>(10);
            var uf = new QuickUnionUF(memory);

            AssertUF(10, memory, uf);
        }

        [TestMethod]
        public void WeightedQuickUnion()
        {
            Console.WriteLine("Size: 10");
            var memory = new MemoryReadWrite<int>(10);
            var sizes = new MemoryReadWrite<int>(10);
            var uf = new WeightedQuickUnionUF(memory, sizes);

            AssertUF(10, memory, uf);
        }

        [TestMethod]
        public void QuickUnionPathCompression()
        {
            Console.WriteLine("Size: 10");
            var memory = new MemoryReadWrite<int>(10);
            var uf = new QuickUnionPathCompressionUF(memory);

            AssertUF(10, memory, uf);
        }

        [TestMethod]
        public void WeightedQuickUnionPathCompression()
        {
            Console.WriteLine("Size: 10");
            var memory = new MemoryReadWrite<int>(10);
            var sizes = new MemoryReadWrite<int>(10);
            var uf = new WeightedQuickUnionPathCompressionUF(memory, sizes);

            AssertUF(10, memory, uf);
        }

        private void AssertUF(long size, IMemoryReport report, IUnionFind uf)
        {
            WriteReport("Init", size, report);

            AssertAll(OnlyEqualPair(10), x => uf.Connected(x.Item1, x.Item2));
            AssertAll(NonEqualPair(10), x => uf.Connected(x.Item1, x.Item2) == false);

            report.Reset();
            uf.Union(2, 8);
            WriteReport("Union", size, report);

            report.Reset();
            var connected = uf.Connected(2, 8);
            WriteReport("Connected", size, report);

            Assert.IsTrue(connected);
            Assert.IsTrue(uf.Connected(8, 2));

            uf.Union(1, 8);

            Assert.IsTrue(uf.Connected(1, 8));
            Assert.IsTrue(uf.Connected(8, 1));
            Assert.IsTrue(uf.Connected(1, 2));
            Assert.IsTrue(uf.Connected(2, 1));
            Assert.IsTrue(uf.Connected(2, 8));
            Assert.IsTrue(uf.Connected(8, 2));

            report.Reset();
            foreach (var item in NonEqualPair(uf.Size))
            {
                uf.Union(item.Item1, item.Item2);
            }
            WriteReport("Union All", size, report);
        }

        private void WriteReport(string name, long size, IMemoryReport report)
        {
            Console.WriteLine(name);
            Console.WriteLine($"Reads: {report.Reads} {PrintBigO(size, report.Reads)}");
            Console.WriteLine($"Write: {report.Writes} {PrintBigO(size, report.Writes)}");

            report.Reset();
        }

        public string PrintBigO(long size, long value)
        {
            var dic = new Dictionary<string, double>();
            dic.Add("O(1)", Math.Abs(value - 1));
            dic.Add("O(ln N)", Math.Abs(value - Math.Log(size)));
            dic.Add("O(N)", Math.Abs(value - size));
            dic.Add("O(N²)", Math.Abs(value - (size * size)));
            dic.Add("O(N³)", Math.Abs(value - (size * size * size)));

            var max = MinElement(dic.AsEnumerable(), (k, v) => v);

            return max.Key;
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

        KeyValuePair<K, V> MaxElement<K, V>(IEnumerable<KeyValuePair<K, V>> items, Func<K, V, int> prop)
        {
            KeyValuePair<K, V>? max = null;
            int maxv = default(int);

            bool isNull = true;

            foreach (var item in items)
            {
                if (isNull)
                {
                    max = item;
                    maxv = prop(item.Key, item.Value);
                    isNull = false;
                }

                var current = prop(item.Key, item.Value);

                if (current > maxv)
                {
                    max = item;
                    maxv = current;
                }
            }

            return max.Value;
        }

        KeyValuePair<K, V> MinElement<K, V>(IEnumerable<KeyValuePair<K, V>> items, Func<K, V, double> prop)
        {
            KeyValuePair<K, V>? max = null;
            double candidate = default(int);

            bool isNull = true;

            foreach (var item in items)
            {
                if (isNull)
                {
                    max = item;
                    candidate = prop(item.Key, item.Value);
                    isNull = false;
                }

                var current = prop(item.Key, item.Value);

                if (current < candidate)
                {
                    max = item;
                    candidate = current;
                }
            }

            return max.Value;
        }
    }
}
