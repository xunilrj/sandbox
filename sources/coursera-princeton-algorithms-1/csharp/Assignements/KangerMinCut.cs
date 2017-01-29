using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections;
using System.Diagnostics;

namespace Assignements
{
    [TestClass]
    public class KangerMinCut
    {
        [TestMethod]
        public void LinkedList()
        {
            var list = new MyLinkedList<int>();

            list.AddLast(1); Assert.AreEqual(1, list.Count);
            list.AddLast(2); Assert.AreEqual(2, list.Count);
            list.AddLast(2); Assert.AreEqual(3, list.Count);
            list.AddLast(3); Assert.AreEqual(4, list.Count);

            list.RemoveAll(x => x == 2); Assert.AreEqual(2, list.Count);

            list.AddLast(2); Assert.AreEqual(3, list.Count);
            list.AddLast(2); Assert.AreEqual(4, list.Count);

            list.RemoveAll(x => x == 2); Assert.AreEqual(2, list.Count);

            list.RemoveAll(x => x == 1); Assert.AreEqual(1, list.Count);

            list.RemoveAll(x => x == 3); Assert.AreEqual(0, list.Count);
        }

        [TestMethod]
        public void MyTestMethod()
        {
            var graph = new Graph();
            var lines = File.ReadLines(@"C:\github\xunilrj-sandbox\sources\coursera-princeton-algorithms-1\csharp\Assignements\_f370cd8b4d3482c940e4a57f489a200b_kargerMinCut.txt");

            foreach (var item in lines)
            {
                var info = item.Split('\t');
                var v = int.Parse(info[0]);
                graph.AddVertex(v);

                foreach (var target in info.Skip(1))
                {
                    if (string.IsNullOrEmpty(target) == false)
                    {
                        graph.AddEdge(v, int.Parse(target));
                    }
                }
            }

            KangerMinCut.Find(graph);
        }

        private static void Find(Graph graph)
        {
            var random = new Random();
            int bestCut = int.MaxValue;

            for (int i = 0; i < 200 * 200; i++)
            {
                var clone = graph.Clone();

                while (clone.VertexCount > 2)
                {
                    var v1 = clone.GetRandomEdge(random);
                    clone.Merge(v1.Item1, v1.Item2);
                }

                if (clone.EdgeCount < bestCut)
                {
                    bestCut = clone.EdgeCount;
                }

            }
            Console.WriteLine(bestCut);
        }

        public class Graph
        {
            public int VertexCount { get { return Vertices.Count; } }
            public int EdgeCount { get { return Edge.Count; } }
            Dictionary<int, bool> Hashes = new Dictionary<int, bool>();

            List<int> Vertices = new List<int>();
            MyLinkedList<Tuple<int, int>> Edge = new MyLinkedList<Tuple<int, int>>();

            public Graph Clone()
            {
                return new Assignements.KangerMinCut.Graph()
                {
                    Vertices = new List<int>(this.Vertices),
                    Edge = new MyLinkedList<Tuple<int, int>>(this.Edge)
                };
            }

            public void AddEdge(int v1, int v2)
            {
                if (v1 < v2)
                {
                    var h = $"{v1:D4}{v2:D4}".GetHashCode();
                    if (!Hashes.ContainsKey(h))
                    {
                        Edge.AddLast(Tuple.Create(v1, v2));
                        Hashes.Add(h, true);
                    }
                }
                else
                {
                    var h = $"{v2:D4}{v1:D4}".GetHashCode();
                    if (!Hashes.ContainsKey(h))
                    {
                        Edge.AddLast(Tuple.Create(v2, v1));
                        Hashes.Add(h, true);
                    }
                }
            }

            public void AddVertex(int v)
            {
                Vertices.Add(v);
            }

            public int GetRandomVertex(Random random)
            {
                return Vertices[random.Next(Vertices.Count)];
            }

            public Tuple<int, int> GetRandomEdge(Random random)
            {
                return Edge.Skip(random.Next(Edge.Count - 1)).First();
            }

            internal void Merge(int v1, int v2)
            {
                Vertices.Remove(v2);

                var f = Edge.First;
                while (f != null)
                {
                    if (f.Value.Item1 == v2)
                    {
                        f.Value = Tuple.Create(v1, f.Value.Item2);
                    }
                    else if (f.Value.Item2 == v2)
                    {
                        f.Value = Tuple.Create(f.Value.Item1, v1);
                    }

                    f = f.Next;
                }

                Edge.RemoveAll(x => x.Item1 == x.Item2);
            }
        }
    }

    public class MyLinkedList<T> : IEnumerable<T>
    {
        public LinkedListNode<T> First { get; set; }
        public LinkedListNode<T> Last { get; set; }

        int _count = 0;
        public int Count
        {
            get
            {
                return _count;
            }
            private set
            {
                _count = value;
                if (_count < 0)
                    throw new ArgumentOutOfRangeException();
            }
        }

        public MyLinkedList(IEnumerable<T> range) : this()
        {
            foreach (var item in range)
            {
                AddLast(item);
            }
        }

        public MyLinkedList()
        {
            _count = 0;
        }

        public void AddLast(T value)
        {
            Count++;

            if (Last == null)
            {
                First = Last = new LinkedListNode<T>()
                {
                    Value = value
                };
            }
            else
            {
                var previousLast = Last;
                Last = new LinkedListNode<T>()
                {
                    Value = value,
                    Previous = previousLast
                };
                previousLast.Next = Last;
            }
        }

        public void RemoveAll(Func<T, bool> pred)
        {
            var f = First;
            while (f != null)
            {
                if (pred(f.Value))
                {
                    Count--;
                    if (f.Previous == null)
                    {
                        First = f.Next;
                        if (f.Next != null)
                        {
                            f.Next.Previous = null;
                        }
                    }
                    else
                    {
                        f.Previous.Next = f.Next;
                        if (f.Next != null)
                        {
                            f.Next.Previous = f.Previous;
                        }
                    }

                    if (f == Last)
                    {
                        Last = f.Previous;
                    }
                }

                f = f.Next;
            }
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return new Enumerator(First);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new Enumerator(First);
        }

        public class Enumerator : IEnumerator<T>
        {
            public T Current { get { return Node.Value; } }

            object IEnumerator.Current
            {
                get
                {
                    return Current;
                }
            }

            LinkedListNode<T> First;
            LinkedListNode<T> Node;

            public Enumerator(LinkedListNode<T> node)
            {
                First = Node = node;
            }

            public void Dispose()
            {
            }

            public bool MoveNext()
            {
                Node = Node.Next;
                return Node != null;
            }

            public void Reset()
            {
                Node = First;
            }
        }
    }

    public class LinkedListNode<T>
    {
        public T Value { get; set; }
        public LinkedListNode<T> Previous { get; set; }
        public LinkedListNode<T> Next { get; set; }
    }
}
