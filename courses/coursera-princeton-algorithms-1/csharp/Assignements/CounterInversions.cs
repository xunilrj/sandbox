using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;

namespace Assignements
{
    [TestClass]
    public class CounterInversions
    {
        [TestMethod]
        [Timeout(600000)]
        public void CountInversionsTest()
        {
            var file = File.OpenRead(@"C:\github\xunilrj-sandbox\sources\coursera-princeton-algorithms-1\_bcb5c6658381416d19b01bfc1d3993b5_IntegerArray.txt");
            var reader = new StreamReader(file);

            var numbers = new List<int>();
            while (reader.EndOfStream == false)
            {
                var number = int.Parse(reader.ReadLine());
                numbers.Add(number);
            }

            var array = numbers.ToArray();
            var r = CountInversions(new ArraySegment<int>(array));

            //var r = CountInversions(new ArraySegment<int>(new[] { 1, 3, 5, 2, 4, 6 }));
            //var r = CountInversions(new ArraySegment<int>(new[] { 1, 3, 5, 2, 4, 6 }));

            Console.WriteLine(r);
        }

        private BigInteger CountInversions(ArraySegment<int> array)
        {
            return CountArray(array, 0).Item2;
        }

        private Tuple<ArraySegment<int>, BigInteger> CountArray(ArraySegment<int> segment, int rec)
        {
            //GC.Collect();
            //Console.WriteLine($"{new string(' ', rec * 3)} {segment.Offset} - {segment.Offset + segment.Count - 1}");
            if (segment.Count == 1)
            {
                return Tuple.Create(segment, (BigInteger)0);
            }
            else
            {
                ArraySegment<int> l, r;

                if (segment.Count % 2 == 1)
                {
                    var newsize = (int)Math.Ceiling(segment.Count / 2.0);
                    l = new ArraySegment<int>(segment.Array, segment.Offset, newsize);
                    r = new ArraySegment<int>(segment.Array, segment.Offset + newsize, segment.Count - newsize);
                }
                else
                {
                    l = new ArraySegment<int>(segment.Array, segment.Offset, segment.Count / 2);
                    r = new ArraySegment<int>(segment.Array, segment.Offset + segment.Count / 2, segment.Count / 2);
                }

                var cl = CountArray(l, rec + 1);
                var cr = CountArray(r, rec + 1);

                var li = 0;
                var ri = 0;
                var count = (BigInteger)0;

                var result = new ArraySegment<int>(new int[segment.Count]);

                for (int i = 0; i < segment.Count; i++)
                {
                    if (li < l.Count)
                    {
                        if (ri >= r.Count || cl.Item1.At(li) < cr.Item1.At(ri))
                        {
                            //Console.WriteLine($"{new string(' ', rec * 3)} l={string.Join(",", cl.Item1.Array.Skip(l.Offset + li).Take(l.Count - li).Select(x => x.ToString()).ToArray())} r={string.Join(",", cr.Item1.Array.Skip(r.Offset + ri).Take(r.Count - ri).Select(x => x.ToString()).ToArray())}");
                            result.At(i, cl.Item1.At(li));
                            li++;
                        }
                        else
                        {
                            //Console.WriteLine($"{new string(' ', rec * 3)} l={string.Join(",", cl.Item1.Array.Skip(l.Offset + li).Take(l.Count - li).Select(x => x.ToString()).ToArray())} r={string.Join(",", cr.Item1.Array.Skip(r.Offset + ri).Take(r.Count - ri).Select(x => x.ToString()).ToArray())} +{l.Count - li}");

                            count += l.Count - li;
                            result.At(i, cr.Item1.At(ri));
                            ri++;
                        }
                    }
                    else
                    {
                        result.At(i, cr.Item1.At(ri));
                        ri++;
                    }
                }

                //Console.WriteLine($"{new string(' ', rec * 3)} result={string.Join(",", result)}");

                return Tuple.Create(result, count + cl.Item2 + cr.Item2);
            }
        }
    }

    public static class A
    {
        public static T At<T>(this ArraySegment<T> segment, int i)
        {
            return segment.Array[segment.Offset + i];
        }

        public static void At<T>(this ArraySegment<T> segment, int i, T value)
        {
            segment.Array[segment.Offset + i] = value; ;
        }
    }
}
