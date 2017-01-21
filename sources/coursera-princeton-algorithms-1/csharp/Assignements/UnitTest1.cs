using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.IO;

namespace Assignements
{
    public static class KaratsubaMultiplication
    {
        public static string Multiply(string l, string r)
        {
            if ((l.Length == 1) && (r.Length == 1))
            {
                return ((l[0] - 48) * (r[0] - 48)).ToString();
            }
            else
            {
                l = l.TrimStart('0');
                r = r.TrimStart('0');

                if (l.Length == 0 || r.Length == 0)
                {
                    return "0";
                }
                else if (l == "1")
                {
                    return r;
                }
                else if (r == "1")
                {
                    return l;
                }
                else if ((l.Length == 1) && (r.Length == 1))
                {
                    return ((l[0] - 48) * (r[0] - 48)).ToString();
                }

                int maxlength = Math.Max(l.Length, r.Length);

                if (l.Length > r.Length)
                {
                    r = r.PadLeft(l.Length, '0');
                }
                else if (l.Length < r.Length)
                {
                    l = l.PadLeft(r.Length, '0');
                }

                if (l.Length % 2 != 0)
                {
                    l = l.PadLeft(l.Length + (l.Length % 2), '0');
                    r = r.PadLeft(r.Length + (r.Length % 2), '0');
                }

                var a = l.Substring(0, l.Length / 2);
                var b = l.Substring(l.Length / 2, l.Length / 2);

                var c = r.Substring(0, l.Length / 2);
                var d = r.Substring(l.Length / 2, l.Length / 2);

                string ac = Multiply(a, c);
                //string ad = Multiply(a, d);
                //string bc = Multiply(b, c);
                string bd = Multiply(b, d);

                var aplusb = Sum1(a, b);
                var cplusd = Sum1(c, d);
                var adbc = Minus(Minus(Multiply(aplusb, cplusd), ac), bd);

                //string adbc = Sum1(ad, bc);

                var p1 = (BigInteger.Pow(10, l.Length) * BigInteger.Parse(ac)).ToString();
                //var p1 = ac + new string('0', ll);
                var p2 = (BigInteger.Pow(10, l.Length / 2) * BigInteger.Parse(adbc)).ToString();

                //var result = Sum(Sum(bd, p2), p1);

                var result = ((BigInteger.Parse(bd) + BigInteger.Parse(p2)) + BigInteger.Parse(p1)).ToString();

                Console.WriteLine($"{l} * {r} = {result} \n    = [{a}.10^{maxlength - 1}+{b}]*[{c}.10^{l.Length / 2}+{d}]\n    = {ac}.10^{l.Length}+(({a}+{b})*({c}+{d})-{ac}-{bd}).10^{l.Length / 2}+{bd}\n    = {p1} + {p2} + {bd}");

                return result;
            }
        }

        public static string Sum1(string l, string r)
        {
            if (l.Length == 1 && r.Length == 1)
            {
                return (int.Parse(l) + int.Parse(r)).ToString();
            }
            else
            {
                var li = l.Length - 1;
                var ri = r.Length - 1;

                var maxlength = Math.Max(l.Length, r.Length);
                var result = new char[maxlength + 1];

                int carriage = 0;
                for (int i = maxlength; i > 0; i--)
                {
                    int lc = 0;
                    if (li >= 0)
                    {
                        lc = (int)l[li] - 48;
                        li--;
                    }

                    int rc = 0;
                    if (ri >= 0)
                    {
                        rc = (int)r[ri] - 48;
                        ri--;
                    }

                    var re = lc + rc + carriage;

                    if (re >= 10)
                    {
                        re = re - 10;
                        carriage = 1;
                    }
                    else
                    {
                        carriage = 0;
                    }

                    result[i] = (char)(re + 48);
                }

                if (carriage == 0)
                {
                    return new string(result.Skip(1).ToArray());
                }
                else
                {
                    result[0] = '1';
                    return new string(result.ToArray());
                }
            }
        }

        public static string Sum2(string l, string r)
        {
            if (l.Length > r.Length)
            {
                r = r.PadLeft(l.Length, '0');
            }
            else if (l.Length < r.Length)
            {
                l = l.PadLeft(r.Length, '0');
            }

            var result = new char[l.Length + 1];

            int carriage = 0;
            for (int i = l.Length - 1; i >= 0; --i)
            {
                var s = ((char)l[i] - 48) + ((char)r[i] - 48) + carriage;
                if (s > 9)
                {
                    carriage = 1;
                    s = s - 10;
                }
                else
                {
                    carriage = 0;
                }

                result[i + 1] = (char)(s + 48);
            }

            if (carriage == 0)
            {
                return new string(result.Skip(1).ToArray());
            }
            else
            {
                result[0] = '1';
                return new string(result.ToArray());
            }
        }

        public static string Minus(string l, string r)
        {
            return (BigInteger.Parse(l) - BigInteger.Parse(r)).ToString();
        }
    }

    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void Sum()
        {
            var data = new[]
            {
                //new [] { "1", "2" },
                new [] { "10", "2" },
                new [] { "100", "200" },
                new [] { "1000", "2000" },
                new [] { "60000", "60000" },
                //new [] { "100000", "200000" },
                //new [] { "10000", "20000" },
                //new [] { "10000000", "20000000" },
                //new [] { "3141592653589793238462643383279502884197169399375105820974944592", "2718281828459045235360287471352662497757247093699959574966967627" }
            };

            foreach (var item in data)
            {
                var result = System.Numerics.BigInteger.Parse(item[0]) + System.Numerics.BigInteger.Parse(item[1]);
                Assert.AreEqual(result.ToString(),
                    KaratsubaMultiplication.Sum1(item[0], item[1]));

                Console.WriteLine("---------------------------------");
                Console.WriteLine(result.ToString());
                Console.WriteLine("---------------------------------");
            }
        }

        [TestMethod]
        public void SumRandom1()
        {
            var random = new Random();

            foreach (var item in Enumerable.Range(0, 100000))
            {
                var l = random.Next().ToString();
                var r = random.Next().ToString();
                var result = BigInteger.Parse(l) + BigInteger.Parse(r);
                Assert.AreEqual(result.ToString(),
                    KaratsubaMultiplication.Sum1(l, r));

                Console.WriteLine("---------------------------------");
                Console.WriteLine(result.ToString());
                Console.WriteLine("---------------------------------");
            }
        }

        [TestMethod]
        public void SumRandom2()
        {
            var random = new Random();

            foreach (var item in Enumerable.Range(0, 100000))
            {
                var l = random.Next().ToString();
                var r = random.Next().ToString();
                var result = BigInteger.Parse(l) + BigInteger.Parse(r);
                Assert.AreEqual(result.ToString(),
                    KaratsubaMultiplication.Sum2(l, r));

                Console.WriteLine("---------------------------------");
                Console.WriteLine(result.ToString());
                Console.WriteLine("---------------------------------");
            }
        }

        [TestMethod]
        public void TestMethod1()
        {
            var data = new[]
            {
                //new [] { "1", "2" },
                //new [] { "10", "20" },
                //new [] { "100", "200" },
                //new [] { "1000", "2000" },
                //new [] { "10000", "20000" },
                //new [] { "100000", "200000" },
                //new [] { "10000", "20000" },
                //new [] { "10000000", "20000000" },
                //new [] { "07", "0142" },
                new [] { "0010", "0486" }  
                //new [] { "3141592653589793238462643383279502884197169399375105820974944592", "2718281828459045235360287471352662497757247093699959574966967627" }
            };

            foreach (var item in data)
            {
                var result = System.Numerics.BigInteger.Parse(item[0]) * System.Numerics.BigInteger.Parse(item[1]);
                Assert.AreEqual(result.ToString(),
                    KaratsubaMultiplication.Multiply(item[0], item[1]));

                Console.WriteLine("---------------------------------");
                Console.WriteLine(result.ToString());
                Console.WriteLine("---------------------------------");
            }
        }

        [TestMethod]
        public void MulRandom()
        {
            var random = new Random();

            foreach (var item in Enumerable.Range(0, 2000))
            {
                var l = random.Next().ToString();
                var r = random.Next().ToString();
                var result = BigInteger.Parse(l) * BigInteger.Parse(r);
                Assert.AreEqual(result.ToString(),
                    KaratsubaMultiplication.Multiply(l, r));

                Console.WriteLine("---------------------------------");
                Console.WriteLine(result.ToString());
                Console.WriteLine("---------------------------------");
            }
        }
    }

    [TestClass]
    public class Week02Challenges
    {
        public class Comparer
        {
            public int Count { get; set; }

            public int Compare(int l, int r)
            {
                Count++;
                return l.CompareTo(r);
            }
        }

        public class SGN2
        {
            public Comparer Comparer;

            public SGN2()
            {
                Comparer = new Comparer();
            }

            public int Get(params int[] numbers)
            {
                var ns = new[] { Math.Min(numbers[0], numbers[1]), Math.Max(numbers[0], numbers[1]) };

                for (int i = 2; i < numbers.Length; ++i)
                {
                    if (Comparer.Compare(numbers[i], ns[1]) > 0)
                    {
                        ns[0] = ns[1];
                        ns[1] = numbers[i];
                    }
                    else if (Comparer.Compare(numbers[i], ns[0]) > 0)
                    {
                        ns[0] = numbers[i];
                    }
                }

                return ns[0];
            }
        }

        public class SecondGreatestNumber
        {
            public Comparer Comparer;

            public SecondGreatestNumber()
            {
                Comparer = new Comparer();
            }

            public int Get(params int[] numbers)
            {
                return _Get(numbers)[0];
            }

            public int[] _Get(params int[] numbers)
            {
                if (numbers.Length == 2)
                {
                    Array.Sort(numbers);
                    return numbers;
                }
                else
                {
                    int[] larger = new int[2];

                    int n = numbers.Length;
                    var l = _Get(numbers.Take(n / 2).ToArray());
                    var r = _Get(numbers.Skip(n / 2).ToArray());

                    int largeri = 0;
                    int li = 0;
                    int ri = 0;
                    for (int i = 0; i < 4 && largeri < 2; i++)
                    {
                        if (Comparer.Compare(l[li], r[li]) > 0)
                        {
                            larger[largeri] = l[li];
                            li++;
                        }
                        else
                        {
                            larger[largeri] = r[ri];
                            ri++;
                        }
                        largeri++;
                    }

                    return larger;
                }
            }
        }


        private static int[] GetArray(int size)
        {
            var array = new int[size];

            var r = new Random();
            for (int i = 0; i < size; i++)
            {
                array[i] = r.Next();
            }

            return array;
        }

        private static void ShowComparisons(int[] array)
        {
            var sgn = new SecondGreatestNumber();
            var n = sgn.Get(array);
            //Assert.AreEqual(3, n);
            Console.Write($"{sgn.Comparer.Count} - ");

            var SGN2 = new SGN2();
            SGN2.Get(array);

            Console.WriteLine(SGN2.Comparer.Count);
        }

        /// <summary>
        ///You are given as input an unsorted array of n distinct numbers,
        ///where n is a power of 2. Give an algorithm that identifies
        ///the second-largest number in the array, and
        ///that uses at most n+log2n−2 comparisons.
        /// </summary>
        [TestMethod]
        public void GetSecondBiggest()
        {
            for (int i = 1; i < 10; ++i)
            {
                int size = (int)Math.Pow(2, i);
                Console.Write($"Size: {size} - {size + (int)Math.Log(size, 2) - 2} - ");
                ShowComparisons(GetArray(size));
            }
        }

        /// <summary>
        /// You are a given a unimodal array of n distinct elements,
        /// meaning that its entries are in increasing order up
        /// until its maximum element, after which its elements are
        /// in decreasing order. Give an algorithm to compute the maximum
        /// element that runs in O(log n) time.
        /// </summary>
        [TestMethod]
        public void FindUnimodalBiggest()
        {
            var array = new[] { 1, 2, 3, 4, 5, 6, 5, 4 };
            var big = GetUnimodalBiggest(array);

            Assert.AreEqual(6, big);

            //var alg = new MasterMethod<int[]>();
            //alg.DividerSize = 2;
            //alg.AddDivider(x => x.Take(x.Length / 2).ToArray());
            ////alg.AddDivider(x => x.Skip(x.Length / 2).ToArray());
            //alg.ConstantConquer((l, r) =>
            //{
            //    return 0;
            //});

            //Console.WriteLine(alg.Complexity);
        }

        private int GetUnimodalBiggest(int[] array)
        {
            int n = array.Length;

            if (n <= 2)
            {
                return Math.Max(array[0], array[1]);
            }
            else
            {
                var l = array.Take(n / 2).ToArray();
                var r = array.Skip(n / 2).ToArray();

                var llast = l.Last();
                var rfirst = r.First();

                if (rfirst > llast)
                {
                    return GetUnimodalBiggest(r);
                }
                else //if (rfirst < llast)
                {
                    return GetUnimodalBiggest(l);
                }
            }
        }

        /// <summary>
        /// You are given a sorted (from smallest to largest) array A
        /// of n distinct integers which can be positive, negative,
        /// or zero. You want to decide whether or not there is an
        /// index i such that A[i] = i. Design the fastest algorithm
        /// that you can for solving this problem.
        /// </summary>
        [TestMethod]
        public void MyTestMethod()
        {
            var array = new[] { -1, 0, 1, 3, 4, 7, 8, 9 };
            var has = new HasAiEqualI().Run(array);
            Assert.IsTrue(has);
        }

        /// <summary>
        /// Best Case: O(log n)
        /// Worst Case: Complete Master Method (a=2, b=2, d=0)
        /// a > b^d = O(n^log_b(a)) = O(n^log_2(2)) = O(n^1) = O(n)
        /// </summary>
        public class HasAiEqualI
        {
            public bool Run(int[] array)
            {
                return Run(array, 0, array.Length - 1, 0);
            }

            public bool Run(int[] array, int istart, int iend, int rec)
            {

                //for (int i = 0; i < array.Length; ++i)
                //{
                //    if (array[i] == i)
                //    {
                //        return true;
                //    }
                //}

                if (istart == iend)
                {
                    Console.WriteLine($"{new string(' ', rec * 3)}{istart} - {iend} {PrintArray(array)}");
                    return array[0] == istart;
                }
                else
                {
                    Console.WriteLine($"{new string(' ', rec * 3)}{istart} - {iend} {PrintArray(array)}");
                }

                var l = array.Take(array.Length / 2).ToArray();
                var r = array.Skip(array.Length / 2).ToArray();

                var lfirst = l.First();
                var rfirst = r.First();

                var lresult = false;
                var rresult = false;

                if ((lfirst >= istart) && (lfirst <= iend))
                {
                    lresult = Run(l, istart, istart + l.Length - 1, rec + 1);
                }

                if ((rfirst >= istart) && (rfirst <= iend))
                {
                    rresult = Run(r, istart + l.Length, iend, rec + 1);
                }

                return lresult || rresult;
            }
        }

        static string PrintArray(int[] array)
        {
            return $"[{string.Join(",", array.Select(x => x.ToString()).ToArray())}]";
        }

        /// <summary>
        /// You are given an n by n grid of distinct numbers.
        /// A number is a local minimum if it is smaller than
        /// all of its neighbors. (A neighbor of a number is one
        /// immediately above, below, to the left, or the
        /// right. Most numbers have four neighbors; numbers 
        /// on the side have three; the four corners have two.)
        /// Use the divide-and-conquer algorithm design paradigm
        /// to compute a local minimum with only O(n) comparisons
        /// between pairs of numbers. (Note: since there are n2 
        /// numbers in the input, you cannot afford to look at 
        /// all of them.
        /// Hint: Think about what types of recurrences would 
        /// give you the desired upper bound.)
        /// </summary>
        [TestMethod]
        public void LowerestInMatrix()
        {
            var matrix = Parse(@"
5 5 5 6 5 5 5
5 4 4 5 4 3 5
5 4 4 5 4 4 5
5 4 4 5 4 4 5
5 4 4 5 4 4 5
5 4 4 5 4 4 5
5 4 4 5 4 4 5");
            localminimunCount = 0;
            var r = FindLocalMinimun(new MatrixReader(matrix), 0);
            Console.WriteLine($"Cmparisons = {localminimunCount}");
            Assert.AreEqual(3, r);
        }

        int localminimunCount = 0;

        private int FindLocalMinimun(MatrixReader matrix, int rec)
        {
            Console.WriteLine($"{matrix.X} {matrix.Y} {matrix.W} {matrix.H}");
            int x = matrix.W / 2;
            int mx = 0, my = 0;
            int currentMinimun = int.MaxValue;
            for (int y = 0; y < matrix.H; y++)
            {
                if (matrix._(x, y) < currentMinimun)
                {
                    mx = x;
                    my = y;
                    currentMinimun = matrix._(x, y);
                }
            }

            Console.WriteLine($"{mx} {my} {currentMinimun}");

            if (matrix._(mx + 1, my) < currentMinimun)
            {
                return FindLocalMinimun(new MatrixReader(matrix.Data,
                    matrix.X + matrix.W / 2,
                    matrix.Y, (int)Math.Ceiling(matrix.W / 2.0),
                    (int)Math.Ceiling(matrix.H / 2.0)), rec + 1);
            }
            else if (matrix._(mx - 1, my) < currentMinimun)
            {
                return FindLocalMinimun(new MatrixReader(matrix.Data,
                    matrix.X, matrix.Y,
                    (int)Math.Floor(matrix.W / 2.0),
                    (int)Math.Floor(matrix.H / 2.0)), rec + 1);
            }

            return currentMinimun;
        }

        public class MatrixReader
        {
            public int[][] Data;
            public int X;
            public int Y;
            public int W;
            public int H;

            public MatrixReader(int[][] data)
                : this(data, 0, 0, data.Length, data[0].Length)
            {
            }

            public MatrixReader(int[][] data, int x, int y, int w, int h)
            {
                Data = data;
                X = x; Y = y; W = w; H = h;
            }

            public int _(int x, int y)
            {
                var lx = X + x;
                var ly = Y + y;
                if (lx < 0) return int.MaxValue;
                if (ly < 0) return int.MaxValue;
                if (lx >= Data[0].Length) return int.MaxValue;
                if (ly >= Data.Length) return int.MaxValue;
                return Data[Y + y][lx];
            }

            public IEnumerable<MatrixReader> GetQuadrants()
            {
                int mx = W / 2;
                int my = H / 2;
                yield return new MatrixReader(Data, X, Y, mx, my);
                yield return new MatrixReader(Data, X + mx, Y, mx, my);
                yield return new MatrixReader(Data, X, Y + my, mx, my);
                yield return new MatrixReader(Data, X + mx, Y + my, mx, my);
            }

            internal bool islower(int x, int y)
            {
                var l = _(x - 1, y) > _(x, y);
                var r = _(x + 1, y) > _(x, y);
                var u = _(x, y - 1) > _(x, y);
                var d = _(x, y + 1) > _(x, y);

                return l || r || u || d;
            }
        }

        private int[][] Parse(string v)
        {
            return v.Split(new[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries)
                .Where(x => !string.IsNullOrEmpty(x))
                .Select(x =>
            {
                return x.Split(' ').Select(s => int.Parse(s)).ToArray();
            }).ToArray();
        }
    }

    public class MasterMethod<T>
    {
        List<Func<T, T>> Dividers = new List<Func<T, T>>();
        int D = 0;

        public int DividerSize { get; set; }

        public string Complexity
        {
            get
            {
                int a = Dividers.Count;
                int b = DividerSize;
                int d = D;

                if (a == (int)Math.Pow(b, d))
                {
                    if (d == 0)
                    {
                        return "O(log(n))";
                    }
                    else
                    {
                        return $"O(n^{d}*log(n))";
                    }
                }
                else if (a > (int)Math.Pow(b, d))
                {
                    var logba = (int)Math.Log(a, b);
                    return $"O(n^{logba})";
                }
                else //if (a < (int)Math.Pow(d, b))
                {
                    return $"O(n^{d})";
                }
            }
        }

        public void AddDivider(Func<T, T> divider)
        {
            Dividers.Add(divider);
        }

        internal void ConstantConquer(Func<object, object, object> p)
        {
            D = 0;
        }
    }



    [TestClass]
    public class Week03
    {
        [TestMethod]
        public void WhenAtBeginning()
        {
            var numbers = File.ReadLines(@"C:\Users\xunil\Desktop\_32387ba40b36359a38625cbb397eee65_QuickSort.txt")
                .Select(x => int.Parse(x))
                .ToArray();

            BigInteger comparisons = 0;
            Quicksort(new ArraySegment<int>(numbers), PivotType.Beginning, ref comparisons, 0);
            Console.WriteLine(comparisons);

            Console.WriteLine("---------------------");

            Assert.IsTrue(numbers.Zip(numbers.Skip(1), (l, r) => r - l).All(x => x == 1));
        }

        [TestMethod]
        public void WhenAtEnd()
        {
            var numbers = File.ReadLines(@"C:\Users\xunil\Desktop\_32387ba40b36359a38625cbb397eee65_QuickSort.txt")
                .Select(x => int.Parse(x))
                .ToArray();

            BigInteger comparisons = 0;
            Quicksort(new ArraySegment<int>(numbers), PivotType.End, ref comparisons, 0);
            Console.WriteLine(comparisons);

            Console.WriteLine("---------------------");

            Assert.IsTrue(numbers.Zip(numbers.Skip(1), (l, r) => r - l).All(x => x == 1));
        }

        [TestMethod]
        public void WhenBestThree()
        {
            var numbers = File.ReadLines(@"C:\Users\xunil\Desktop\_32387ba40b36359a38625cbb397eee65_QuickSort.txt")
                .Select(x => int.Parse(x))
                .ToArray();

            BigInteger comparisons = 0;
            Quicksort(new ArraySegment<int>(numbers), PivotType.BestThree, ref comparisons, 0);
            Console.WriteLine(comparisons);

            Console.WriteLine("---------------------");

            Assert.IsTrue(numbers.Zip(numbers.Skip(1), (l, r) => r - l).All(x => x == 1));
        }

        enum PivotType
        {
            Beginning, End, BestThree
        }

        void Quicksort(ArraySegment<int> array, PivotType type, ref BigInteger comp, int rec)
        {
            //Console.WriteLine($"{new string(' ', rec * 3)} {array.Offset} {array.Offset + array.Count - 1}");
            //-­‐ If n = 1 return
            if (array.Count == 1)
            {
                return;
            }

            int temp;

            //-­‐ p = ChoosePivot(A, n)
            var pivot = ChoosePivot(type, array);

            temp = array.Array[array.Offset];
            array.Array[array.Offset] = array.Array[pivot];
            array.Array[pivot] = temp;
            pivot = array.Offset;

            int i = array.Offset + 1;

            //-­‐ Partition  A  around  p
            for (int j = array.Offset + 1; j < array.Offset + array.Count; ++j)
            {
                //if A[j] < p
                if (array.Array[j] < array.Array[pivot])
                {
                    //-­‐swap A[j] and A[i]
                    temp = array.Array[i];
                    array.Array[i] = array.Array[j];
                    array.Array[j] = temp;
                    //-­‐  i:= i + 1
                    ++i;
                }
            }

            temp = array.Array[i - 1];
            array.Array[i - 1] = array.Array[pivot];
            array.Array[array.Offset] = temp;

            var ls = array.Offset;
            var le = i - 1;
            var rs = i;
            var re = array.Offset + array.Count;

            comp += array.Count - 1;

            //-­‐ Recursively sort 1 st part
            if (le > ls)
            {
                Quicksort(new ArraySegment<int>(array.Array, ls, le - ls), type, ref comp, rec + 1);
            }

            if (re > rs)
            {
                //-­‐ Recursively sort 2 nd part
                Quicksort(new ArraySegment<int>(array.Array, rs, re - rs), type, ref comp, rec + 1);
            }
        }

        private int ChoosePivot(PivotType type, ArraySegment<int> array)
        {
            if (type == PivotType.Beginning)
            {
                return array.Offset;
            }
            else if (type == PivotType.End)
            {
                return array.Offset + array.Count - 1;
            }
            else if (type == PivotType.BestThree)
            {
                var l = array.Offset;
                var lv = array.Array[l];
                var r = array.Offset + array.Count - 1;
                var rv = array.Array[r];

                var mpos = 0;
                if (array.Count % 2 == 0)
                {
                    mpos = (array.Count / 2) - 1;
                }
                else
                {
                    mpos = array.Count / 2;
                }
                var m = array.Offset + mpos;
                var mv = array.Array[m];

                if (lv <= mv && lv <= rv)
                {
                    if (mv < rv)
                    {
                        return m;
                    }
                    else
                    {
                        return r;
                    }
                }
                else if (mv <= lv && mv <= rv)
                {
                    if (lv < rv)
                    {
                        return l;
                    }
                    else
                    {
                        return r;
                    }
                }
                else if (rv <= lv && rv <= mv)
                {
                    if (lv < mv)
                    {
                        return l;
                    }
                    else
                    {
                        return m;
                    }
                }
            }

            throw new NotImplementedException();
        }
    }
}
