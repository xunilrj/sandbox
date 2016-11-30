using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Diagnostics;
using System.Linq;
using System.IO;

namespace SedgewickBook
{
    [TestClass]
    public class Chapter01Exercises
    {
        [TestMethod]
        public void Exercise11001()
        {
            Assert.AreEqual(7, (0 + 15) / 2);
            Assert.AreEqual(200.0000002, 2.0e-6 * 100000000.1);
            Assert.IsTrue(true && false || true && true);
        }

        [TestMethod]
        public void Exercise11002()
        {
            var result1 = (1 + 2.236) / 2;
            Assert.IsInstanceOfType(result1, typeof(double));
            Assert.AreEqual(1.618, result1);

            var result2 = 1 + 2 + 3 + 4.0;
            Assert.IsInstanceOfType(result2, typeof(double));
            Assert.AreEqual(10.0, result2);

            var result3 = 4.1 >= 4;
            Assert.IsInstanceOfType(result3, typeof(bool));
            Assert.IsTrue(result3);

            var result4 = 1 + 2 + "3";
            Assert.IsInstanceOfType(result4, typeof(string));
            Assert.AreEqual("33", result4);
        }

        [TestMethod]
        public void Exercise11003()
        {
            global::Exercise113.Program.Main(new[] { "1", "1", "1" });
            global::Exercise113.Program.Main(new[] { "1", "2", "1" });
            global::Exercise113.Program.Main(new[] { "1", "1", "2" });
            global::Exercise113.Program.Main(new[] { "2", "1", "2" });
        }

        [TestMethod]
        public void Exercise11005()
        {
            Assert.IsTrue(BetweenZeroOne(0.1, 0.2));

            Assert.IsFalse(BetweenZeroOne(-0.1, 0.2));
            Assert.IsFalse(BetweenZeroOne(0.1, -0.2));
            Assert.IsFalse(BetweenZeroOne(-0.1, -0.2));

            Assert.IsFalse(BetweenZeroOne(1.1, 0.2));
            Assert.IsFalse(BetweenZeroOne(0.1, 1.2));
            Assert.IsFalse(BetweenZeroOne(1.1, 1.2));

            Assert.IsFalse(BetweenZeroOne(-1.1, 1.2));
            Assert.IsFalse(BetweenZeroOne(1.1, -1.2));
        }

        bool BetweenZeroOne(double x, double y)
        {
            return Between(x, 0, 1) && Between(y, 0, 1);
        }

        bool BetweenZeroOne(double x)
        {
            return Between(x, 0, 1);
        }

        bool Between(double x, double start, double end)
        {
            return (x > start) && (x < end);
        }

        [TestMethod]
        public void Exercise11006()
        {
            int f = 0;
            int g = 1;
            for (int i = 0; i <= 15; i++)
            {
                Console.WriteLine(f);
                f = f + g;
                g = f - g;
            }
        }

        [TestMethod]
        public void Exercise11007()
        {
            double t = 9.0;
            while (Math.Abs(t - 9.0 / t) > .001)
                t = (9.0 / t + t) / 2.0;
            Console.WriteLine("{0:N5}", t);

            int sum = 0;
            for (int i = 1; i < 1000; i++)
                for (int j = 0; j < i; j++)
                    sum++;
            Console.WriteLine(sum);

            sum = 0;
            for (int i = 1; i < 1000; i *= 2)
                for (int j = 0; j < 1000; j++)
                    sum++;
            Console.WriteLine(sum);
        }

        [TestMethod]
        public void Exercise11008()
        {
            Console.WriteLine('b');
            Console.WriteLine('b' + 'c');
            Console.WriteLine((char)('a' + 4));
        }

        [TestMethod]
        public void Exercise11009()
        {
            int x = 7;
            uint mask = (uint)1 << 31;
            while (mask > 0)
            {
                var r = x & mask;
                Console.Write(r > 0 ? "1" : "0");
                mask = mask >> 1;
            }
        }

        [TestMethod]
        public void Exercise11011()
        {
            bool[,] m = new bool[20, 20];

            for (int i = 0; i < m.GetLength(0); ++i)
            {
                for (int j = 0; j < m.GetLength(1); ++j)
                {
                    m[i, j] = true;
                }
            }
            Print(m);
        }

        private void Print(bool[,] matrix)
        {
            Print(matrix, 0, 1);
        }

        private void Print(bool[,] matrix, int yaxis, int xaxis)
        {
            var ysize = matrix.GetLength(yaxis);
            var xsize = matrix.GetLength(xaxis);

            var ychars = (int)Math.Log10(ysize) + 1;
            var xchars = (int)Math.Log10(xsize) + 1;

            var ycolumnsize = ychars + 4;

            Console.Write(new string(' ', ycolumnsize));

            //Print Header
            for (int i = 0; i < matrix.GetLength(xaxis); i++)
            {
                Console.Write("[,");
                Console.Write((i).ToString().PadLeft(xchars));
                Console.Write("] ");
            }

            Console.WriteLine();

            for (int y = 0; y < matrix.GetLength(yaxis); y++)
            {
                Console.Write("[");
                Console.Write(y.ToString().PadLeft(ychars));
                Console.Write(",] ");

                for (int x = 0; x < matrix.GetLength(xaxis); x++)
                {
                    int lchars = xchars / 2;
                    int lmod = xchars % 2;
                    Console.Write(" ".PadLeft(lchars + lmod + 1));
                    var indices = new[] { y, x }
                        .Zip(new[] { yaxis, xaxis }, Tuple.Create)
                        .OrderBy(item => item.Item2)
                        .Select(item => item.Item1)
                        .ToArray();
                    Console.Write((bool)matrix.GetValue(indices) ? ("*") : (" "));
                    Console.Write(" ".PadRight(lchars + 2));
                }

                Console.WriteLine();
            }
        }

        [TestMethod]
        public void Exercise11012()
        {
            int[] a = new int[10];
            for (int i = 0; i < 10; i++)
                a[i] = 9 - i;
            for (int i = 0; i < 10; i++)
                a[i] = a[a[i]];
            for (int i = 0; i < 10; i++)
                Console.WriteLine(a[i]);
        }

        [TestMethod]
        public void Exercise11013()
        {
            bool[,] matrix = new bool[2, 3];
            PrintTransposition(matrix);
        }

        private void PrintTransposition(bool[,] matrix)
        {
            Print(matrix, 1, 0);
        }

        [TestMethod]
        public void Exercise11014()
        {
            Assert.AreEqual(1, Lg(2));
            Assert.AreEqual(1, Lg(3));
            Assert.AreEqual(2, Lg(4));
            Assert.AreEqual(2, Lg(5));
            Assert.AreEqual(2, Lg(6));
            Assert.AreEqual(2, Lg(7));
            Assert.AreEqual(3, Lg(8));
            Assert.AreEqual(3, Lg(9));
            Assert.AreEqual(3, Lg(10));
            Assert.AreEqual(3, Lg(11));
            Assert.AreEqual(3, Lg(12));
            Assert.AreEqual(3, Lg(13));
            Assert.AreEqual(3, Lg(14));
            Assert.AreEqual(3, Lg(15));
            Assert.AreEqual(4, Lg(16));
            Assert.AreEqual(4, Lg(17));
        }

        private static int Lg(int x)
        {
            int lg = x;
            int i = 0;

            while (lg > 1)
            {
                lg = lg >> 1;
                i++;
            }

            return i;
        }

        [TestMethod]
        public void Exercise11015()
        {
            var numbers = new[] { 1, 1, 1, 2, 1, 3, 2, 1 };
            var hist = Histogram(4, numbers);

            Assert.AreEqual(0, hist[0]);
            Assert.AreEqual(5, hist[1]);
            Assert.AreEqual(2, hist[2]);
            Assert.AreEqual(1, hist[3]);
        }

        private int[] Histogram(int size, int[] numbers)
        {
            var hist = new int[size];

            for (int i = 0; i < numbers.Length; i++)
            {
                hist[numbers[i]]++;
            }

            return hist;
        }

        [TestMethod]
        public void Exercise11016()
        {
            Console.WriteLine(exR1(6));
        }

        public static String exR1(int n)
        {
            if (n <= 0) return "";
            return exR1(n - 3) + n + exR1(n - 2) + n;
        }

        [TestMethod]
        public void Exercise11018()
        {
            Console.WriteLine(mystery(2, 25));
            Console.WriteLine(mystery(3, 11));

            Console.WriteLine(mystery2(2, 10));
        }

        public static int mystery(int a, int b)
        {
            if (b == 0) return 0;
            if (b % 2 == 0) return mystery(a + a, b / 2);
            return mystery(a + a, b / 2) + a;
        }

        public static int mystery2(int a, int b)
        {
            if (b == 0) return 1;
            if (b % 2 == 0) return mystery2(a * a, b / 2);
            return mystery2(a * a, b / 2) * a;
        }

        [TestMethod, Ignore]
        public void Exercise11019A()
        {
            for (int N = 0; N < 100; N++)
            {
                var w = Stopwatch.StartNew();

                var result = F(N);

                w.Stop();
                Console.WriteLine($"{w.Elapsed} - {N} - {result}");

            }
        }

        [TestMethod]
        public void Exercise11019B()
        {
            for (int N = 0; N < 100; N++)
            {
                var w = Stopwatch.StartNew();

                var result = F2(N);

                w.Stop();
                Console.WriteLine($"{w.Elapsed} - {N} - {result}");

            }
        }

        public static long F(int N)
        {
            if (N == 0) return 0;
            if (N == 1) return 1;
            return F(N - 1) + F(N - 2);
        }

        static long?[] F2Result = new long?[100];
        public static long F2(int N)
        {
            if (F2Result[N].HasValue)
            {
                return F2Result[N].Value;
            }

            if (N == 0)
            {
                F2Result[0] = 0;
                return 0;
            }
            if (N == 1)
            {
                F2Result[1] = 1;
                return 1;
            }

            var result = F2(N - 1) + F2(N - 2);
            F2Result[N] = result;
            return result;
        }

        [TestMethod]
        public void Exercise11020()
        {
            var rf = Factorial(10);
            var rln = Math.Log(rf);
            Assert.AreEqual(rln, lnf(10), 0.000001);
        }

        public static int Factorial(int N)
        {
            int result = 1;
            while (N > 0)
            {
                result *= N;
                N--;
            }
            return result;
        }

        public static double lnf(int N)
        {
            if (N == 0) return 0;
            return Math.Log(N) + lnf(N - 1);
        }

        [TestMethod]
        public void Exercise11021()
        {
            var reader = new StringReader(@"Daniel 4 2
Daniel 5 2");
            Print(reader);
        }

        private void Print(StringReader reader)
        {
            string line = reader.ReadLine();
            while (line != null)
            {
                var data = line.Split(' ');
                var result = double.Parse(data[1]) / double.Parse(data[2]);
                Console.WriteLine($"{data[0]} {data[1]} {data[2]} {result:N3}");
                line = reader.ReadLine();
            }
        }

        [TestMethod]
        public void Exercise11022()
        {
            var numbers = new[] { 10, 20, 30, 40, 50, 60, 70, 80 };
            TracedRank(30, numbers);
        }

        public static int TracedRank(int key, int[] a)
        {
            return TracedRank(key, a, 0, a.Length - 1, 0);
        }

        public static int TracedRank(int key, int[] a, int lo, int hi, int depth)
        {
            // Index of key in a[], if present, is not smaller than lo
            // and not larger than hi.
            Console.WriteLine($"{new String(' ', depth * 4)} {lo} {hi}");

            if (lo > hi) return -1;

            int mid = lo + (hi - lo) / 2;

            if (key < a[mid])
            {
                return TracedRank(key, a, lo, mid - 1, ++depth);
            }
            else if (key > a[mid])
            {
                return TracedRank(key, a, mid + 1, hi, ++depth);
            }
            else
            {
                return mid;
            }
        }

        [TestMethod]
        public void Exercise11023()
        {
            var numbers = new[] { 10, 20, 30, 40, 50, 60, 70, 80 };
            BlackWhiteList(numbers, "+", new StringReader("10\r\n11\r\n19\r\n20\r\n21"));
            Console.WriteLine("-------------------");
            BlackWhiteList(numbers, "-", new StringReader("10\r\n11\r\n19\r\n20\r\n21"));
        }

        public static void BlackWhiteList(int[] numbers, string op, TextReader reader)
        {
            string line = reader.ReadLine();
            while(line != null)
            {
                var n = int.Parse(line);
                var rank = BinarySearch.Rank(n, numbers);

                if (op == "+")
                {
                    if(rank == -1)
                    {
                        Console.WriteLine(n);
                    }
                }
                else
                {
                    if (rank != -1)
                    {
                        Console.WriteLine(n);
                    }
                }

                line = reader.ReadLine();
            }
        }

        [TestMethod]
        public void Exercise11024()
        {
            TracedGCD(new StringReader(@"1111111 1234567"));
        }

        private void TracedGCD(TextReader reader)
        {
            var numbers = reader.ReadLine();
            var data = numbers.Split(' ').Select(x => int.Parse(x)).ToArray();

            var gcd = Euclid.TracedGCD(data[0], data[1]);
            Console.WriteLine(gcd);
        }

        [TestMethod]
        public void Exercise11027()
        {
            AssertBinomialCallCount(0, 0, 0);

            AssertBinomialCallCount(2, 1, 0);
            AssertBinomialCallCount(4, 2, 0);
            AssertBinomialCallCount(6, 3, 0);

            AssertBinomialCallCount(200, 100, 0);
            AssertBinomialCallCount(10102, 100, 1);
            AssertBinomialCallCount(333702, 100, 2);
            //AssertBinomialCallCount(8186052, 100, 3);
            //AssertBinomialCallCount(200, 100, 4);

            AssertBinomialCallCount(2, 0, 1);
            AssertBinomialCallCount(2, 0, 2);

            AssertBinomialCallCount(4, 1, 1);
            AssertBinomialCallCount(6, 1, 2);
            AssertBinomialCallCount(6, 1, 3);
            AssertBinomialCallCount(6, 1, 4);

            AssertBinomialCallCount(12, 2, 2);
            AssertBinomialCallCount(28, 3, 3);

            //AssertBinomialCallCount(28, 10, 20);
        }

        private static void AssertBinomialCallCount(int expected, int N, int k)
        {
            int count = 0;
            binomial(N, k, 0, ref count);
            Assert.AreEqual(expected, count);
        }

        static int?[,] binomialMemory = new int?[101, 101];
        public static double binomial(int N, int k, double p, ref int count)
        {
            if ((N == 0) && (k == 0)) return 1.0;
            if ((N < 0) || (k < 0)) return 0.0;

            //if (N >= 1 && k >= 1 && binomialMemory[N-1, k].HasValue && binomialMemory[N - 1, k-1].HasValue)
            //{
            //    count += binomialMemory[N-1, k].Value + binomialMemory[N-1, k-1].Value;
            //    count += 2;
            //    binomialMemory[N, k] = count;
            //    return 0;
            //}
            //else
            //{
                count += 2;
                binomialMemory[N, k] = count;
                return (1 - p) * binomial(N - 1, k, p, ref count) + p * binomial(N - 1, k - 1, p, ref count);
            //}
        }
    }

    public static class Euclid
    {
        public static int gcd(int p, int q)
        {
            if (q == 0) return p;
            return gcd(q, p % q);
        }

        public static int TracedGCD(int p, int q)
        {
            Console.WriteLine($"{p} {q}");

            if (q == 0) return p;
            int r = p % q;
            return TracedGCD(q, r);
        }
    }


    [TestClass]
    public class Page21
    {


        [TestMethod]
        public void TestMethod1()
        {
            //TestSQRT("ClassicNewton1", x => SQRTS.ClassicNewton1(x));


            //[1,2,3,4,5,6,7,8,9,10,11,12,13]
            // ^                           ^
            // ^         ^   ^             ^
            //     ^
            // ^ ^   ^ ^          
            Assert.AreEqual(3, (6 + 1) / 2);
            Assert.AreEqual(3, (6 - 1) / 2 + 1);

        }

        void TestSQRT(string name, Func<double, double> calc)
        {
            var sw = Stopwatch.StartNew();

            for (int i = 2; i < 10000; i++)
            {
                var fromDOTNET = Math.Sqrt((double)i);
                var fromFunc = calc((double)i);

                Assert.AreEqual(fromDOTNET, fromFunc, 0.0000001);
            }

            sw.Stop();
            Console.WriteLine($"{name}: {sw.Elapsed}");
        }
    }

    public static class Helpers
    {
        /// <summary>
        /// find the maximum of the array values
        /// page 21
        /// </summary>
        public static double Max(double[] a)
        {
            double max = a[0];
            for (int i = 1; i < a.Length; i++)
            {
                if (a[i] > max)
                {
                    max = a[i];
                }
            }

            return max;
        }

        /// <summary>
        /// compute the average of the array values.
        /// Page 21
        /// </summary>
        /// <param name="a"></param>
        /// <returns></returns>
        public static double Average(double[] a)
        {
            int N = a.Length;

            double sum = 0.0;
            for (int i = 0; i < N; i++)
            {
                sum += a[i];
            }

            return sum / N;
        }

        /// <summary>
        /// Copy to another array
        /// page 21
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="a"></param>
        /// <param name="b"></param>
        public static void Copy<T>(T[] a, out T[] b)
        {
            int N = a.Length;
            b = new T[N];

            for (int i = 0; i < N; i++)
            {
                b[i] = a[i];
            }
        }

        /// <summary>
        /// reverse the elements within an array
        /// page 21
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="a"></param>
        public static void Reverse<T>(T[] a)
        {
            int N = a.Length;

            for (int i = 0; i < N / 2; i++)
            {
                T temp = a[i];
                a[i] = a[N - i - 1];
                a[N - i - 1] = temp;
            }
        }

        /// <summary>
        /// matrix-matrix multiplication
        /// (square matrices)
        /// a[][]*b[][] = c[][]
        /// page 21
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        public static void Multiply(double[,] a, double[,] b)
        {
            int N = a.GetLength(0);
            double[,] c = new double[N, N];

            for (int i = 0; i < N; i++)
                for (int j = 0; j < N; j++)
                {
                    // Compute dot product of row i and column j.
                    for (int k = 0; k < N; k++)
                    {
                        c[i, j] += a[i, k] * b[k, j];
                    }
                }
        }

        /// <summary>
        /// page 23
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static int abs(int x)
        {
            if (x < 0) return -x;
            else return x;
        }

        /// <summary>
        /// page 23
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double abs(double x)
        {
            if (x < 0.0) return -x;
            else return x;
        }

        /// <summary>
        /// page 23
        /// </summary>
        /// <param name="N"></param>
        /// <returns></returns>
        public static bool isPrime(int N)
        {
            if (N < 2) return false;

            for (int i = 2; i * i <= N; i++)
            {
                if (N % i == 0)
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Newtons Method
        /// page 23
        /// Tangent linequation is
        /// y - y_0 = m(x - x_0)
        /// this line intercept x-axis at
        /// (x_1,0), so
        /// 0 - y_0 = m(x_1 - x_0)
        /// - y_0/m = x_1 - x_0
        /// x_1 = x_0 - (y_0/m)
        /// m is the slope of the tangent line
        /// that is the functino derivative
        /// and y_0 is the function on x_0
        /// so
        /// x_1 = x_0 - (f(x_0)/f'(x_0))
        /// using the newtons method with sqrt
        /// f(x) = x^2 - c
        /// and
        /// f'(x) = 2x 
        /// so
        /// x_{i+1} = x_i - ((x_i*x_i) - c)
        ///                -----------------
        ///                     2*x_i
        ///                     
        ///         = x_i * 2*x_i  - ((x_i*x_i) - c) 
        ///                -------   ----------------
        ///                 2*x_i        2*x_i
        /// 
        ///         = 2*x_i*x_i - x_i*x_i + c 
        ///           ------------------------
        ///                   2*x_i
        ///                   
        ///         = x_i*x_i + c 
        ///           ------------
        ///              2*x_i
        /// 
        ///         =   x_i*x_i       c         1
        ///           ( -------  +  -----  ) * ----
        ///               x_i        x_i        2
        /// 
        /// 
        ///        =           c         1
        ///          ( x_i + ----- ) * ----
        ///                   x_i        2
        /// 
        /// </summary>
        /// <param name="c"></param>
        /// <returns></returns>
        public static double sqrt(double c)
        {
            if (c < 0.0) return Double.NaN;

            double err = 1e-15;
            double t = c;

            while (abs(t - c / t) > err * t)
            {
                t = (c / t + t) / 2.0;
            }

            return t;
        }

        /// <summary>
        /// page 23
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>
        public static double hypotenuse(double a, double b)
        {
            return sqrt(a * a + b * b);
        }

        /// <summary>
        /// see page 185
        /// </summary>
        /// <param name="N"></param>
        /// <returns></returns>
        public static double H(int N)
        {
            double sum = 0.0;
            for (int i = 1; i <= N; i++)
            {
                sum += 1.0 / i;
            }

            return sum;
        }
    }

    public static class BinarySearch
    {
        public static int Rank(int key, int[] a)
        {
            return rank(key, a, 0, a.Length - 1);
        }

        public static int rank(int key, int[] a, int lo, int hi)
        {
            // Index of key in a[], if present, is not smaller than lo
            // and not larger than hi.

            if (lo > hi) return -1;

            int mid = lo + (hi - lo) / 2;

            if (key < a[mid])
            {
                return rank(key, a, lo, mid - 1);
            }
            else if (key > a[mid])
            {
                return rank(key, a, mid + 1, hi);
            }
            else
            {
                return mid;
            }
        }
    }

    public static class StdRandom
    {
        public static double random()
        {
            return new Random().NextDouble();
        }

        /// <summary>
        /// random double value in [a, b)
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>
        public static double Uniform(double a, double b)
        {
            return a + StdRandom.random() * (b - a);
        }

        /// <summary>
        /// random int value in [0..N)
        /// </summary>
        /// <param name="N"></param>
        /// <returns></returns>
        public static int Uniform(int N)
        {
            return (int)(StdRandom.random() * N);
        }

        /// <summary>
        /// random int value in [lo..hi)
        /// </summary>
        /// <param name="lo"></param>
        /// <param name="hi"></param>
        /// <returns></returns>
        public static int uniform(int lo, int hi)
        {
            return lo + StdRandom.Uniform(hi - lo);
        }

        /// <summary>
        /// random int value drawn
        /// from discrete distribution
        /// (i with probability a[i] )
        /// </summary>
        /// <param name="a"></param>
        /// <returns></returns>
        public static int Discrete(double[] a)
        {
            // Entries in a[] must sum to 1.
            double r = StdRandom.random();
            double sum = 0.0;

            for (int i = 0; i < a.Length; i++)
            {
                sum = sum + a[i];
                if (sum >= r) return i;
            }

            return -1;
        }

        /// <summary>
        /// randomly shuffle the
        /// elements in an array of
        /// double values
        /// (See Exercise 1.1.36)
        /// </summary>
        /// <param name="a"></param>
        public static void Shuffle(double[] a)
        {
            int N = a.Length;
            for (int i = 0; i < N; i++)
            {
                // Exchange a[i] with random element in a[i..N-1]
                int r = i + Uniform(N - i);
                double temp = a[i];
                a[i] = a[r];
                a[r] = temp;
            }
        }
    }

    public static class SQRTS
    {
        public static double ClassicNewton1(double x)
        {
            double current = x;
            while (Math.Abs((current * current) - x) > 0.000000000001)
            {
                current = (current + (x / current)) / 2.0;
            }
            return current;
        }
    }
}
