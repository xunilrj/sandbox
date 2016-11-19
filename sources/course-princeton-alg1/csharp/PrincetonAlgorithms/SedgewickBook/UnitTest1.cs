using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Diagnostics;

namespace SedgewickBook
{
    [TestClass]
    public class Chapter01Exercises
    {
        [TestMethod]
        public void Exercise111()
        {
            Assert.AreEqual(7, (0 + 15) / 2);
            Assert.AreEqual(200.0000002, 2.0e-6 * 100000000.1);
            Assert.IsTrue(true && false || true && true);
        }

        [TestMethod]
        public void Exercise112()
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
        public void Exercise113()
        {
            global::Exercise113.Program.Main(new[] { "1", "1", "1" });
            global::Exercise113.Program.Main(new[] { "1", "2", "1" });
            global::Exercise113.Program.Main(new[] { "1", "1", "2" });
            global::Exercise113.Program.Main(new[] { "2", "1", "2" });
        }

        [TestMethod]
        public void Exercise115()
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
        public static int rank(int key, int[] a)
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
