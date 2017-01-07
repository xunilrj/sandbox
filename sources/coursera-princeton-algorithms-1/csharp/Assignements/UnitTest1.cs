using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

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
}
