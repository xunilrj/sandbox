using System;
using System.Collections.Generic;
using System.Text;

namespace ConsoleApplication
{
    public class Program
    {
        public static void Main(string[] args)
        {
            string num1 = args[0];
            string num2 = args[1];

            Console.WriteLine($"{num1} * {num2}");

            var result = KaratsubaMultiplication.Multiply(num1, num2);
            Console.WriteLine(result);
        }
    }

    public static class KaratsubaMultiplication
    {
        public static string Multiply(string l, string r)
        {
            if(l.Length == 1)
            {
                return (int.Parse(l) * int.Parse(r)).ToString();
            }
            else
            {
                var a = l.Substring(0, l.Length/2);
                var b = l.Substring(l.Length, l.Length/2);

                var c = r.Substring(0, l.Length/2);
                var d = r.Substring(l.Length, l.Length/2);

                string ac = Multiply(a,c);
                string ad = Multiply(a,d);
                string bc = Multiply(b,c);
                string bd = Multiply(b,d);

                string adbc = Sum(ad, bc);

                string p1 = $"{ac}{"0".PadLeft(l.Length,'0')}";
                string p2 = $"{adbc}{"0".PadLeft(l.Length/2,'0')}";
                string p3 = bd;

                return Sum(Sum(p1,p2),p3);
            }
        }

        public static string Sum(string l, string r)
        {
            var builder = new List<char>();
            int carriage = 0;
            for(int i = l.Length - 1; i >= 0; ++i)
            {
                var s = int.Parse(l[i].ToString()) + int.Parse(r[i].ToString()) + carriage;
                if(s > 9)
                {
                    carriage = 1;
                    s = s - 10;
                }
                else
                {
                    carriage = 0;
                }

                builder.Add(s.ToString()[0]);
            }            

            var asarray = builder.ToArray();
            Array.Reverse(asarray);

            return new String(asarray);
        }
    }
}
