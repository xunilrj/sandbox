using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Exercise113
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            var num1 = int.Parse(args[0]);
            var num2 = int.Parse(args[1]);
            var num3 = int.Parse(args[2]);

            Console.WriteLine((num1 == num2) && (num2 == num3));
        }
    }
}
