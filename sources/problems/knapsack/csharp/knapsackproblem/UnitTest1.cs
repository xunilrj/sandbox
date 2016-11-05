using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;

namespace knapsackproblem
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod, Timeout(30 * 60 * 1000)]
        [Ignore]
        public void TestMethod1()
        {
            int max = 32;

            Console.WriteLine($"C,Size,Time");
            for (int c = 0; c < max; c++)
            {
                var C = (int)Math.Pow(1.4, c);
                //var C = 1024;

                //for (int n = 0; n < max; n++)
                {
                    var n = max;
                    var size = (int)Math.Pow(1.4, n);
                    var w = new int[size];
                    var v = new int[size];

                    var result = Profile.Time(() =>
                    {
                        Knapsack.Solve(C, w, v);
                    });

                    Console.WriteLine($"{C},{size},{result}");
                }
            }

        }

        [TestMethod]
        public void MinimunCoinsTest()
        {
            var coins = new int[] { 100, 50, 25, 10, 5, 1 };

            var result = MinimunCoins.Solve(0, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 0, 0);

            result = MinimunCoins.Solve(1, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 0, 1);

            result = MinimunCoins.Solve(2, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 0, 2);

            result = MinimunCoins.Solve(4, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 0, 4);

            result = MinimunCoins.Solve(5, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 1, 0);

            result = MinimunCoins.Solve(6, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 1, 1);

            result = MinimunCoins.Solve(9, coins);
            AssertEnumerable(result, 0, 0, 0, 0, 1, 4);

            result = MinimunCoins.Solve(99, coins);
            AssertEnumerable(result, 0, 1, 1, 2, 0, 4);
        }

        [TestMethod]
        public void MinimunCoinsTest2()
        {
            var coins = new int[] { 100, 50, 25, 10, 5, 1 };

            var parameters = new CoinsParameters(100, 50, 25, 10, 5, 1);

            CoinsSolutions result;
            var coinsSolver = new CoinsSolver();
            //var result = coinsSolver.Solve(0, parameters);
            //Assert.AreEqual(0, result.Count);
            //Assert.IsNull(result.Previous);

            //result = coinsSolver.Solve(1, parameters);
            //Assert.AreEqual(1, result.Count);
            //Assert.AreEqual(0, result.Previous.Count);

            result = coinsSolver.Solve(4, parameters);
            Assert.AreEqual(1, result.Coin);
            Assert.AreEqual(1, result.Previous.Coin);
            Assert.AreEqual(1, result.Previous.Previous.Coin);
            Assert.AreEqual(1, result.Previous.Previous.Previous.Coin);
            Assert.AreEqual(0, result.Previous.Previous.Previous.Previous.Coin);

            result = coinsSolver.Solve(5, parameters);
            Assert.AreEqual(5, result.Coin);
            Assert.AreEqual(0, result.Previous.Coin);

            var result1 = GetCoins(4, coins);
            var result2 = GetCoins(5, coins);

        }

        int[] GetCoins(int value, params int[] coins)
        {
            if (value < 0)
            {
                return null;
            }
            else if (value == 0)
            {
                return new int[coins.Length];
            }

            var bestSolution = coins.Select((x, i) =>
            {
                if(x > value)
                {
                    return null;
                }

                var subSolution = GetCoins(value - x, coins);

                if (subSolution == null)
                {
                    return null;
                }
                else
                {
                    var currentSolution = (int[])subSolution.Clone();
                    currentSolution[i]++;

                    return currentSolution;
                }
            })
            .Where(x => x != null)
            .MaxElement((current, max) => max.Sum().CompareTo(current.Sum()));

            return bestSolution;
        }

        void AssertEnumerable(IEnumerable<int> actual, params int[] expected)
        {
            Assert.AreEqual(expected.Length, actual.Count());
            var result = expected.Zip(actual, Tuple.Create).All(x => x.Item1 == x.Item2);
            Assert.IsTrue(result);
        }

        [TestMethod]
        public void SkipPreserveIndex()
        {
            var a = new int[] { 4, 5, 6 };
            a
                .SkipWhile(x => x == 4)
                .Select((v, i) => { Console.WriteLine($"{i} {v}"); return 0; })
                .ToArray();
        }
    }

    public static class Profile
    {
        public static double Time(Action run)
        {
            var stopwatch = Stopwatch.StartNew();

            run();

            stopwatch.Stop();
            return stopwatch.ElapsedMilliseconds;
        }
    }

    public static class MinimunCoins
    {
        public static T MaxElement<T>(this IEnumerable<T> source, Func<T, T, int> compare)
        {
            using (var iterator = source.GetEnumerator())
            {
                if (!iterator.MoveNext())
                {
                    throw new InvalidOperationException("Empty sequence");
                }

                T maxElement = iterator.Current;

                while (iterator.MoveNext())
                {
                    T element = iterator.Current;
                    if (compare(element, maxElement) > 0)
                    {
                        maxElement = element;
                    }
                }

                return maxElement;
            }
        }

        public static T MinElement<T>(this IEnumerable<T> source, Func<T, T, int> compare)
        {
            using (var iterator = source.GetEnumerator())
            {
                if (!iterator.MoveNext())
                {
                    throw new InvalidOperationException("Empty sequence");
                }

                T maxElement = iterator.Current;

                while (iterator.MoveNext())
                {
                    T element = iterator.Current;
                    if (compare(element, maxElement) < 0)
                    {
                        maxElement = element;
                    }
                }

                return maxElement;
            }
        }

        public static IEnumerable<int> Solve(int total, int[] coins)
        {
            var memory = new Dictionary<int, int[]>();

            for (int i = 0; i <= total; i++)
            {
                if (i == 0)
                {
                    memory.Add(i, new int[coins.Length]);
                }
                else
                {
                    var solutions = coins.Select((coinValue, coinIndex) =>
                    {
                        int[] lastSolution = null;
                        if (memory.TryGetValue(i - coinValue, out lastSolution))
                        {
                            var previousSolution = (int[])memory[i - coinValue].Clone();
                            previousSolution[coinIndex]++;
                            return new { Quantity = previousSolution.Sum(), Solution = previousSolution };
                        }
                        else
                        {
                            return new { Quantity = int.MaxValue, Solution = (int[])null };
                        }
                    });
                    var bestSolution = solutions.MinElement((current, max) =>
                    {
                        return current.Quantity.CompareTo(max.Quantity);
                    });

                    memory.Add(i, bestSolution.Solution);
                }
            }

            return memory[total];
        }
    }

    public class CoinsParameters : BaseParamaters<int>
    {
        public int[] Coins { get; set; }

        public CoinsParameters(params int[] coins)
        {
            Coins = coins;
        }

        public override IEnumerable<int> GetItems()
        {
            return Coins;
        }
    }

    public class CoinsSolutions : BaseSolution<CoinsSolutions>
    {
        public int Coin { get; }

        public CoinsSolutions(int count)
        {
            Coin = count;
        }
    }

    public class CoinsSolver : DynamicProgrammingTemplate<CoinsParameters, CoinsSolutions>
    {
        protected override int Compare(CoinsSolutions l, CoinsSolutions max)
        {
            return l.Coin.CompareTo(max.Coin);
        }

        protected override CoinsSolutions Compute(int current, int candidate)
        {
            var previousSolution = Retrieve(current - candidate);

            if (previousSolution != null)
            {
                return new CoinsSolutions(candidate)
                {
                    Previous = previousSolution
                };
            }
            else
            {
                return null;
            }
        }

        protected override void ComputeBase()
        {
            Save(0, new CoinsSolutions(0));
        }
    }

    public abstract class BaseParamaters<TItem>
    {
        public abstract IEnumerable<TItem> GetItems();
    }

    public abstract class BaseSolution<T>
    {
        public T Previous { get; set; }
    }

    public abstract class DynamicProgrammingTemplate<TParameters, TSolution>
        where TParameters : BaseParamaters<int>
        where TSolution : BaseSolution<TSolution>
    {
        Dictionary<int, TSolution> Solutions = new Dictionary<int, TSolution>();

        protected abstract void ComputeBase();
        protected abstract TSolution Compute(int value, int candidate);
        protected abstract int Compare(TSolution l, TSolution r);

        protected TSolution Retrieve(int value)
        {
            TSolution solution = null;
            if (Solutions.TryGetValue(value, out solution))
            {
                return solution;
            }

            return null;
        }

        protected void Save(int current, TSolution solution)
        {
            if (Solutions.ContainsKey(current) == false)
            {
                Solutions.Add(current, solution);
            }
        }

        public TSolution Solve(int target, TParameters parameters)
        {
            ComputeBase();

            for (int i = 0; i <= target; i++)
            {
                if (Solutions.ContainsKey(i) == false)
                {
                    var best = parameters.GetItems()
                        .Select(x => Compute(i, x))
                        .Where(x => x != null)
                        .MaxElement((a, max) => Compare(a, max));

                    Save(i, best);
                }
            }

            return Solutions[target];
        }
    }

    public static class Knapsack
    {
        public static int Solve(int capacity, int[] weights, int[] values)
        {
            var memory = new int[capacity + 1, values.Length + 1];
            var l = weights.Length;

            for (int c = 0; c <= capacity; c++)
            {
                for (int w = 0; w < weights.Length; w++)
                {
                    if (c == 0)
                    {
                        memory[c, w] = 0;
                        //Console.WriteLine($"m[{c},{w}] = { memory[c, w] }");
                    }
                    else
                    {
                        var lastValue = c - weights[w];

                        if (lastValue < 0)
                        {
                            memory[c, w] = 0;
                            //Console.WriteLine($"m[{c},{w}] = { memory[c, w] }");
                        }
                        else
                        {
                            memory[c, w] = memory[lastValue, l] + values[w];
                            //Console.WriteLine($"m[{c},{w}] = { memory[c, w] }");
                        }
                    }
                }

                memory[c, l] = memory.EnumerateDimension(1, c).Max();
                //Console.WriteLine($"m[{c},BEST] = { memory[c, l] }");
            }

            return memory[capacity, l];
        }

        public static IEnumerable<T> EnumerateDimension<T>(this T[,] table, int dimension, int value)
        {
            var rows = table.GetLength(dimension);

            for (int i = 0; i < rows; i++)
            {
                yield return table[value, i];
            }
        }
    }
}
