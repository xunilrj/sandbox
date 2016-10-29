using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MicrosoftResearch.Infer.Models;
using MicrosoftResearch.Infer;
using MicrosoftResearch.Infer.Distributions;
using System.Linq;
using Accord.Statistics.Distributions.Univariate;
using System.Collections.Generic;
using Accord.Statistics.Visualizations;
using DotImaging.Primitives2D;
using DotImaging;
using Accord.IO;
using Accord.Statistics;

namespace LemonStand.Simulation
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void UsingInferNet()
        {
            var arrivals = Variable
                .GaussianFromMeanAndVariance(600, 30)
                .Named("arrivals");

            var profits = Variable.New<double>();
            var discreteProfit = (Variable.Discrete(0.3, 0.3, 0.4));
            using (Variable.Case(discreteProfit, 0)) { profits.SetTo(Variable.Constant(5.0)); }
            using (Variable.Case(discreteProfit, 1)) { profits.SetTo(Variable.Constant(3.5)); }
            using (Variable.Case(discreteProfit, 2)) { profits.SetTo(Variable.Constant(4.0)); }

            var tips = Variable.New<double>();
            var discreteTips = Variable.Discrete(0.5, 0.2, 0.2, 0.1);
            using (Variable.Case(discreteTips, 0)) { tips.SetTo(Variable.Constant(0.0)); }
            using (Variable.Case(discreteTips, 1)) { tips.SetTo(Variable.Constant(0.25)); }
            using (Variable.Case(discreteTips, 2)) { tips.SetTo(Variable.Constant(1.0)); }
            using (Variable.Case(discreteTips, 3)) { tips.SetTo(Variable.Constant(2.0)); }

            var net = (arrivals * profits) + (arrivals * tips);

            var inferenceEngine = new InferenceEngine();            var result = inferenceEngine.Infer<Gaussian>(net);            var variance = result.GetVariance();            Console.WriteLine($"Mean: {result.GetMean().ToString("N2")}");
            Console.WriteLine($"Variance: {variance.ToString("N2")}");
            Console.WriteLine($"StdDev: {Math.Sqrt(variance).ToString("N2")}");

            //var samples = Enumerable.Range(0, 10000).Select(x => net.ra).ToArray();

            //var hist = new Histogram();
            //hist = hist.FromData(samples);

            //foreach (var item in hist.Bins)
            //{
            //    Console.WriteLine(new string('*', item.Value / 5));
            //}
        }

        [TestMethod]
        public void UsingAccord()
        {
            var arrivals = NormalDistribution.Random(600, 30, 10000);
            var profits = GeneralDiscreteDistribution.Random(new[] { 0.3, 0.3, 0.4 }, 10000)
                .Select(x =>
                {
                    switch (x)
                    {
                        case 0: return 5.0;
                        case 1: return 3.5;
                        default: return 4.0;
                    }
                });
            var tips = GeneralDiscreteDistribution.Random(new[] { 0.5, 0.2, 0.2, 0.1 }, 10000)
                .Select(x =>
                 {
                     switch (x)
                     {
                         case 0: return 0.0;
                         case 1: return 0.25;
                         case 2: return 1.0;
                         default: return 2.0;
                     }
                 });

            var net = Linq.Zip(arrivals, profits, tips, (a, p, t) =>
            {
                return (a * p) + (a * t);
            }).ToArray();

            var hist = new Histogram();
            hist = hist.FromData(net);

            Console.WriteLine($"Mean: {net.Mean().ToString("N2")}");
            Console.WriteLine($"Variance: {net.Variance().ToString("N2")}");
            Console.WriteLine($"StdDev: {net.StandardDeviation().ToString("N2")}");

            foreach (var item in hist.Bins)
            {
                Console.WriteLine(new string('*', item.Value / 5));
            }
        }
    }

    public static class Linq
    {
        public static IEnumerable<TResult> Zip<TA, TB, TC, TResult>(IEnumerable<TA> @as, IEnumerable<TB> @bs, IEnumerable<TC> @cs, Func<TA, TB, TC, TResult> apply)
        {
            return @as.Zip(bs, Tuple.Create).Zip(cs, Tuple.Create)
                .Select(x => apply(x.Item1.Item1, x.Item1.Item2, x.Item2));
        }
    }
}
