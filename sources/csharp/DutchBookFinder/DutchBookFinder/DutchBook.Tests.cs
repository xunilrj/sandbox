using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace DutchBookFinder
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var options = new[]
            {
                new Bet(){ Odds = 1, Price = 100 },
                new Bet(){ Odds = 3, Price = 50 },
                new Bet(){ Odds = 4, Price = 40 },
                new Bet(){ Odds = 9, Price = 20 }
            };

            var result = DutchBook.IsDutchBookFinder(options);

            Assert.AreEqual(-10, result);
        }
    }

    public class Bet
    {
        public int Odds { get; set; }
        public decimal Price { get; set; }
    }

    public static class DutchBook
    {
        public static decimal IsDutchBookFinder(IEnumerable<Bet> bets)
        {
            var totalPrice = bets.Sum(x => x.Price);
            var pay = bets.Average(x => x.Price * (x.Odds+1));

            return pay - totalPrice;
        }
    }
}
