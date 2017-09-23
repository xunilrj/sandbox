using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpFeatures
{
    [TestClass]
    public class PatternMatchTests
    {
        [TestMethod]
        public void A()
        {
            object a = 1;
            if (a is int i)
            {
                Assert.AreEqual(1, i);
            }
            else if (a is string s)
            {
            }

            //Variable i leak from its context. This is by design.
            //Else does not leak variable;
            //https://docs.microsoft.com/en-us/dotnet/csharp/pattern-matching
            // Language rules for pattern matching expressions help you avoid 
            // misusing the results of a match expression. In the example above,
            // the variables s, c, and r are only in scope and definitely 
            // assigned when the respective pattern match expressions have true results.
            // If you try to use either variable in another location, your code generates compiler errors.
            //https://github.com/dotnet/roslyn/issues/19129

            i = 2;
            Assert.AreEqual(2, i);

            //s is correctly not accessable
            //Assert.AreEqual("", s);

            Figure circle1 = new Circle();
            switch (circle1)
            {
                case Square s:
                default:
                case null:
                    Assert.Fail();
                    break;
                case Circle c:
                    Assert.IsTrue(true);
                    break;
            }

            switch (circle1)
            {
                default:
                case null:
                    break;
                case Circle c when c.Radius == 0:
                    break;
            }
        }



        public class Figure { }
        public class Circle : Figure
        {
            public int CenterX { get; set; }
            public int CenterY { get; set; }
            public int Radius { get; set; }

            public void Deconstruct(out int cx, out int cy, out int r)
            {
                cx = CenterX;
                cy = CenterY;
                r = Radius;
            }
        }
        public class Square : Figure { }
    }
}
