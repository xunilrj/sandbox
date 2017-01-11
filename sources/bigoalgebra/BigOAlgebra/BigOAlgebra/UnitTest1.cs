using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Linq.Expressions;
using System.Collections;
using System.Collections.Generic;

namespace BigOAlgebra
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void O1IsConstant()
        {
            var o1 = new O(1);
            Assert.IsTrue(O.IsO1(o1));
            Assert.IsFalse(O.IsOn(o1));
            Assert.IsFalse(O.IsOnlogn(o1));
        }

        [TestMethod]
        public void SumOfConstantIsConstant()
        {
            var o2 = new O(1) + new O(1);
            Assert.IsTrue(O.IsO1(o2));
            Assert.IsFalse(O.IsOn(o2));
            Assert.IsFalse(O.IsOnlogn(o2));
        }

        [TestMethod]
        public void OnisOn()
        {
            var on = new O(n => n);
            Assert.IsFalse(O.IsO1(on));
            Assert.IsTrue(O.IsOn(on));
            Assert.IsFalse(O.IsOnlogn(on));
        }

        [TestMethod]
        public void SumOfOnisOn()
        {
            var on = new O(n => n) + new O(n => n);
            Assert.IsTrue(O.IsOn(on));
            Assert.IsFalse(O.IsOnlogn(on));
        }

        [TestMethod]
        public void SumOfOnO1isOn()
        {
            var on1 = new O(n => n) + new O(1);
            Assert.IsTrue(O.IsOn(on1));
            Assert.IsFalse(O.IsOnlogn(on1));

            var o1n = new O(1) + new O(n => n);
            Assert.IsTrue(O.IsOn(o1n));
            Assert.IsFalse(O.IsOnlogn(o1n));
        }

        [TestMethod]
        public void OnlognIsnlogn()
        {
            var onlogn = new O(n => n * Math.Log(n));
            Assert.IsTrue(O.IsOnlogn(onlogn));
            Assert.IsFalse(O.IsOn(onlogn));
        }

        [TestMethod]
        public void SumOfOnlognIsnlogn()
        {
            var onlogn = new O(n => n * Math.Log(n)) + new O(n => n * Math.Log(n));
            Assert.IsTrue(O.IsOnlogn(onlogn));
        }
    }

    public class O
    {
        public static bool IsO1(O o)
        {
            var visitor = new IsConstantExpressionVisitor();
            visitor.Visit(o.Exp);

            return visitor.IsConstant;
        }

        public static bool IsOn(O o)
        {
            var visitor = new IsOnExpressionVisitor();
            visitor.Visit(o.Exp);

            return visitor.Result;
        }

        public static bool IsOnlogn(O o)
        {
            var visitor = new IsNLognExpressionVisitor();
            visitor.Visit(o.Exp);

            return visitor.Result;
        }

        dynamic Exp;

        public O(int constant)
        {
            Exp = Get(() => constant);
        }

        public O(Expression<Func<int, int>> exp)
        {
            Exp = exp;
        }

        public O(Expression<Func<int, double>> exp)
        {
            Exp = exp;
        }

        O() { }

        Expression<Func<int>> Get(Expression<Func<int>> _) { return _; }

        public static O operator +(O l, O r)
        {
            if (O.IsO1(l) && O.IsO1(r))
            {
                return new O(1);
            }
            else if (O.IsOn(l) && O.IsOn(r))
            {
                return new O(n => n);
            }
            else if (O.IsOn(l) && O.IsO1(r))
            {
                return new O(n => n);
            }
            else if (O.IsO1(l) && O.IsOn(r))
            {
                return new O(n => n);
            }

            throw new NotImplementedException();
        }
    }

    public class IsConstantExpressionVisitor : ExpressionVisitor
    {
        public bool IsConstant { get { return ResultStack.Peek(); } }

        Stack<bool> ResultStack = new Stack<bool>();

        public override Expression Visit(Expression node)
        {
            return base.Visit(node.Reduce());
        }

        protected override Expression VisitConstant(ConstantExpression node)
        {
            ResultStack.Push(true);
            return base.VisitConstant(node);
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            ResultStack.Push(false);
            return base.VisitParameter(node);
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            if (node.NodeType != ExpressionType.Add)
            {
                ResultStack.Push(false);
                return node;
            }

            Visit(node.Left);
            Visit(node.Right);

            var result = ResultStack.Pop() && ResultStack.Pop();

            ResultStack.Push(true);

            return node;
        }
    }

    public class IsOnExpressionVisitor : ExpressionVisitor
    {
        public bool Result { get { return ResultStack.Peek(); } }

        Stack<bool> ResultStack = new Stack<bool>();

        public override Expression Visit(Expression node)
        {
            var node2 = base.Visit(node);

            if (ResultStack.Count == 0)
            {
                ResultStack.Push(false);
            }

            return node2;
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            ResultStack.Push(true);
            return base.VisitParameter(node);
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            if (node.NodeType == ExpressionType.Multiply)
            {
                var visitor = new ContainsParameter();
                visitor.Visit(node.Left);
                var lc = visitor.Result;
                visitor.Visit(node.Right);
                var rc = visitor.Result;

                if(lc && rc)
                {
                    ResultStack.Push(false);
                    return node;
                }
            }
            else if (node.NodeType != ExpressionType.Add)
            {
                ResultStack.Push(false);
                return node;
            }

            Visit(node.Left);
            Visit(node.Right);

            var result = ResultStack.Pop() && ResultStack.Pop();

            ResultStack.Push(true);

            return node;
        }
    }


    public class IsNLognExpressionVisitor : ExpressionVisitor
    {
        public bool Result { get { return ResultStack.Peek(); } }

        Stack<bool> ResultStack = new Stack<bool>();

        public override Expression Visit(Expression node)
        {
            base.Visit(node);

            if (ResultStack.Count == 0)
            {
                ResultStack.Push(false);
            }

            return node;
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            if (node.NodeType != ExpressionType.Multiply)
            {
                ResultStack.Push(false);
                return node;
            }

            var isn = new IsOnExpressionVisitor();
            isn.Visit(node.Left);

            var containslogn = new ContainsLogNExpressionVisitor();
            containslogn.Visit(node.Right);

            var result = isn.Result && containslogn.Result;

            if (result)
            {
                ResultStack.Push(true);
            }

            return node;
        }
    }

    public class ContainsLogNExpressionVisitor : ExpressionVisitor
    {
        public bool Result { get; set; }

        protected override Expression VisitMethodCall(MethodCallExpression node)
        {
            if (node.Method.Name == "Log") Result = true;
            return base.VisitMethodCall(node);
        }
    }

    public class ContainsParameter : ExpressionVisitor
    {
        public bool Result { get; set; }

        public override Expression Visit(Expression node)
        {
            Result = false;
            return base.Visit(node);
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            Result = true;
            return base.VisitParameter(node);
        }
    }
}
