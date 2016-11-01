using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Linq.Expressions;
using System.Collections.Generic;

namespace Algebra
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var m1 = new MathNaturalNumber(3);
            var m2 = new MathNaturalNumber(2);
            
            var algebra = new Algebra();
            algebra.Map((a, b) => a + b,
                (MathNaturalNumber a, MathNaturalNumber b) => MathNaturalNumber.Sum(a, b));

            var op = algebra.Get((a, b) => a + b);

            var result = op.Run(m1, m2) as MathNaturalNumber;
            Assert.AreEqual(5, result.Value);
        }
    }

    public class AlgebraObject
    {
        public static AlgebraObject operator +(AlgebraObject l, AlgebraObject r)
        {
            throw new NotImplementedException();
        }
    }

    public class AlgebraOperation
    {
        Expression Expression;
        Algebra Algebra;

        public AlgebraOperation(Expression exp, Algebra algebra)
        {
            Expression = exp;
            Algebra = algebra;
        }

        internal object Run(params object[] args)
        {
            var visitor = new BinaryOperatorVisitor(args[0], args[1], Algebra);

            var exp = visitor.Visit(Expression);
            return (exp as ConstantExpression).Value;
        }
    }

    public class BinaryOperatorVisitor : ExpressionVisitor
    {
        Algebra Algebra;
        object Left;
        object Right;

        public BinaryOperatorVisitor(object left, object right, Algebra algebra)
        {
            Left = left;
            Right = right;
            Algebra = algebra;
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            if (node.NodeType == ExpressionType.Add)
            {
                //var left = (node.Left as ConstantExpression).Value;
                //var right = (node.Right as ConstantExpression).Value;

                var realMethod = (Func<object, object, object>)Algebra.GetMap(Left.GetType(), Right.GetType(), node.NodeType);

                var newValue = realMethod(Left, Right);

                return Expression.Constant(newValue);
            }

            throw new NotImplementedException();
        }

        protected override Expression VisitConstant(ConstantExpression node)
        {
            return base.VisitConstant(node);
        }

        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            var r = base.Visit(node.Body);
            return r;
        }
    }

    public class Algebra
    {
        public AlgebraOperation Get(Expression<Func<AlgebraObject, AlgebraObject, AlgebraObject>> exp)
        {
            var operation = new AlgebraOperation(exp, this);

            return operation;
        }

        Dictionary<Tuple<Type, Type, ExpressionType>, object> Maps = new Dictionary<Tuple<Type, Type, ExpressionType>, object>();

        public void Map<T1, T2, T3>(Expression<Func<AlgebraObject, AlgebraObject, AlgebraObject>> operation, Func<T1, T2, T3> real)
        {
            var nodeType = ((operation as LambdaExpression).Body as BinaryExpression).NodeType;
            Maps.Add(
                Tuple.Create(typeof(T1), typeof(T2), nodeType),
                new Func<object, object, object>(
                    (object a, object b) => real((T1)a, (T2)b)));
        }

        internal object GetMap(Type type1, Type type2, ExpressionType nodeType)
        {
            return Maps[Tuple.Create(type1, type2, nodeType)];
        }
    }

    public class MathNaturalNumber
    {
        public int Value { get; private set; }

        public MathNaturalNumber(int value)
        {
            Value = value;
        }

        public static MathNaturalNumber Sum(MathNaturalNumber l, MathNaturalNumber r)
        {
            return new MathNaturalNumber(l.Value + r.Value);
        }
    }
}
