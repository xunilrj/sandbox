using System;

namespace MachinaAurum.Core
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }

    public interface IValueWrapper<T>
    {
        T Value { get; }
    }

    public interface IConditionalNoValueWrapper<TOk>
    {
    }

    public static class ConditionalNoValueWrapperExtensions
    {
        public static TR Map<T, TR>(this IConditionalNoValueWrapper<T> item, Func<T, TR> ok, Func<TR> nok = null)
        {
            if (item is IValueWrapper<T> wrapper)
            {
                var value = wrapper.Value;
                if (value != null) return ok(value);
                else return nok();
            }
            else
            {
                return nok();
            }
        }
    }

    public interface IConditionalValueWrapper<TOk, TNok>
    {
    }

    public static class ConditionalValueWrapperExtensions
    {
        public static TR Map<TOk, TNok, TR>(this IConditionalValueWrapper<TOk, TNok> item, Func<T, TR> ok, Func<TR> nok = null)
        {
            if (item is IValueWrapper<T> wrapper)
            {
                var value = wrapper.Value;
                if (value != null) return ok(value);
                else return nok();
            }
            else
            {
                return nok();
            }
        }
    }

}
