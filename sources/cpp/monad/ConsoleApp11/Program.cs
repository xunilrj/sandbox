using System;
using System.Linq;
using System.Threading.Tasks;

namespace ConsoleApp11
{
    class Program
    {
        static void Main(string[] argss)
        {
            var args = new Enumerable<string, Wrapper<string>>(argss);
            var fw = args.First();
            
            Console.WriteLine("Hello World!");
        }
    }

    public interface INoneToken { }
    public class NoneToken : INoneToken
    {
        public static NoneToken None
        {
            get { return new NoneToken(); }
        }
    }

    public class Wrapper<T>
    {
        public static implicit operator Wrapper<T>(T t) => new Wrapper<T>(t);
        public static implicit operator Wrapper<T>(NoneToken t) => new Wrapper<T>(new NullReferenceException());
        public static implicit operator NoneToken(Wrapper<T> w) => NoneToken.None;

        Exception Error;
        T Value;

        public Wrapper(T value)
        {
            Value = value;
            Error = null;
        }

        public Wrapper(Exception error)
        {
            Error = error;
            Value = default(T);
        }
    }

    public class Enumerable<T, M>
        where M : T, INoneToken
    {
        System.Collections.Generic.IEnumerable<T> Value;
        public Enumerable(System.Collections.Generic.IEnumerable<T> v) => Value = v;

        public M First()
        {
            if (Value.Count() > 0)
            {
                return (M)Value.First();
            }
            else
            {
                return NoneToken.None;
            }
        }
    }
}
