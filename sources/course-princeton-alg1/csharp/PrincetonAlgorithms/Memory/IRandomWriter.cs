using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PrincetonAlgorithms.Memory
{
    public interface IRandomWriter<T>
    {
        void Write(int index, T item);
    }
}