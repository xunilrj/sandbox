using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace PrincetonAlgorithms.Memory
{
    class MemoryReadWrite<T> : IRandomAccess<T>, IMemoryReport
    {
        T[] Array;

        long _Reads = 0;
        long _Writes = 0;

        public long Reads { get { return _Reads; } }
        public long Writes { get { return _Writes; } }

        public long Size { get; private set; }

        public MemoryReadWrite(int size)
        {
            Array = new T[size];
            Size = size;
        }

        public T Read(int index)
        {
            Interlocked.Increment(ref _Reads);
            return Array[index];
        }

        public void Write(int index, T item)
        {
            Interlocked.Increment(ref _Writes);
            Array[index] = item;
        }

        public void Reset()
        {
            _Reads = 0;
            _Writes = 0;
        }
    }
}
