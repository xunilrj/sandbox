using System;
using System.Runtime.Serialization;
using System.Threading;

namespace Algorithms.UnionFind.Memory
{
    [Serializable]
    public class MemoryReadWrite<T> : IRandomAccess<T>, IMemoryReport, ISerializable
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

        public MemoryReadWrite(SerializationInfo info, StreamingContext ctxt)
        {
            Array = (T[])info.GetValue(nameof(Array), typeof(int[]));
        }

        void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue(nameof(Array), Array);
        }
    }
}
