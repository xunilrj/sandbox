using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PrincetonAlgorithms.Memory
{
    interface IMemoryReport
    {
        long Reads { get; }
        long Writes { get; }

        void Reset();
    }
}
