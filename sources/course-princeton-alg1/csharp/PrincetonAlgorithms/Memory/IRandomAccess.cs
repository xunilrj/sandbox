﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PrincetonAlgorithms.Memory
{
    public interface IRandomAccess<T> : IRandomReader<T>, IRandomWriter<T>, IFiniteMemory
    {
    }
}
