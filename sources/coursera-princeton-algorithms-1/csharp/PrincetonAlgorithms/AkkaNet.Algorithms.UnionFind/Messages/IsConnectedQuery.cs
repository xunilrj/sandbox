using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AkkaNet.Algorithms.UnionFind.Messages
{
    public class IsConnectedQuery
    {
        public int Left { get; set; }
        public int Right { get; set; }

        public IsConnectedQuery(int left, int right)
        {
            Left = left;
            Right = right;
        }
    }
}
