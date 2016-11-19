namespace AkkaNet.Algorithms.UnionFind.Messages
{
    public class UnionCommand
    {
        public int Left { get; set; }
        public int Right { get; set; }
                
        public UnionCommand(int left, int right)
        {
            Left = left;
            Right = right;
        }
    }
}
