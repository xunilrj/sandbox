namespace MachinaAurum.CSharp 
{
    public struct Either<L, R>
    {
        private readonly bool IsLeft;
        private readonly L Left;
        private readonly R Right;

        public Either(L v)
        {
            IsLeft = true;
            Left = v;
            Right = default;
        }

        public Either(R v)
        {
            IsLeft = false;
            Left = default;
            Right = v;
        }

        public static implicit operator Either<L, R>(L v) => new Either<L,R>(v);
        public static implicit operator Either<L, R>(R v) => new Either<L,R>(v);
    }
}
