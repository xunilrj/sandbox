namespace MachinaAurum.CSharp 
{
    public struct Unit
    {
        public static Unit Instance => new Unit(null);
        private Unit(object o) { }
    }
}
