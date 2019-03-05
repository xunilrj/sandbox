using System;
using Xunit;

namespace pdf.tests
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            var pdf = new Pdf();
            pdf.StartAsync();
            pdf.WriteHeader();
            pdf.WriteCatalog();
            pdf.WritePageTree();
            pdf.WritePage();
            pdf.WriteFont();
            pdf.WritePageContent();
            pdf.WriteXRef();
            pdf.WriteTrailer();
            pdf.WriteEOF();

        }
    }
}
