using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace pdf
{
    public class Pdf 
    {
        StreamWriter W;

        public Stream Stream { get; }
               
        public Pdf()
        {
            Stream = new MemoryStream();
        }


        public Task StartAsync()
        {
            W = new StreamWriter(Stream, Encoding.ASCII);
            return Task.CompletedTask;
        }

        public Task WriteHeader()
        {
            return W.WriteAsync(@"%PDF-1.4");
        }

        public Task WriteCatalog()
        {
            return W.WriteAsync(@"1 0 obj
<</Type /Catalog
/Pages 2 0 R
>>
endobj");
        }

        public Task WritePageTree()
        {
            return W.WriteAsync(@"2 0 obj
<</Type /Pages
/Kids [3 0 R]
/Count 1
>>
endobj");
        }

        public Task WritePage()
        {
            return W.WriteAsync(@"3 0 obj
<</Type /Page
/Parent 2 0 R
/MediaBox [0 0 500 500]
/Contents 5 0 R
/Resources <</ProcSet [/PDF /Text]
/Font <</F1 4 0 R>>
>>
>>
endobj");
        }

        public Task WriteFont()
        {
            return W.WriteAsync(@"4 0 obj
<</Type /Font
/Subtype /Type1
/Name /F1
/BaseFont /Helvetica
/Encoding /MacRomanEncoding
>>
endobj");
        }

        public Task WritePageContent()
        {
            return W.WriteAsync(@"5 0 obj
<</Length 53
>>
stream
BT
/F1 20 Tf
120 120 Td
(Hello from Steve) Tj
ET
endstream
endobj");
        }

        public Task WriteXRef()
        {
            return W.WriteAsync(@"xref
0 6
0000000000 65535 f
0000000009 00000 n
0000000063 00000 n
0000000124 00000 n
0000000277 00000 n
0000000392 00000 n");
        }

        public Task WriteTrailer()
        {
            return W.WriteAsync(@"trailer
<</Size 6
/Root 1 0 R
>>");
        }

        public Task WriteEOF()
        {
            return W.WriteAsync(@"startxref
502");
        }

    }
}
