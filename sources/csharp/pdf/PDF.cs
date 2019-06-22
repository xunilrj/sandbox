using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace pdf
{
    public class PdfStream : PdfObject
    {
        public PdfPage Page { get; set; }

        public Stream Stream { get; }

        StreamWriter W;
        public PdfStream()
        {
            Stream = new MemoryStream();
            W = new StreamWriter(Stream, Encoding.ASCII);
            W.AutoFlush = true;
        }

        public async Task AddValue (string str)
        {
            await W.WriteLineAsync(str);
        }

        public override async Task WriteTo(int i, StreamWriter w)
        {
            Stream.Position = 0;
            await w.WriteLineAsync($@"<</Length {Stream.Length}>> stream");
            await Stream.CopyToAsync(w.BaseStream);
            await w.WriteLineAsync($@"endstream");
            Stream.Position = Stream.Length - 1;
        }

        public PdfStream Raw(string raw) => WriteLine(raw);

        public PdfStream BeginText() => WriteLine("BT");
        public PdfStream EndText() => WriteLine("ET");
        public PdfStream TextFont(PdfFont font, int size)
        {
            var pos = Page.GetResource(font);
            return WriteLine($"/F{pos} {size} Tf");
        }
        public PdfStream TextPosition(int x, int y) => WriteLine($"{x} {y} Td");
        public PdfStream TextPrint(string str) => WriteLine($"({str}) Tj");
        public PdfStream TextNewLine() => WriteLine($"T*");
        public PdfStream TextLeading(int leading) => WriteLine($"{leading} TL");



        PdfStream WriteLine(string str)
        {
            W.WriteLine(str);
            return this;
        }
    }

    public class PdfFont : PdfObject
    {
        public PdfFont()
        {
            FontName = "Arial";
        }

        public string FontName { get; set; }
        
        public override async Task WriteTo(int i, StreamWriter w)
        {
            await w.WriteLineAsync($@"<</Type /Font
/Subtype /Type1
/Name /F1
/BaseFont /{FontName}
/Encoding /MacRomanEncoding
>>");
        }
    }

    public class PdfPage : PdfObject
    {
        PdfStream content;
        public PdfStream Content
        {
            get => content;
            set
            {
                content = value;
                content.Page = this;
            }
        }

        public int Width {get;set;}
        public int Height {get;set;}

        public PdfPage()
        {
            Width = 595;
            Height = 842;
        }


        public override async Task WriteTo(int i, StreamWriter w)
        {
            await w.WriteLineAsync($@"<</Type /Page
/Parent 2 0 R
/MediaBox [0 0 {Width} {Height}]
/Contents {Content.Index} 0 R
/Resources <</ProcSet [/PDF /Text] /Font <<");

            var fonts = Resources.Where(x => x.Key is PdfFont);
            int fi = 1;
            foreach(var f in fonts)
            {
                await w.WriteAsync($"/F{fi} {f.Key.Index} 0 R");
                ++fi;
            }
            await w.WriteAsync(">> >> >>");
        }

        Dictionary<PdfObject, int> Resources = new Dictionary<PdfObject, int>();

        public int GetResource(PdfObject obj)
        {
            if (Resources.TryGetValue(obj, out var i))
                return i;
            Resources.Add(obj, Resources.Count + 1);
            return Resources.Count;
        }
    }

    public class PdfPageTree : PdfObject
    {
        List<PdfPage> Pages;

        public PdfPageTree()
        {
            Pages = new List<PdfPage>();
        }

        public override async Task WriteTo(int i, StreamWriter w)
        {
            await w.WriteAsync($@"<</Type /Pages /Kids [");
            foreach(var p in Pages)
            {
                await w.WriteAsync($"{p.Index} 0 R ");
            }
            await w.WriteAsync($"] /Count {Pages.Count} >>");
        }

        public PdfPage AddPage(PdfPage page)
        {
            Pages.Add(page);
            return page;
        }
    }

    public abstract class PdfObject
    {
        public int Index { get; set; }
        public abstract Task WriteTo(int i, StreamWriter w);
    }

    public class Pdf : IDisposable 
    {
        const int istart = 2;
        StreamWriter W;

        List<PdfObject> Objects { get; }
        Dictionary<string, object> Trailer { get; }

        public Pdf()
        {
            Trailer = new Dictionary<string, object>();
            Objects = new List<PdfObject>();
        }

        public void AddObject(PdfObject obj)
        {
            obj.Index = Objects.Count + istart;
            Objects.Add(obj);
        }

        public Stream Stream { get; private set; }
               
        public Task StartAsync()
        {
            Stream = new MemoryStream();
            W = new StreamWriter(Stream, Encoding.ASCII);
            W.AutoFlush = true;
            return Task.CompletedTask;
        }

        public Task WriteHeader()
        {
            return W.WriteLineAsync("%PDF-1.4");
        }

        public Task WriteCatalog()
        {
            return W.WriteLineAsync(@"1 0 obj
<</Type /Catalog
/Pages 2 0 R
>>
endobj");
        }

        private long xrefpos = 0;

        public async Task WriteXRef()
        {
            var pos = new List<long>();
            int i = istart;
            foreach (var obj in Objects)
            {
                pos.Add(Stream.Position);
                await W.WriteLineAsync($"{i} 0 obj");
                await obj.WriteTo(i, W);
                await W.WriteLineAsync("endobj");

                ++i;
            }

            xrefpos = Stream.Position;
            await W.WriteLineAsync($@"xref
0 {istart + pos.Count}
0000000000 65535 f
0000000009 00000 n
0000000063 00000 n"
);

            foreach(var p in pos)
            {
                await W.WriteLineAsync($"{p:D10} 00000 n");
            }
        }


        public async Task WriteTrailer()
        {
            await W.WriteLineAsync("trailer <<");
            await W.WriteLineAsync($"/Size {istart + Objects.Count}");
            await W.WriteLineAsync("/Root 1 0 R");

            foreach (var kv in Trailer)
            {
                await W.WriteLineAsync($"/{kv.Key} {kv.Value}");

            }
            await W.WriteLineAsync(">>");
        }

        public Task WriteEOF()
        {
            return W.WriteLineAsync($"startxref {xrefpos}");
        }

        #region IDisposable Support
        private bool disposedValue = false;

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                    W.Dispose();
                    Stream.Dispose();

                    W = null;
                    Stream = null;
                }

                disposedValue = true;
            }
        }

        public void Dispose()
        {
            Dispose(true);
        }
        #endregion
    }

    public static class PdfExtensions
    {
        public static async Task<PdfStream> AddStream(this Pdf pdf, string str = null)
        {
            var s = new PdfStream();
            if(!string.IsNullOrEmpty(str))
                await s.AddValue(str);
            pdf.AddObject(s);
            return s;
        }

        public static Task<PdfFont> AddFont(this Pdf pdf, string fontName = null)
        {
            var font = new PdfFont();
            if(!string.IsNullOrEmpty(fontName))
                font.FontName = fontName;
            pdf.AddObject(font);
            return Task.FromResult(font);
        }

        public static Task<PdfPage> AddPage(this Pdf pdf)
        {
            var page = new PdfPage();
            pdf.AddObject(page);
            return Task.FromResult(page);
        }

        public static Task<PdfPageTree> AddPageTree(this Pdf pdf)
        {
            var tree = new PdfPageTree();
            pdf.AddObject(tree);
            return Task.FromResult(tree);
        }
    }
}
