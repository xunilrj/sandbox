using SixLabors.Fonts;
using System;
using System.IO;
using System.Threading.Tasks;

namespace pdf
{
    class Program
    {
        //https://blog.idrsolutions.com/2013/01/understanding-the-pdf-file-format-overview/#helloworld
        //https://blog.idrsolutions.com/2010/09/grow-your-own-pdf-file-part-1-pdf-objects-and-data-types/
        //https://blog.idrsolutions.com/2010/09/grow-your-own-pdf-file-part-2-structure-of-a-pdf-file/
        //https://blog.idrsolutions.com/2010/10/grow-your-own-pdf-part-2b-create-your-own-non-working-pdf/
        //https://blog.idrsolutions.com/2010/10/grow-your-own-pdf-file-%E2%80%93-part-3-diy-blank-page/

        //https://resources.infosecinstitute.com/pdf-file-format-basic-structure/

        //http://www.printmyfolders.com/understanding-pdf
        //http://www.printmyfolders.com/Home/understanding-the-portable-document-format-pdf-part-2

        //https://www.adobe.com/content/dam/acom/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
        static async Task Main(string[] args)
        {
            using (var pdf = new Pdf())
            {
                var collection = new FontCollection();
                var arialFamily = collection.Install("C:/Windows/Fonts/arial.ttf");
                var arial = new Font(arialFamily, 20);

                var str = "Hello World";
                float w = 0;
                foreach(var c in str)
                {
                    var glyph = arial.GetGlyph((int)c);
                    w += glyph.Instance.AdvanceWidth / (72.0f / 96.0f);
                }
                //var shapes = SixLabors.Shapes.Temp.TextBuilder.GenerateGlyphs(text, new Primitives.PointF(50f, 4f), new RendererOptions(font, 72));


                var tree = await pdf.AddPageTree();
                var page = tree.AddPage(await pdf.AddPage());
                var font = await pdf.AddFont();
                page.Content = await pdf.AddStream();
                page.Content.BeginText()
                    .Raw("/P << /MCID 0 >>")
                    .Raw("BDC")
                    .TextFont(font, 20)
                    .Raw("1 2 3 4 5 6 Tm")
                    //.TextLeading(20)
                    //.TextPosition(0, 822)
                    .TextPrint(@"Something which is extremely hot, in any sense. Hot weather, sexual arousal, one who is wanted by the police, etc. are all described as ""hotter than..."" or "" as hot as a fresh fucked fox in a forest fire")
                    //.TextNewLine()
                    .TextPosition((int)w, 0)
                    .TextPrint("Again!")
                    .Raw("EMC")
                .EndText();
                var page2 = tree.AddPage(await pdf.AddPage());
                page2.Content = await pdf.AddStream();
                page2.Content.BeginText()
                   .TextFont(font, 20)
                   .TextPosition(0, 0)
                   .TextPrint("Hello World2!")
               .EndText();
                await pdf.StartAsync();

                await pdf.WriteHeader();
                await pdf.WriteCatalog();

                await pdf.WriteXRef();
                await pdf.WriteTrailer();
                await pdf.WriteEOF();

                using (var f = File.OpenWrite("./a.pdf"))
                {
                    pdf.Stream.Position = 0;
                    await pdf.Stream.CopyToAsync(f);
                    await f.FlushAsync();
                }
            }
        }
    }
}
