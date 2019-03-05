using HtmlAgilityPack;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

namespace html2tex
{
    internal enum SectionType
    {
        Chapter,
        Section
    }

    internal class Program
    {
        static Stack<bool> UseMath = new Stack<bool>();
        static Stack<bool> GeneratePar = new Stack<bool>();

        private static StringBuilder sb;
        private static void Main(string[] args)
        {
            var ISBN = "9781787123663";
            string dir = $@"C:\Users\xunil\Downloads\packpub\{ISBN}";

            var tocJson = File.ReadAllText($@"{ dir}\.build\toc.json");
            var toc = JsonConvert.DeserializeObject<JObject>(tocJson);

            var summaryJson = File.ReadAllText($@"{ dir}\.build\summary.json");
            var summary = JsonConvert.DeserializeObject<JObject>(summaryJson);
            var title = summary["title"].Value<string>();
            var publicationDate = DateTime.ParseExact(summary["publicationDate"].Value<string>(),
                "MM/dd/yyyy hh:mm:ss", Thread.CurrentThread.CurrentUICulture);

            var authorsJson = File.ReadAllText($@"{ dir}\.build\authors.json");
            var authors = JsonConvert.DeserializeObject<JArray>(authorsJson);

            sb = new StringBuilder();
            sb.AppendLine(@"\documentclass[10pt,a4paper]{book}");
            sb.AppendLine(@"\usepackage[utf8]{inputenc}");
            sb.AppendLine(@"\usepackage{amsmath}");
            sb.AppendLine(@"\usepackage{amsfonts}");
            sb.AppendLine(@"\usepackage{amssymb}");
            sb.AppendLine(@"\usepackage{graphicx}");
            sb.AppendLine(@"\usepackage[hidelinks]{hyperref}");
            sb.AppendLine(@"\usepackage{pdfpages}");
            sb.AppendLine($@"\title{{{title}}}");
            sb.AppendLine($@"\date{{{publicationDate.ToString("MMMM dd, yyyy")}}}");
            sb.AppendLine(@"\begin{document}");

            sb.AppendLine(@"\pagenumbering{gobble}");
            sb.AppendLine(@"\clearpage");
            sb.AppendLine(@"\includepdf{cover.png}");

            sb.AppendLine(@"\newpage");
            sb.AppendLine(@"\clearpage");
            sb.AppendLine(@"\maketitle");

            sb.AppendLine(@"\newpage");
            sb.AppendLine(@"\clearpage");
            foreach (JObject author in authors)
            {
                sb.AppendLine($@"\noindent \textbf{{{author["author"]}}}\\");
                var description = author["description"].Value<string>();
                var doc = new HtmlDocument();
                doc.LoadHtml(description);
                DumpContent(doc.DocumentNode, skip: 0);
            }

            sb.AppendLine(@"\newpage");
            sb.AppendLine(@"\clearpage");
            sb.AppendLine(@"\tableofcontents");
            sb.AppendLine(@"\newpage");
            sb.AppendLine(@"\pagenumbering{arabic}");
            sb.AppendLine(@"\setcounter{page}{1}");

            var files = Directory.EnumerateFiles($@"{dir}\.build\", "*.html");
            foreach(var file in files)
                Convert(file);

            sb.AppendLine(@"\end{document}");
            var ofile = $@"{dir}\.build\book.tex";
            File.WriteAllText(ofile, sb.ToString());
            Process.Start(
                $@"C:\Program Files\MiKTeX 2.9\miktex\bin\x64\pdflatex.exe", 
                $@"-synctex=1 -interaction=nonstopmode {ofile} --shell-escape -output-directory={dir}\.build\")
                .WaitForExit();
            var finalFile = $"{dir}/{ISBN}.pdf";
            if (File.Exists(finalFile))
                File.Delete(finalFile);
            File.Move($@"{dir}\.build\book.pdf", finalFile);
        }

        private static void Convert(string file)
        {
            var doc = new HtmlDocument();
            doc.Load(file);

            var root = doc.DocumentNode.FirstChild;
            var type = GetSectionType(root);

            if (type == SectionType.Chapter)
            {
                var title = GetChapterTitle(root);
                sb.AppendLine($@"\chapter{{{title}}}");
                DumpContent(root);
            }
            else if (type == SectionType.Section)
            {
                var title = GetSectionTitle(root);
                sb.AppendLine($@"\section{{{title}}}");
                DumpContent(root);
            }
        }

        private static void DumpContent(HtmlNode root, int skip = 1,
            bool skipText = false)
        {
            foreach (var item in root.ChildNodes.Skip(skip))
            {
                if (string.Equals(item.Name, "p", StringComparison.OrdinalIgnoreCase))
                {
                    DumpContent(item, skip: 0);
                    if (!GeneratePar.TryPeek(out var genPar))
                        genPar = true;
                    if(genPar)
                        sb.AppendLine(@"\par");
                }
                else if (string.Equals(item.Name, "#text", StringComparison.OrdinalIgnoreCase))
                {
                    if (!skipText)
                    {
                        var txt = item.InnerText;
                        sb.Append(ToLatexStr(txt));
                    }
                }
                else if (string.Equals(item.Name, "a", StringComparison.OrdinalIgnoreCase)
                    && string.IsNullOrWhiteSpace(item.InnerText))
                {
                    var id = item.Id;
                    sb.Append($"\\hypertarget{{{id}}}\\phantom{{}}");
                }
                else if (string.Equals(item.Name, "a", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "#text", StringComparison.OrdinalIgnoreCase)
                    && item.Attributes.Any(x => x.Name == "linkend"))
                {
                    var target = item.Attributes["linkend"].Value;
                    var txt = ToLatexStr(item.InnerText);
                    sb.Append($@"\hyperref[{target}]{{{txt}}}");
                }
                else if (string.Equals(item.Name, "a", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "#text", StringComparison.OrdinalIgnoreCase)
                    && item.HasClass("ulink"))
                {
                    var src = item.Attributes["href"].Value;
                    var txt = ToLatexStr(item.InnerText);
                    sb.Append($@"\href{{{src}}}{{{txt}}}");
                }
                else if (string.Equals(item.Name, "span", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "strong", StringComparison.OrdinalIgnoreCase))
                {
                    var txt = ToLatexStr(item.InnerText);
                    sb.Append($"\\textbf{{{txt}}}");
                }
                else if (string.Equals(item.Name, "span", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "em", StringComparison.OrdinalIgnoreCase))
                {
                    var txt = ToLatexStr(item.InnerText);
                    sb.Append($"\\emph{{{txt}}}");
                }
                else if (string.Equals(item.Name, "span", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "img", StringComparison.OrdinalIgnoreCase))
                {
                    var src = item.FirstChild.Attributes["src"].Value;
                    sb.Append($@"\includegraphics{{{src}}}");
                }
                else if (string.Equals(item.Name, "code", StringComparison.OrdinalIgnoreCase))
                {
                    var txt = ToLatexStr(item.InnerText);
                    sb.Append($@"\emph{{{txt}}}");
                }
                else if (string.Equals(item.Name, "div", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "ul", StringComparison.OrdinalIgnoreCase))
                {
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{itemize}");
                    var items = item.FirstChild.ChildNodes;
                    foreach (var x in items)
                    {
                        sb.AppendLine($@"\item {{{ToLatexStr(x.InnerText)}}}");
                    }
                    sb.AppendLine(@"\end{itemize}");
                }
                else if (string.Equals(item.Name, "div", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "ol", StringComparison.OrdinalIgnoreCase))
                {
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{itemize}");
                    var items = item.FirstChild.ChildNodes;
                    foreach (var x in items)
                    {
                        sb.AppendLine(@"\item {");
                        DumpContent(x, skip: 0);
                        sb.AppendLine(@"}");
                    }
                    sb.AppendLine(@"\end{itemize}");
                }
                else if (string.Equals(item.Name, "ul", StringComparison.OrdinalIgnoreCase))
                {
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{itemize}");
                    var items = item.FirstChild.ChildNodes;
                    foreach (var x in items)
                    {
                        sb.AppendLine($@"\item {{{ToLatexStr(x.InnerText)}}}");
                    }
                    sb.AppendLine(@"\end{itemize}");
                }
                else if (string.Equals(item.Name, "ol", StringComparison.OrdinalIgnoreCase))
                {
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{itemize}");
                    var items = item.FirstChild.ChildNodes;
                    foreach (var x in items)
                    {
                        sb.AppendLine($@"\item {{{ToLatexStr(x.InnerText)}}}");
                    }
                    sb.AppendLine(@"\end{itemize}");
                }
                else if (string.Equals(item.Name, "div", StringComparison.OrdinalIgnoreCase)
                    && item.HasClass("note"))
                {
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{samepage}");
                    sb.AppendLine(@"\nopagebreak[4]");
                    sb.AppendLine(@"\noindent\makebox[\linewidth]{\rule{\paperwidth}{0.4pt}}");
                    sb.AppendLine(@"\nopagebreak[4]");
                    DumpContent(item, skip: 0);
                    sb.AppendLine(@"\nopagebreak[4]");
                    sb.AppendLine(@"\noindent\makebox[\linewidth]{\rule{\paperwidth}{0.4pt}}");
                    sb.AppendLine(@"\nopagebreak[4]");
                    sb.AppendLine(@"\end{samepage}");
                }
                else if (string.Equals(item.Name, "div", StringComparison.OrdinalIgnoreCase)
                    && item.ChildNodes.Count == 1
                    && string.Equals(item.FirstChild.Name, "img", StringComparison.OrdinalIgnoreCase))
                {
                    var src = item.FirstChild.Attributes["src"].Value;
                    sb.AppendLine();
                    sb.AppendLine($@"\begin{{figure}}[h!]");
                    sb.AppendLine(@"\centering");
                    sb.AppendLine($@"\includegraphics{{{src}}}");
                    sb.AppendLine($@"\end{{figure}}");
                }
                else if (string.Equals(item.Name, "img", StringComparison.OrdinalIgnoreCase))
                {
                    var src = item.Attributes["src"].Value;
                    sb.AppendLine();
                    sb.AppendLine($@"\begin{{figure}}[h!]");
                    sb.AppendLine(@"\centering");
                    sb.AppendLine($@"\includegraphics{{{src}}}");
                    sb.AppendLine($@"\end{{figure}}");
                }
                else if (string.Equals(item.Name, "h3", StringComparison.OrdinalIgnoreCase))
                {
                    var txt = ToLatexStr(item.InnerText);
                    sb.AppendLine();
                    sb.AppendLine($@"\subsubsection{{{txt}}}");
                    DumpContent(item, skip: 0, skipText: true);
                }
                else if (string.Equals(item.Name, "h4", StringComparison.OrdinalIgnoreCase))
                {
                    var txt = ToLatexStr(item.InnerText);
                    sb.AppendLine();
                    sb.AppendLine($@"\subsubsection{{{txt}}}");
                    DumpContent(item, skip: 0, skipText: true);
                }
                else if (string.Equals(item.Name, "div", StringComparison.OrdinalIgnoreCase))
                {
                    DumpContent(item, skip: 0);
                }
                else if (string.Equals(item.Name, "pre", StringComparison.OrdinalIgnoreCase))
                {
                    UseMath.Push(false);
                    sb.AppendLine();
                    sb.AppendLine(@"\begin{verbatim}");
                    sb.AppendLine(ToLatexStr(item.InnerText));
                    sb.AppendLine(@"\end{verbatim}");
                    UseMath.Pop();
                }
                else if (string.Equals(item.Name, "table", StringComparison.OrdinalIgnoreCase))
                {
                    var thead = item.ChildNodes
                        .FirstOrDefault(x => string.Equals(x.Name, "thead", StringComparison.OrdinalIgnoreCase));

                    sb.AppendLine(@"\begin{table}");
                    if (thead != null)
                    {
                        var tr = thead.FirstChild;
                        var ths = tr.ChildNodes
                            .Where(x => string.Equals(x.Name, "th", StringComparison.OrdinalIgnoreCase));
                        var beginformat = ths.Select(x => $@"l");
                        var headers = ths.Select(x => $@"{ToLatexStr(x.InnerText).Trim('\n').Trim(' ')}");
                        sb.AppendLine($@"\begin{{tabular}}{{|{string.Join(" | ", beginformat)}|}}");
                        sb.AppendLine(@"\hline");
                        sb.AppendLine($@"{string.Join(" & ", headers)}\\");
                        sb.AppendLine(@"\hline");
                    }
                    else
                    {
                        var colgroup = item.ChildNodes
                            .FirstOrDefault(x => string.Equals(x.Name, "colgroup", StringComparison.OrdinalIgnoreCase));
                        var beginformat = colgroup.ChildNodes.Select(x => $@"l");
                        sb.AppendLine($@"\begin{{tabular}}{{ {string.Join(" | ", beginformat)} }}");
                        sb.AppendLine(@"\hline");
                    }

                    var tbody = item.ChildNodes
                       .FirstOrDefault(x => string.Equals(x.Name, "tbody", StringComparison.OrdinalIgnoreCase));
                    foreach(var tr in tbody.ChildNodes)
                    {
                        bool putAmperstand = false;
                        foreach(var cell in tr.ChildNodes)
                        {
                            if(putAmperstand)
                                sb.AppendLine("&");
                            GeneratePar.Push(false);
                            DumpContent(cell);
                            GeneratePar.Pop();
                            putAmperstand = true;
                        }
                        sb.AppendLine($@"\\");
                        sb.AppendLine(@"\hline");
                    }
                    sb.AppendLine(@"\end{tabular}");
                    sb.AppendLine(@"\end{table}");
                }
                else if (string.Equals(item.Name, "blockquote", StringComparison.OrdinalIgnoreCase))
                {
                    sb.AppendLine(@"\begin{quote}");
                    DumpContent(item, skip:0);
                    sb.AppendLine(@"\end{quote}");
                }
                else
                {

                }
            }
        }

        static string ToLatexStr(string str)
        {
            if(!UseMath.TryPeek(out var useMath))
                useMath = true;
            return str
                .Replace("&lt;=", useMath ? "$\\le$" : "<=")
                .Replace("&gt;=", useMath ? "$\\ge$" : ">=")
                .Replace("&lt;", useMath ? "$<$" : "<")
                .Replace("&gt;", useMath ? "$>$" : ">")
                .Replace("#", "\\#")
                .Replace("&amp;", "&")
                .Replace("||", useMath ? "$||$" : "||");
        }

        private static string GetChapterTitle(HtmlNode node)
            => node.FirstChild.InnerText;
        private static string GetSectionTitle(HtmlNode node)
            => node.FirstChild.InnerText;

        private static SectionType GetSectionType(HtmlNode node)
        {
            if (node.HasClass("chapter"))
                return SectionType.Chapter;
            else if (node.HasClass("section"))
                return SectionType.Section;

            throw new NotSupportedException();
        }
    }
}
