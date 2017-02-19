using HtmlAgilityPack;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace WpfApplication1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        int index = 0;
        HtmlParser parser;

        public MainWindow()
        {
            InitializeComponent();

            
        }

        private void ShowDocument()
        {
            var art = parser.Articles.Skip(index).First();

            var doc = new FlowDocument();

            doc.Blocks.Add(art.Title);
            doc.Blocks.Add(art.Content);

            RootPanel.Document = doc;
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            index++;
            ShowDocument();
        }

        private void TextBox_LostFocus(object sender, RoutedEventArgs e)
        {
            var text = ((TextBox)sender).Text;
            Task.Factory.StartNew(async () =>
            {
                var client = new HttpClient();
                var html = await client.GetStringAsync(text);

                parser = new HtmlParser();
                parser.Parse(html);

                Dispatcher.Invoke(() =>
                {
                    ShowDocument();
                });
            });
        }
    }

    public class HtmlArticle
    {
        HtmlNode Root;

        public HtmlArticle(HtmlNode root)
        {
            Root = root;
        }

        public Block Title
        {
            get
            {
                var header = Root.SelectNodes("//header").First();
                var text = header.InnerText;

                var p1 = new Paragraph();
                p1.Inlines.Add(text);
                return p1;
            }
        }

        public Block Content
        {
            get
            {
                var doc = new HtmlDocument();
                doc.LoadHtml(Root.OuterHtml);

                var header = doc.DocumentNode.SelectNodes("//header").First();
                var footer = doc.DocumentNode.SelectNodes("//footer").First();

                header?.ParentNode?.RemoveChild(header);
                footer?.ParentNode?.RemoveChild(footer);

                var paragraph = new Paragraph();

                var current = doc.DocumentNode.DescendantNodes();
                foreach (var item in current)
                {
                    if (item.Name == "p")
                    {
                        paragraph.Inlines.Add(item.InnerText);
                    }
                    else if (item.Name == "a")
                    {
                        var hp = new Hyperlink()
                        {
                            NavigateUri = new Uri(item.Attributes["href"].Value),
                        };
                        var run1 = new Run()
                        {
                            Text = item.InnerText
                        };
                        hp.Inlines.Add(run1);


                        paragraph.Inlines.Add(hp);
                    }
                }


                return paragraph;
            }
        }
    }

    public class HtmlParser
    {
        List<HtmlArticle> _Articles = new List<HtmlArticle>();

        internal void Parse(string html)
        {
            var parser = new HtmlAgilityPack.HtmlDocument();
            parser.LoadHtml(html);

            var articles = parser.DocumentNode.SelectNodes("//article");
            foreach (var item in articles)
            {
                _Articles.Add(new HtmlArticle(item));
            }
        }

        public IEnumerable<HtmlArticle> Articles
        {
            get
            {
                return _Articles;
            }
        }
    }
}
