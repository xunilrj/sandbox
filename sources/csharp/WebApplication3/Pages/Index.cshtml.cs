using Microsoft.AspNetCore.Html;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.AspNetCore.Mvc.ViewComponents;
using Microsoft.AspNetCore.Razor.TagHelpers;

namespace WebApplication3.Pages
{
    public class IndexPageModel : PageModel
    {
        public void OnGet()
        {
        }

        public IActionResult OnPostMethod1Async()
        {
            return RedirectToPage();
        }
    }


    [ViewComponent(Name = "Button")]
    public class ButtonViewComponent : ViewComponent
    {
        public IViewComponentResult Invoke(string eventName)
        {
            return new HtmlContentViewComponentResult(new HtmlString($"<button onclick=\"" +
                $"var newEvent = new CustomEvent('{eventName}', {{ detail: 1, bubbles: true }});\n" +
                $"event.target.dispatchEvent(newEvent);\n" +
                $"return false;\">button text</button>"));
        }
    }
}

namespace WebApplication3.TagHelpers
{
    public class NodeTagHelper : TagHelper
    {
        public override void Process(TagHelperContext context, TagHelperOutput output)
        {
            output.TagName = "div";
            output.PostElement.AppendHtml(new HtmlString("<script>document.currentScript.previousSibling.addEventListener('increase', function (e) { console.log('send to server'); }, false);</script>"));
        }
    }
}
