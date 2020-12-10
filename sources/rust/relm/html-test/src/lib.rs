#[cfg(test)]
mod tests {
    #[derive(Debug, Clone)]
    enum DummyMessage {}

    // #[test]
    // fn HtmlElementAutoClose() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div />
    //     }
    // }

    // #[test]
    // fn HtmlElementEmpty() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div></div>
    //     }

    //     dbg!(&f.html);
    // }

    // #[test]
    // fn HtmlElementWithString() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div>"Test"</div>
    //     }

    //     dbg!(&f.html);
    // }

    // #[test]
    // fn HtmlElementWithOneChild() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div><div></div></div>
    //     }

    //     dbg!(&f.html);
    // }

    // #[test]
    // fn HtmlElementWithIf() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div>{if true { <div></div> }}</div>
    //     }

    //     dbg!(&f.html);
    // }

    // #[test]
    // fn HtmlElementWithIfElse() {
    //     let mut f = runtime::FormatterHtml::<DummyMessage>::new();
    //     html::html! {
    //         <div>{if true { <div></div> } else {<div></div>}}</div>
    //     }

    //     dbg!(&f.html);
    // }

    #[test]
    fn HtmlElementsFromVec() {
        let mut f = runtime::FormatterHtml::<DummyMessage>::new();
        let v = vec![1];

        html::html! {
            {v.map(|x| {<div>{x}</div>})}
        }

        dbg!(&f.html);
    }
}
