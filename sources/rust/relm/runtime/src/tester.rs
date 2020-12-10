#[cfg(target_arch = "x86_64")]
fn pretty_print_node<'a>(
    depth: usize,
    r: &'a ego_tree::NodeRef<'a, scraper::Node>,
    newhtml: &mut String,
) {
    use std::fmt::Write;

    let node = r.value();
    let spaces = " ".repeat(depth * 2);
    if let Some(element) = node.as_element() {
        write!(newhtml, "{}<{}", spaces, element.name.local).unwrap();

        let mut attrs: Vec<_> = element.attrs().collect();
        attrs.sort_by(|(lk, _), (rk, _)| lk.cmp(rk));
        for (k, v) in attrs.iter() {
            write!(newhtml, " {}=\"{}\"", k, v).unwrap();
        }

        writeln!(newhtml, ">").unwrap();
    }
    if let Some(txt) = node.as_text() {
        writeln!(newhtml, "{}{}", spaces, txt.to_string()).unwrap();
    }

    for child in r.children() {
        pretty_print_node(depth + 1, &child, newhtml);
    }

    if let Some(element) = node.as_element() {
        writeln!(newhtml, "{}</{}>", spaces, element.name.local).unwrap();
    }
}

#[cfg(target_arch = "x86_64")]
fn pretty_print_html(doc: &scraper::Html, newhtml: &mut String) {
    let root = doc.tree.root();
    for child in root.children() {
        for child in child.children() {
            pretty_print_node(0, &child, newhtml);
        }
    }
}

#[cfg(target_arch = "x86_64")]
fn parse_and_print(html: &str) -> (scraper::Html, String) {
    let document = scraper::Html::parse_fragment(html);

    let mut newhtml = String::new();
    pretty_print_html(&document, &mut newhtml);

    (document, newhtml)
}

#[cfg(target_arch = "x86_64")]
pub struct Tester<T: Default + crate::UpdatableState> {
    pub state: T,
    messages: Vec<crate::MessageFactory<<T as crate::UpdatableState>::Message>>,
    pub actions: Vec<<T as crate::UpdatableState>::Actions>,
    document: scraper::Html,
    pretty_html: String,
}

#[cfg(target_arch = "x86_64")]
impl<
        TMessage: Clone + std::fmt::Debug,
        T: Default
            + crate::UpdatableState<Message = TMessage>
            + crate::DisplayHtml<Message = TMessage>,
    > Tester<T>
{
    pub fn new() -> Self {
        let state: T = Default::default();

        let mut formatter = crate::FormatterHtml {
            html: "".to_string(),
            messages: Vec::new(),
        };
        <T as crate::DisplayHtml>::fmt(&state, &mut formatter);

        let (document, pretty_html) = parse_and_print(formatter.html.as_str());

        Self {
            state,
            messages: formatter.messages,
            document,
            pretty_html,
            actions: Vec::new(),
        }
    }

    fn get_message_id(&self, selector: &str, idx: usize, event_name: &str) -> Option<usize> {
        let selector = scraper::Selector::parse(selector).unwrap();
        let mut s = self.document.select(&selector);
        let el = s.nth(idx).unwrap();
        let el = el.value();

        if let Some(s) = el.attr(event_name) {
            let i = s.find("send(").unwrap();
            let (_, s) = s.split_at(i);
            if s.starts_with("send(") {
                let mut parameters = s
                    .trim_start_matches("send(")
                    .trim_end_matches(");")
                    .split(",");
                Some(parameters.nth(0).unwrap().parse::<u64>().unwrap() as usize)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn click(&mut self, query: &str, idx: usize) {
        println!("Clicking {:?}", query);

        if let Some(msg_id) = self.get_message_id(query, idx, "onclick") {
            let ps = Vec::new();
            self.send_message(msg_id, ps);
        } else {
            panic!("element not found");
        }
    }

    fn send_message(&mut self, msg: usize, ps: Vec<u64>) {
        let messages = &mut self.messages
            as *mut Vec<crate::MessageFactory<<T as crate::UpdatableState>::Message>>
            as *mut ();

        if let Ok(message) = self.state.build_message(msg, ps, messages) {
            self.actions = self.state.send(&message);
            let html = self.state.render2(messages);
            let (document, new_pretty_html) = parse_and_print(html.as_str());

            let diff = prettydiff::diff_lines(self.pretty_html.as_str(), new_pretty_html.as_str());
            println!("----------------------------------");
            diff.prettytable();
            if self.actions.len() > 0 {
                println!("Actions");
                dbg!(&self.actions);
            }
            println!("----------------------------------");

            self.document = document;
            self.pretty_html = new_pretty_html;
        } else {
            panic!("element not found");
        }
    }

    pub fn input(&mut self, query: &str, idx: usize, text: &str) {
        println!("Sending input {:?} to {:?}", text, query);

        if let Some(msg_id) = self.get_message_id(query, idx, "oninput") {
            let addr = crate::wasm::alloc_str(text);
            let ps = vec![addr.data_ptr() as u64];
            std::mem::forget(addr);

            self.send_message(msg_id, ps);
        } else {
            panic!("element not found");
        }
    }

    pub fn assert_inner_text<F>(&mut self, query: &str, f: F)
    where
        F: Fn(&str) -> bool,
    {
        let selector = scraper::Selector::parse(query).unwrap();
        let mut s = self.document.select(&selector);
        if let Some(node) = s.nth(0) {
            let inner_text: Vec<_> = node.text().collect();
            let inner_text = format!("{}", inner_text.iter().nth(0).unwrap());
            let r = f(inner_text.as_str());
            if !r {
                println!("Failed: {}", inner_text);
            }
            assert!(r);
        } else {
            assert!(false);
        }
    }

    pub fn get_action(&self, id: usize) -> Option<&T::Actions> {
        self.actions.iter().nth(id)
    }

    pub fn handle_action<
        F: Fn(&T::Actions) -> Result<<T as crate::UpdatableState>::Message, ()>,
    >(
        &mut self,
        id: usize,
        f: F,
    ) -> Result<(), u64> {
        let messages = &mut self.messages
            as *mut Vec<crate::MessageFactory<<T as crate::UpdatableState>::Message>>
            as *mut ();
        match self.actions.iter().nth(id) {
            Some(a) => {
                println!("Handling {:?}", a);
                match f(&a) {
                    Ok(message) => {
                        println!("Sending {:?}", message);
                        self.actions = self.state.send(&message);
                        let html = self.state.render2(messages);
                        let (document, new_pretty_html) = parse_and_print(html.as_str());

                        let diff = prettydiff::diff_lines(
                            self.pretty_html.as_str(),
                            new_pretty_html.as_str(),
                        );
                        println!("----------------------------------");
                        diff.prettytable();
                        if self.actions.len() > 0 {
                            println!("Actions");
                            dbg!(&self.actions);
                        }
                        println!("----------------------------------");

                        self.document = document;
                        self.pretty_html = new_pretty_html;

                        Ok(())
                    }
                    _ => Err(0),
                }
            }
            None => Err(0),
        }
    }
}
