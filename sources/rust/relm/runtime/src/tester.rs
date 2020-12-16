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
    let mut tempstr = String::new();

    let root = doc.tree.root();
    for child in root.children() {
        for child in child.children() {
            pretty_print_node(0, &child, &mut tempstr);
        }
    }

    let mut size = 30usize;
    if let Some((terminal_size::Width(w), terminal_size::Height(_))) =
        terminal_size::terminal_size()
    {
        size = ((w - 20) / 2) as usize;
    }

    for line in tempstr.lines() {
        if line.len() > size {
            let (a, b) = line.split_at(size);
            newhtml.push_str(a);
            if b.len() > 0 {
                newhtml.push_str("\n");
                newhtml.push_str(b);
            }
        } else {
            newhtml.push_str(line);
        }
        newhtml.push_str("\n");
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
pub struct TesterMutElement<
    'a,
    TMessage: Clone + crate::MyDebug,
    T: Default + crate::UpdatableState<Message = TMessage> + crate::DisplayHtml<Message = TMessage>,
> {
    t: &'a mut Tester<T>,
    query: String,
}

#[cfg(target_arch = "x86_64")]
impl<
        'a,
        TMessage: Clone + crate::MyDebug,
        T: Default
            + crate::UpdatableState<Message = TMessage>
            + crate::DisplayHtml<Message = TMessage>,
    > TesterMutElement<'a, TMessage, T>
{
    pub fn raise<TEvent>(self, e: TEvent)
    where
        crate::HtmlEvents: From<TEvent>,
    {
        let e: crate::HtmlEvents = e.into();
        match e {
            crate::HtmlEvents::OnInput(crate::OnInput { data }) => {
                self.t.input(self.query.as_str(), 0, data.as_str());
            }
            crate::HtmlEvents::OnClick(_) => {
                self.t.click(self.query.as_str(), 0);
            }
        }
    }
}

#[allow(dead_code)] //TODO
#[cfg(target_arch = "x86_64")]
pub struct TesterElement<
    'a,
    TMessage: Clone + crate::MyDebug,
    T: Default + crate::UpdatableState<Message = TMessage> + crate::DisplayHtml<Message = TMessage>,
> {
    t: &'a Tester<T>,
    query: String,
    idx: usize,
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
        TMessage: Clone + crate::MyDebug,
        T: Default
            + crate::UpdatableState<Message = TMessage>
            + crate::DisplayHtml<Message = TMessage>,
    > Tester<T>
{
    pub fn new() -> Self {
        let state: T = Default::default();

        let mut formatter = crate::FormatterHtml {
            app: 0,
            actor: 0,
            html: "".to_string(),
            messages: Vec::new(),
        };
        <T as crate::DisplayHtml>::fmt(&state, &mut formatter);

        let (document, pretty_html) = parse_and_print(formatter.html.as_str());
        // println!("----------------------------------");
        // println!("{}", pretty_html);
        // println!("----------------------------------");

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
                Some(parameters.nth(2).unwrap().parse::<u64>().unwrap() as usize)
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
            let html = self.state.render2(0, 0, messages);
            let (document, new_pretty_html) = parse_and_print(html.as_str());

            let diff = prettydiff::diff_lines(self.pretty_html.as_str(), new_pretty_html.as_str());

            let print_html = std::env::var("PRINT_HTML").unwrap_or("".to_string());
            if print_html == "true" {
                println!("----------------------------------");
                diff.prettytable();
                if self.actions.len() > 0 {
                    println!("Actions");
                    // dbg!(&self.actions); //TODO
                }
                println!("----------------------------------");
            }

            self.document = document;
            self.pretty_html = new_pretty_html;
        } else {
            panic!(format!("Message not found [{}]", msg));
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

    pub fn query_selector_all(&self, query: &str) -> Vec<&scraper::node::Element> {
        let selector = scraper::Selector::parse(query).unwrap();
        let s = self.document.select(&selector);
        s.map(|x| x.value()).collect()
    }

    pub fn get_action(&self, id: usize) -> Option<&T::Actions> {
        self.actions.iter().nth(id)
    }

    pub fn handle_action<
        F: Fn(&T::Actions) -> Result<<T as crate::UpdatableState>::Message, ()>,
    >(
        &mut self,
        f: F,
    ) -> Result<(), u64> {
        let messages = &mut self.messages
            as *mut Vec<crate::MessageFactory<<T as crate::UpdatableState>::Message>>
            as *mut ();

        for a in &self.actions {
            #[cfg(feature = "derive-debug")]
            {
                println!("Handling {:?}", a);
            }
            match f(&a) {
                Ok(message) => {
                    #[cfg(feature = "derive-debug")]
                    {
                        println!("Sending {:?}", message);
                    }
                    self.actions = self.state.send(&message);
                    let html = self.state.render2(0, 0, messages);
                    let (document, new_pretty_html) = parse_and_print(html.as_str());

                    let diff =
                        prettydiff::diff_lines(self.pretty_html.as_str(), new_pretty_html.as_str());

                    let print_html = std::env::var("PRINT_HTML").unwrap_or("".to_string());
                    if print_html == "true" {
                        println!("----------------------------------");
                        diff.prettytable();
                        if self.actions.len() > 0 {
                            println!("Actions");
                            // dbg!(&self.actions); //TODO
                        }
                        println!("----------------------------------");
                    }

                    self.document = document;
                    self.pretty_html = new_pretty_html;

                    return Ok(());
                }
                _ => continue,
            }
        }

        Err(0)
    }

    pub fn get_mut<'a>(&'a mut self, selector: &str) -> Option<TesterMutElement<'a, TMessage, T>> {
        let selection = self.query_selector_all(selector);
        if selection.len() == 1 {
            Some(TesterMutElement {
                t: self,
                query: selector.to_string(),
            })
        } else if selection.len() == 0 {
            None
        } else {
            panic!("unexpected more than one element selected");
        }
    }

    pub fn get_all_mut<'a>(&'a self, selector: &str) -> Vec<TesterElement<'a, TMessage, T>> {
        let selection = self.query_selector_all(selector);

        let mut v = Vec::new();
        for (i, _) in selection.iter().enumerate() {
            v.push(TesterElement {
                t: self,
                query: selector.to_string(),
                idx: i,
            });
        }
        v
    }
}

pub fn any<T2: Any<T2>>() -> T2 {
    <T2 as Any<T2>>::any()
}

pub trait Any<T> {
    fn any() -> T;
}
impl Any<&str> for &str {
    fn any() -> &'static str {
        "ANY"
    }
}
impl Any<String> for String {
    fn any() -> String {
        "ANY".to_string()
    }
}
impl Any<u32> for u32 {
    fn any() -> u32 {
        0
    }
}

#[cfg(target_arch = "x86_64")]
#[extend::ext(pub)]
impl<T> Vec<T> {
    fn assert_single(&self) -> &T {
        if self.len() > 1 {
            panic!("Vec has more than one item");
        }
        self.get(0).unwrap()
    }
}
