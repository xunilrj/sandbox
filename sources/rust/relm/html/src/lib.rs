use proc_macro::TokenStream;
use quote::*;
use std::collections::HashMap;
use syn::{
    parse::Parse, parse::ParseStream, token::Brace, token::Div, token::Gt, token::Lt, token::Token,
    Ident, *,
};

mod parsers;
use crate::parsers::*;

#[derive(Debug)]
enum HtmlElementContent {
    None,
    Expression(Expr),
    String(LitStr),
    Children(Vec<HtmlElement>),
}

#[derive(Debug)]
enum HtmlAttributeContent {
    None,
    Expression(Expr),
    String(LitStr),
}

#[derive(Debug)]
struct HtmlElement {
    attributes: HashMap<String, HtmlAttributeContent>,
    name: Ident,
    content: HtmlElementContent,
}

impl HtmlElement {
    fn quote_by_event_name(k: &str, expr: &Expr) -> (proc_macro2::TokenStream, String) {
        if let Expr::Closure(c) = expr {
            match k {
                "onclick" => (
                    quote! {let message = MessageFactory::OnClick(Box::new(#c))},
                    "=\"send({});\"".to_string(),
                ),
                "oninput" => (
                    quote! {let message = MessageFactory::OnInput(Box::new(#c))},
                    "=\"var e = arguments[0];send({}, [e.data]);\"".to_string(),
                ),
                _ => panic!("unkown event"),
            }
        } else {
            (
                quote! {let message = MessageFactory::Message(#expr)},
                "=\"send({});\"".to_string(),
            )
        }
    }

    fn quote_attrs(&self, q: &mut proc_macro2::TokenStream) {
        // println!("Quoting Attributes for {}", self.name);
        for (k, v) in &self.attributes {
            // println!("Attribute {}", k);
            q.extend(quote! {html.push_str(#k);});
            match v {
                HtmlAttributeContent::None => {}
                HtmlAttributeContent::Expression(expr) => {
                    // dbg!(&expr);
                    let is_eventcallback = k.starts_with("on");
                    if is_eventcallback {
                        let (message_init, event_html) =
                            Self::quote_by_event_name(k.as_str(), expr);
                        q.extend(quote! {
                            {
                                #message_init;
                                let id = messages.len();
                                messages.push(message);
                                html.push_str(&format!(#event_html, id));
                            }
                        });
                    } else {
                        q.extend(quote! {
                            {
                                let v = &(#expr);
                                html.push_str(&format!("=\"{}\" ", &v));
                            }
                        });
                    }
                }
                HtmlAttributeContent::String(s) => {
                    q.extend(quote! {
                        html.push_str(&format!("=\"{}\" ", #s));
                    });
                }
            }
        }
    }

    pub fn quote(&self) -> proc_macro2::TokenStream {
        let open_html = format!("<{} ", self.name);
        let close_html = format!("</{}>", self.name);

        let mut q = quote! {};

        match &self.content {
            HtmlElementContent::None => {
                q.extend(quote! {html.push_str(#open_html);});
                self.quote_attrs(&mut q);
                q.extend(quote! {html.push_str("/>");});

                q.into()
            }
            HtmlElementContent::Children(children) => {
                if children.len() > 0 {
                    let close_html = format!("</{}>", self.name);

                    q.extend(quote! {html.push_str(#open_html);});
                    self.quote_attrs(&mut q);
                    q.extend(quote! {html.push_str("/>");});

                    for child in children {
                        q.extend(child.quote());
                    }

                    q.extend(quote! {html.push_str(#close_html);});
                    q.into()
                } else {
                    let html = format!("<{} />", self.name);
                    q.extend(quote! {#html});
                    q.into()
                }
            }
            HtmlElementContent::Expression(expr) => {
                q.extend(quote! {html.push_str(#open_html);});
                self.quote_attrs(&mut q);
                q.extend(quote! {html.push_str("/>");});

                q.extend(quote! {
                    {
                        let v = #expr;
                        html.push_str(&format!("{}", v));
                    }
                });

                q.extend(quote! {html.push_str(#close_html);});

                q.into()
            }
            HtmlElementContent::String(s) => {
                q.extend(quote! {html.push_str(#open_html);});
                self.quote_attrs(&mut q);
                q.extend(quote! {html.push_str("/>");});
                q.extend(quote! {html.push_str(#s);});
                q.extend(quote! {html.push_str(#close_html);});

                q.into()
            }
        }
    }
}

fn open_element(stream: &mut ParseStream) -> Option<HtmlElement> {
    if let Ok((_, name, _, _)) = parsers::parse_seq4::<Lt, Ident, Div, Gt>(stream) {
        return Some(HtmlElement {
            name,
            content: HtmlElementContent::None,
            attributes: HashMap::new(),
        });
    }

    if let Ok((_, name)) = parsers::parse_seq2::<Lt, Ident>(stream) {
        let mut element = HtmlElement {
            name,
            content: HtmlElementContent::None,
            attributes: HashMap::new(),
        };

        // we now can have
        // end the open tag
        // end of the tag (todo)
        // or attributes

        if stream.peek(Gt) {
            stream.parse::<Gt>().unwrap();
        } else {
            while let Ok(attr_name) = parsers::parse_seq1::<Ident>(stream) {
                let attr_name = format!("{}", attr_name);

                // println!("Found Attribute {}", attr_name);
                element
                    .attributes
                    .entry(attr_name.clone())
                    .or_insert(HtmlAttributeContent::None);

                if stream.peek(token::Eq) {
                    stream.parse::<token::Eq>().unwrap();
                    if let Ok(block) = parsers::parse_seq1::<Block>(stream) {
                        if let Stmt::Expr(expr) = block.stmts.get(0).unwrap() {
                            element.attributes.entry(attr_name).and_modify(|v| {
                                *v = HtmlAttributeContent::Expression(expr.clone())
                            });
                        }
                    } else if let Ok(s) = parsers::parse_seq1::<LitStr>(stream) {
                        element
                            .attributes
                            .entry(attr_name)
                            .and_modify(|v| *v = HtmlAttributeContent::String(s.clone()));
                    } else {
                        panic!("mal formed attribute")
                    }
                } else {
                    panic!("mal formed attribute")
                }
            }

            // We can now have
            // end the open tag
            // end of the tag (todo)
            if stream.peek(Gt) {
                stream.parse::<token::Gt>().unwrap();
            }
            if stream.peek(Div) {
                stream.parse::<token::Div>().unwrap();
                stream.parse::<token::Gt>().unwrap();

                return Some(element);
            }
        }

        // element content

        if stream.peek(token::Brace) {
            if let Ok(block) = parsers::parse_seq1::<Block>(stream) {
                if let Stmt::Expr(expr) = block.stmts.get(0).unwrap() {
                    element.content = HtmlElementContent::Expression(expr.clone());
                }
            }
        } else if stream.peek(token::Lt) {
            let mut children = Vec::new();
            while let Some(child) = open_element(stream) {
                children.push(child);
            }
            element.content = HtmlElementContent::Children(children)
        } else {
            if let Ok(s) = parsers::parse_seq1::<LitStr>(stream) {
                element.content = HtmlElementContent::String(s);
            }
        }

        // close element

        let _ = parsers::parse_seq4::<Lt, Div, Ident, Gt>(stream);

        return Some(element);
    }

    None
}

#[derive(Debug)]
struct HtmlDom {
    children: Vec<HtmlElement>,
}

impl HtmlDom {
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
        }
    }

    pub fn append(&mut self, element: HtmlElement) {
        self.children.push(element);
    }

    pub fn quote(&self) -> proc_macro2::TokenStream {
        let mut tokens = quote! {
            let mut html = String::new();
            let mut messages = Vec::new();
        };

        for children in &self.children {
            tokens.extend(children.quote());
        }

        tokens.extend(quote! {
            (html, messages)
        });

        tokens.into()
    }
}

impl Parse for HtmlDom {
    fn parse(mut stream: ParseStream) -> Result<Self> {
        let mut dom = HtmlDom::new();

        if let Some(element) = open_element(&mut stream) {
            dom.append(element);
        }

        Ok(dom)
    }
}

#[proc_macro]
pub fn html(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as HtmlDom);
    // dbg!(&parsed);
    parsed.quote().into()
}
