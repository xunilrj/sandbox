use proc_macro::TokenStream;
use quote::*;
use std::collections::HashMap;
use syn::{parse::Parse, parse::ParseStream, token::Div, token::Gt, token::Lt, Ident, *};

mod parsers;

enum HtmlElementClose {
    AutoClose,
    CloseElement,
}

#[derive(Debug, Clone)]
enum HtmlElementContent {
    None,
    Expression(Expr),
    String(LitStr),
    Element(Box<HtmlElement>),
    Children(Vec<HtmlElementContent>),
    If(Expr, Box<HtmlElementContent>, Box<HtmlElementContent>),
    Map(
        Vec<Ident>,
        syn::punctuated::Punctuated<syn::Pat, syn::token::Comma>,
        Box<HtmlElement>,
    ),
}

impl HtmlElementContent {
    pub fn quote(&self, first_close: bool) -> (HtmlElementClose, proc_macro2::TokenStream) {
        let mut q = quote! {};
        match &self {
            HtmlElementContent::None => (HtmlElementClose::AutoClose, q.into()),
            HtmlElementContent::Children(children) => {
                if children.len() > 0 {
                    if first_close {
                        q.extend(quote! {html.push_str(">");});
                    }

                    for child in children {
                        let (_, tokens) = child.quote(false);
                        q.extend(tokens);
                    }

                    (HtmlElementClose::CloseElement, q.into())
                } else {
                    (HtmlElementClose::AutoClose, q.into())
                }
            }
            HtmlElementContent::Expression(expr) => {
                q.extend(quote! {
                    {
                        let v = #expr;
                        html.push_str(&format!("{}", v));
                    }
                });
                (HtmlElementClose::CloseElement, q.into())
            }
            HtmlElementContent::String(s) => {
                q.extend(quote! {html.push_str(#s);});
                (HtmlElementClose::CloseElement, q.into())
            }
            HtmlElementContent::If(cond, t, f) => {
                let (_, tquote) = t.quote(false);
                let (_, fquote) = f.quote(false);

                q.extend(quote! {
                    {
                        if #cond {
                            #tquote
                        } else {
                            #fquote
                        }
                    }
                });

                (HtmlElementClose::CloseElement, q.into())
            }
            HtmlElementContent::Element(e) => {
                if first_close {
                    q.extend(quote! {html.push_str(">");});
                }
                q.extend(e.quote());
                (HtmlElementClose::CloseElement, q.into())
            }
            HtmlElementContent::Map(idents, args, body) => {
                q.extend(quote! { for #args in &});
                for (i, ident) in idents.iter().enumerate() {
                    if i > 0 {
                        q.extend(quote! {.});
                    }
                    q.extend(quote! {#ident});
                }

                let t = body.quote();

                q.extend(quote! {{
                    #t
                }});

                // println!("{}", q);
                (HtmlElementClose::CloseElement, q.into())
            }
        }
    }

    pub fn get_ids(
        &self,
        typ: &syn::Ident,
    ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        let mut qsig = quote! {};
        let mut qcode = quote! {};

        match self {
            HtmlElementContent::Element(e) => {
                let (sig, code) = e.get_ids(typ);
                qsig.extend(sig);
                qcode.extend(code);
            }
            HtmlElementContent::If(_, t, f) => {
                let (sig, code) = t.get_ids(typ);
                qsig.extend(sig);
                qcode.extend(code);

                let (sig, code) = f.get_ids(typ);
                qsig.extend(sig);
                qcode.extend(code);
            }
            HtmlElementContent::Children(children) => {
                for child in children {
                    let (sig, code) = child.get_ids(typ);
                    qsig.extend(sig);
                    qcode.extend(code);
                }
            }
            HtmlElementContent::Map(_, _, e) => {
                let (sig, code) = e.get_ids(typ);
                qsig.extend(sig);
                qcode.extend(code);
            }
            _ => {}
        }

        (qsig.into(), qcode.into())
    }
}

#[derive(Debug, Clone)]
enum HtmlAttributeContent {
    None,
    Expression(Expr),
    String(LitStr),
}

#[derive(Debug, Clone)]
struct HtmlElement {
    attributes: HashMap<String, HtmlAttributeContent>,
    name: Option<Ident>,
    content: HtmlElementContent,
}

impl HtmlElement {
    fn quote_by_event_name(k: &str, expr: &Expr) -> (proc_macro2::TokenStream, String) {
        if let Expr::Closure(c) = expr {
            match k {
                "onclick" => (
                    quote! {let message = MessageFactory::OnClick(Box::new(#c))},
                    "=\"send({},{},{})\"".to_string(),
                ),
                "oninput" => (
                    quote! {let message = MessageFactory::OnInput(Box::new(#c))},
                    "=\"var e=arguments[0];send({},{},{},[e.data])\"".to_string(),
                ),
                _ => panic!("unkown event"),
            }
        } else {
            (
                quote! {let message = MessageFactory::Message(#expr)},
                "=\"send({},{},{});\"".to_string(),
            )
        }
    }

    fn quote_attrs(&self, q: &mut proc_macro2::TokenStream) {
        // //println!("Quoting Attributes for {}", self.name);
        for (k, v) in &self.attributes {
            // //println!("Attribute {}", k);

            let mut cfgq = quote! {};
            if k == "ref" {
                cfgq = quote! {#[cfg(test)]};
            }

            let name = format!(" {}", k);

            let valueq = match v {
                HtmlAttributeContent::None => quote! {},
                HtmlAttributeContent::Expression(expr) => {
                    let mut q = quote! {};

                    let is_eventcallback = k.starts_with("on");
                    if is_eventcallback {
                        let (message_init, event_html) =
                            Self::quote_by_event_name(k.as_str(), expr);
                        q.extend(quote! {
                            {
                                #message_init;
                                let id = messages.len();
                                messages.push(message);
                                html.push_str(&format!(#event_html, f.app, f.actor, id));
                            }
                        });
                    } else {
                        q.extend(quote! {
                            {
                                let v = &(#expr);
                                html.push_str(&format!("=\"{}\"", &v));
                            }
                        });
                    }

                    q
                }
                HtmlAttributeContent::String(s) => {
                    let mut q = quote! {};

                    let all = format!("=\"{}\"", &s.value());
                    q.extend(quote! {
                        html.push_str(#all);
                    });

                    q
                }
            };

            q.extend(quote! {
                #cfgq
                {
                    html.push_str(#name);
                    #valueq
                }
            });
        }
    }

    pub fn quote(&self) -> proc_macro2::TokenStream {
        let mut q = quote! {};

        if let Some(name) = &self.name {
            let open_html = format!("<{}", name);
            q.extend(quote! {html.push_str(#open_html);});
        }

        self.quote_attrs(&mut q);

        let (close, content_quote) = self.content.quote(true);
        q.extend(content_quote);

        match close {
            HtmlElementClose::AutoClose => {
                let close_html = "/>";
                q.extend(quote! {html.push_str(#close_html);});
            }
            HtmlElementClose::CloseElement => {
                if let Some(name) = &self.name {
                    let close_html = format!("</{}>", name);
                    q.extend(quote! {html.push_str(#close_html);});
                }
            }
        }
        q.into()
    }

    pub fn get_ids(
        &self,
        typ: &syn::Ident,
    ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        let mut qsig = quote! {};
        let mut qcode = quote! {};

        for (k, v) in &self.attributes {
            if let HtmlAttributeContent::String(v) = v {
                // ID OR REF
                if k == "id" || k == "ref" {
                    let selector = if k == "id" {
                        format!("#{}", v.value())
                    } else {
                        format!("[ref=\"{}\"]", v.value())
                    };

                    let fnname = syn::Ident::new(
                        &format!("get_{}", v.value()),
                        proc_macro2::Span::call_site(),
                    );

                    qsig.extend(quote! {
                        #[cfg(test)]
                        fn #fnname <'a> (&'a mut self) -> Option<
                            runtime::tester::TesterMutElement<
                                'a,
                                <#typ as runtime::UpdatableState>::Message,
                                #typ>
                            >;
                    });

                    qcode.extend(quote! {
                        #[cfg(test)]
                        fn #fnname <'a> (&'a mut self) -> Option<
                            runtime::tester::TesterMutElement<
                                'a,
                                <#typ as runtime::UpdatableState>::Message,
                                #typ>
                            >
                        {
                            self.get_mut(#selector)
                        }
                    });
                }
                // CLASS
                else if k == "class" {
                    let classes = v.value();
                    let classes = classes.split(" ");
                    for class in classes {
                        let selector = format!(".{}", class);

                        let fnname = syn::Ident::new(
                            &format!("query_class_{}", class),
                            proc_macro2::Span::call_site(),
                        );
                        qsig.extend(quote! {
                            #[cfg(test)]
                            fn #fnname <'a> (&'a mut self) -> Vec<
                                runtime::tester::TesterElement<
                                    'a,
                                    <#typ as runtime::UpdatableState>::Message,
                                    #typ>
                                >;
                        });

                        qcode.extend(quote! {
                            #[cfg(test)]
                            fn #fnname <'a> (&'a mut self) -> Vec<
                                runtime::tester::TesterElement<
                                    'a,
                                    <#typ as runtime::UpdatableState>::Message,
                                    #typ>
                                >
                            {
                                self.get_all_mut(#selector)
                            }
                        });
                    }
                }
            }
        }

        let (sig, code) = self.content.get_ids(typ);
        qsig.extend(sig);
        qcode.extend(code);

        (qsig.into(), qcode.into())
    }
}

fn open_element(stream: &mut ParseStream) -> Option<HtmlElement> {
    //println!("");
    //println!("open_element: {}", &stream);
    //println!("");

    // <Ident />
    if let Ok((_, name, _, _)) = parsers::parse_seq4::<Lt, Ident, Div, Gt>(stream) {
        //println!("<Ident />: {}", &stream);
        return Some(HtmlElement {
            name: Some(name),
            content: HtmlElementContent::None,
            attributes: HashMap::new(),
        });
    }

    // <Ident
    if let Ok((_, name)) = parsers::parse_seq2::<Lt, Ident>(stream) {
        //println!("Found Element {}", name);
        let mut element = HtmlElement {
            name: Some(name),
            content: HtmlElementContent::None,
            attributes: HashMap::new(),
        };

        // we now can have
        // end the open tag
        // end of the tag (todo)
        // or attributes

        if stream.peek(Gt) {
            //println!("Found Element Closing");
            stream.parse::<Gt>().unwrap();
        } else {
            // all attributes
            type OrIdentOrRef = parsers::Or<syn::Ident, syn::token::Ref>;
            while let Ok(attr_name) = parsers::parse_seq1::<OrIdentOrRef>(stream) {
                let attr_name = match attr_name {
                    parsers::Or::A(v) => format!("{}", v),
                    parsers::Or::B(_) => format!("ref"),
                };

                // //println!("Found Attribute {}", attr_name);
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
            // end of the tag
            if stream.peek(Gt) {
                stream.parse::<token::Gt>().unwrap();
            }
            if stream.peek(Div) {
                stream.parse::<token::Div>().unwrap();
                stream.parse::<token::Gt>().unwrap();

                //println!("Auto Close : {}", &stream);
                return Some(element);
            }
        }

        // element content
        //println!("Try reading content: {}", stream);
        if let Ok(mut dom) = parsers::parse_seq1::<HtmlDom>(stream) {
            //println!("Recursing DOM: {:?}", &dom);
            element.content = HtmlElementContent::Children(dom.children_as_element_content());
        } else {
            //println!("No Content found!");
        }

        // close element
        parsers::parse_seq4::<Lt, Div, Ident, Gt>(stream).unwrap();

        //println!("<Ident : {}", &stream);

        return Some(element);
    }

    fn braced_options(stream: &mut ParseStream) -> Result<HtmlElementContent> {
        // println!("braced_options {}", stream);
        if let Ok(expr) = parsers::parse_seq1::<syn::Expr>(stream) {
            Ok(HtmlElementContent::Expression(expr.clone()))
        } else if let Ok((_, cond)) = parsers::parse_seq2::<syn::token::If, syn::Expr>(stream) {
            //println!("Found if <Expr>:  {}", stream);
            let mut dom = parsers::braced::<HtmlDom>(stream).unwrap();
            let t = Box::new(HtmlElementContent::Children(
                dom.children_as_element_content(),
            ));
            let content = if stream.peek(syn::token::Else) {
                let _ = stream.parse::<syn::token::Else>();
                //println!("Found else:  {}", stream);
                let mut else_dom = parsers::braced::<HtmlDom>(stream).unwrap();
                HtmlElementContent::If(
                    cond.clone(),
                    t,
                    Box::new(HtmlElementContent::Children(
                        else_dom.children_as_element_content(),
                    )),
                )
            } else {
                HtmlElementContent::If(cond.clone(), t, Box::new(HtmlElementContent::None))
            };

            //println!("IF: {}", &stream);
            Ok(content)

        // <ident>.<ident>.map(|x| <HtmlElement>)
        } else if (stream.peek(syn::Ident) || stream.peek(syn::token::SelfValue))
            && stream.peek2(syn::token::Dot)
        {
            let mut idents = Vec::new();

            if let Ok((ident, _)) =
                parsers::parse_seq2::<syn::token::SelfValue, syn::token::Dot>(stream)
            {
                idents.push(syn::Ident::new("self", ident.span));
            }

            while let Ok((ident, _)) = parsers::parse_seq2::<syn::Ident, syn::token::Dot>(stream) {
                idents.push(ident);
            }

            if stream.peek(syn::Ident) && stream.peek2(syn::token::Paren) {
                // println!("Ident Paren");
                if let Ok(ident) = parsers::parse_seq1::<syn::Ident>(stream) {
                    let name = format!("{}", ident);
                    // println!("Ident {}", name);
                    if name == "map" {
                        let closure_tokens;
                        syn::parenthesized!(closure_tokens in stream);

                        // Closure Inputs
                        let mut inputs = syn::punctuated::Punctuated::new();
                        parsers::parse_seq1::<Token![|]>(&mut &closure_tokens).unwrap();
                        loop {
                            let value =
                                parsers::parse_seq1::<syn::Pat>(&mut &closure_tokens).unwrap();
                            inputs.push_value(value);
                            if let Ok(_) = parsers::parse_seq1::<Token![|]>(&mut &closure_tokens) {
                                break;
                            }
                            let punct: Token![,] = closure_tokens.parse()?;
                            inputs.push_punct(punct);
                        }

                        // println!("Inputs: {:?}", inputs);
                        // println!("Stream: {}", closure_tokens);

                        let body_tokens;
                        syn::braced!(body_tokens in closure_tokens);
                        let body = open_element(&mut &body_tokens);

                        // println!("Body recognized: {:?}", closure_tokens);
                        Ok(HtmlElementContent::Map(
                            idents,
                            inputs,
                            Box::new(body.unwrap()),
                        ))
                    } else {
                        Err(stream.error("Unkown inside braces"))
                    }
                } else {
                    Err(stream.error("Unkown inside braces"))
                }
            } else {
                Err(stream.error("Unkown inside braces"))
            }
        } else {
            Err(stream.error("Unkown inside braces"))
        }
    }

    // { <Expr> }
    // { if <Expr> { <HtmlElement>* } else { <HtmlElement>* } }
    //println!("Trying braced: {}", &stream);
    if let Ok(content) = parsers::braced_map(stream, braced_options) {
        //println!("Found braced {:?}; Stream: {}", content, &stream);

        let element = HtmlElement {
            name: None,
            content,
            attributes: HashMap::new(),
        };
        return Some(element);
    }

    // "..."
    if let Ok(s) = parsers::parse_seq1::<LitStr>(stream) {
        //println!("Found LitStr: {}", &stream);

        let element = HtmlElement {
            name: None,
            content: HtmlElementContent::String(s),
            attributes: HashMap::new(),
        };
        return Some(element);
    }

    //println!("Don't know what to do: {}", &stream);
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
            f.html = html;
            f.messages = messages;
        });

        tokens.into()
    }

    pub fn children_as_element_content(&mut self) -> Vec<HtmlElementContent> {
        let mut v = vec![HtmlElementContent::None; self.children.len()];
        while let Some(e) = self.children.pop() {
            let idx = self.children.len();
            v[idx] = HtmlElementContent::Element(Box::new(e));
        }
        v
    }

    pub fn get_ids(
        &self,
        typ: &syn::Ident,
    ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        let mut qsig = quote! {};
        let mut qcode = quote! {};

        for children in &self.children {
            let (sig, code) = children.get_ids(typ);

            qsig.extend(sig);
            qcode.extend(code);
        }

        (qsig.into(), qcode.into())
    }
}

impl Parse for HtmlDom {
    fn parse(mut stream: ParseStream) -> Result<Self> {
        //println!("Parsing HtmlDOM");
        let mut dom = HtmlDom::new();

        while let Some(element) = open_element(&mut stream) {
            //println!("HtmlDom: Element found");
            dom.append(element);
        }

        if dom.children.len() == 0 {
            //println!("No DOM found!");
            Err(stream.error("No DOM found!"))
        } else {
            Ok(dom)
        }
    }
}

#[derive(Debug)]
struct HtmlDomFunction {
    children: Vec<HtmlElement>,
}

struct Main {
    typ: syn::Ident,
    dom: HtmlDom,
}

impl Parse for Main {
    fn parse(stream: ParseStream) -> Result<Self> {
        let typ: syn::Ident = stream.parse()?;
        let dom: HtmlDom = stream.parse()?;

        Ok(Self { typ, dom })
    }
}

#[proc_macro]
pub fn html(item: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(item as Main);

    let typ = &parsed.typ;
    let typename = syn::Ident::new(&format!("{}Html", typ), proc_macro2::Span::call_site());

    let code = parsed.dom.quote();
    let (idssigs, idscodes) = parsed.dom.get_ids(typ);

    let mut q = quote! {};
    q.extend(quote! {
        trait #typename {
            #idssigs
        }

        #[cfg(test)]
        impl #typename for runtime::tester::Tester<#typ> {
            #idscodes
        }

        impl runtime::DisplayHtml for #typ {
            type Message = Messages;
            fn fmt(&self, f: &mut runtime::FormatterHtml<Self::Message>)
            {
                #code
            }
        }
    });
    q.into()
}
