use std::str::FromStr;

use proc_macro2::{TokenStream, TokenTree, Delimiter, Ident};
use quote::quote;


// TODO handle if cases, while, loop, etc.

#[proc_macro]
pub fn unclassified_line(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    quote!(compile_error!("UNCLASSIFIED LINE")).into()
}

/// Designates the start of the fortran instructions, from this point on
/// the code will be rewritten to use gotos
#[proc_macro]
pub fn fortran_body(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    quote!(compile_error!("fortran_body!() must be used inside a function with #[rewrite_fortran_goto]")).into()
}

/// Designates a label in the code, the label must be unique
#[proc_macro]
pub fn fortran_label(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    quote!(compile_error!("fortran_label!() must be used inside a function with #[rewrite_fortran_goto] and after fortran_body!()")).into()
}

/// Go to a label
#[proc_macro]
pub fn fortran_goto(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    quote!(compile_error!("fortran_goto!() must be used inside a function with #[rewrite_fortran_goto] and after fortran_body!()")).into()
}

#[proc_macro_attribute]
pub fn fortran_function(_attrs: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let f: TokenStream = rewrite_fortran_goto(proc_macro::TokenStream::new(), input).into();
    // wrap the body in an unsafe block
    let mut result = TokenStream::new();
    for token in f.into_iter() {
        match &token {
            TokenTree::Group(group) => {
                if group.delimiter() == Delimiter::Brace {
                    let body = group.stream();
                    result.extend(quote! { { unsafe { #body } } });
                } else {
                    result.extend(quote! { #token });
                }
            }
            _ => {
                result.extend(quote! { #token });
            }
        }
    }
    result.into()
}

#[proc_macro_attribute]
pub fn rewrite_fortran_goto(_attrs: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();

    // TODO check if the input is a function

    let mut result = TokenStream::new();
    for token in input.into_iter() {
        match &token {
            TokenTree::Group(group) => {
                if group.delimiter() == Delimiter::Brace {
                    let stream = group.stream().into_iter().collect::<Vec<_>>();

                    // the first part of the stream has to be kept as is.
                    // after the fortran_body!() macro, the rest of the stream
                    // has to be rewritten
                    let mut group_content = TokenStream::new();
                    let mut i = 0;
                    while i < stream.len() {
                        if let Some(n) = parse_fortran_body(stream[i..].iter().cloned()) {
                            i = i + n;
                            group_content.extend(rewrite_fortran_goto_impl(&Ctx::root(), stream[i..].iter().cloned()));
                            break;
                        } else {
                            let t = stream[i].clone();
                            group_content.extend(quote! { #t });
                            i += 1;
                        }
                    }

                    result.extend(quote! { { #group_content } });
                    
                    //result.extend(rewrite_fortran_goto_impl(&Ctx::root(), group.stream()));
                } else {
                    result.extend(quote! { #token });
                }
            }
            _ => {
                result.extend(quote! { #token });
            }
        }
    }
    result.into()
}

fn rewrite_fortran_goto_impl(
    parent: &Ctx,
    stream: impl IntoIterator<Item = TokenTree> + Clone,
) -> TokenStream {
    let ctx = Ctx::new(parent, stream.clone());
    if ctx.labels.is_empty() {
        let mut result = TokenStream::new();
        let tokens = stream.into_iter().collect::<Vec<_>>();
        let mut i = 0;
        while i < tokens.len() {
            if let Some((label, n)) = parse_goto(tokens[i..].iter().cloned()) {
                result.extend(ctx.make_goto(label));
                i += n;
            } else {
                result.extend(rewrite_token(&ctx, &tokens[i]));
                i += 1;
            }
        }
        result
    } else {
        let mut result = TokenStream::new();
        result.extend(make_preamble(&ctx));
        let tokens = stream.into_iter().collect::<Vec<_>>();
        {
            let current_enum = enum_name(&ctx);
            let loop_name = format!("_fortran_goto_loop_{}", ctx.level());
            let set_name = format!("_fortran_goto_set_{}", ctx.level());
            let loop_label = TokenStream::from_str(&format!("'{}", loop_name)).unwrap();
            let set_label = TokenStream::from_str(&format!("'{}", set_name)).unwrap();
            let mut match_body = TokenStream::new();
            let mut i = 0;
            let mut label_name = "Start".to_string();
            let mut content = TokenStream::new();
            while i < tokens.len() {
                if let Some((label, n)) = parse_label(tokens[i..].iter().cloned()) {
                    // push to match body: label_name => { group }

                    // label name as identifier
                    let c = TokenTree::Ident(Ident::new(&label_name, proc_macro2::Span::call_site()));
                    match_body.extend(quote! {
                        #current_enum::#c => { #content #current_enum::#c.next() }
                    });
                    label_name = format!("Label{}", label);
                    content = TokenStream::new();
                    i += n;
                } else {
                    if let Some((label, n)) = parse_goto(tokens[i..].iter().cloned()) {
                        content.extend(ctx.make_goto(label));
                        i += n;
                    } else {
                        content.extend(rewrite_token(&ctx, &tokens[i]));
                        i += 1;
                    }
                    //content.extend(rewrite_token(&ctx, &tokens[i]));
                    //i += 1;
                }
            }
            let c = TokenTree::Ident(Ident::new(&label_name, proc_macro2::Span::call_site()));
            match_body.extend(quote! {
                #current_enum::#c => { #content break #loop_label; }
            });
            result.extend(quote! {
                #loop_label: loop {
                    state = #set_label: {
                        match state {
                            #match_body
                        }
                    };
                }
            });
        }
        result
    }
}

fn rewrite_token(
    ctx: &Ctx,
    token: &TokenTree,
) -> TokenStream {
    match token {
        TokenTree::Group(group) => {
            if group.delimiter() == Delimiter::Brace {
                //panic!("{:?}", group.to_string());
                let mut result = TokenStream::new();
                let content = rewrite_fortran_goto_impl(ctx, group.stream());
                //panic!("{:?}", content.to_string());
                result.extend(quote! { { #content } });
                result
            } else {
                let mut result = TokenStream::new();
                result.extend(quote! { #token });
                result
            }
        }
        _ => {
            let mut result = TokenStream::new();
            result.extend(quote! { #token });
            result
        }
    }
}

fn enum_name(
    ctx: &Ctx,
) -> Ident {
    let enum_name = format!("FortranGotoState{}", ctx.level());
    Ident::new(&enum_name, proc_macro2::Span::call_site())
}

fn make_preamble(
    ctx: &Ctx,
) -> TokenStream {
    let enum_name = enum_name(ctx);

    let enum_variants = ctx.labels.iter().map(|label| {
        let label_name = format!("Label{}", label);
        let label_name = Ident::new(&label_name, proc_macro2::Span::call_site());
        quote! {
            #label_name,
        }
    }).collect::<Vec<_>>();

    let first_label_variant = ctx.labels.first().map(|label| {
        let label_name = format!("Label{}", label);
        Ident::new(&label_name, proc_macro2::Span::call_site())
    }).unwrap();
    let last_label_variant = ctx.labels.last().map(|label| {
        let label_name = format!("Label{}", label);
        Ident::new(&label_name, proc_macro2::Span::call_site())
    }).unwrap();

    let nexts = ctx.labels.iter().zip(ctx.labels.iter().skip(1)).map(|(label, next)| {
        let label_name = format!("Label{}", label);
        let label_name = Ident::new(&label_name, proc_macro2::Span::call_site());
        let next_name = format!("Label{}", next);
        let next_name = Ident::new(&next_name, proc_macro2::Span::call_site());
        quote! {
            #enum_name::#label_name => #enum_name::#next_name,
        }
    }).collect::<Vec<_>>();

    quote! {
        enum #enum_name {
            Start,
            #(#enum_variants)*
        }

        impl #enum_name {
            fn next(&self) -> Self {
                match self {
                    #enum_name::Start => #enum_name::#first_label_variant,
                    #(#nexts)*
                    #enum_name::#last_label_variant => unreachable!(),
                }
            }
        }

        let mut state = #enum_name::Start;
    }
}

// tries to parse a label from the stream,
// returns the label and the number of tokens consumed
//
// Labels are in the form: fortran_label!(123)
fn parse_label(
    tokens: impl Iterator<Item = TokenTree>,
) -> Option<(i32, usize)> {
    let mut tokens = tokens.peekable();
    if let Some(TokenTree::Ident(ident)) = tokens.next() {
        if ident.to_string().len() > 5 {
            //panic!("{:?}", ident.to_string());
        }
        if ident.to_string() == "fortran_label" {
            if let Some(TokenTree::Punct(punct)) = tokens.next() {
                if punct.as_char() == '!' {
                    if let Some(TokenTree::Group(group)) = tokens.next() {
                        if group.delimiter() == Delimiter::Parenthesis {
                            let mut tokens = group.stream().into_iter().peekable();
                            if let Some(TokenTree::Literal(literal)) = tokens.next() {
                                if let Ok(label) = literal.to_string().parse::<i32>() {
                                    if let None = tokens.next() {
                                        return Some((label, 4));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

// fortran_goto!(123);
fn parse_goto(
    tokens: impl Iterator<Item = TokenTree>,
) -> Option<(i32, usize)> {
    let mut tokens = tokens.peekable();
    //panic!("AAA");
    if let Some(TokenTree::Ident(ident)) = tokens.next() {
        if ident.to_string() == "fortran_goto" {
            if let Some(TokenTree::Punct(punct)) = tokens.next() {
                if punct.as_char() == '!' {
                    if let Some(TokenTree::Group(group)) = tokens.next() {
                        if group.delimiter() == Delimiter::Parenthesis {
                            let mut tokens = group.stream().into_iter().peekable();
                            if let Some(TokenTree::Literal(literal)) = tokens.next() {
                                if let Ok(label) = literal.to_string().parse::<i32>() {
                                    if let None = tokens.next() {
                                        return Some((label, 4));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

// fortran_body!();
fn parse_fortran_body(
    tokens: impl Iterator<Item = TokenTree>,
) -> Option<usize> {
    let mut tokens = tokens.peekable();
    let r = if let Some(TokenTree::Ident(ident)) = tokens.next() {
        if ident.to_string() == "fortran_body" {
            if let Some(TokenTree::Punct(punct)) = tokens.next() {
                if punct.as_char() == '!' {
                    if let Some(TokenTree::Group(_)) = tokens.next() {
                        Some(3)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };
    if let Some(r) = r {
        // add one if followed by a semicolon
        if let Some(TokenTree::Punct(punct)) = tokens.peek() {
            if punct.as_char() == ';' {
                return Some(r + 1);
            }
        }
    }
    r
}

struct Ctx<'a> {
    parent: Option<&'a Ctx<'a>>,
    labels: Vec<i32>,
}

impl<'a> Ctx<'a> {
    fn root() -> Self {
        Self {
            parent: None,
            labels: Vec::new(),
        }
    }

    fn new(parent: &'a Ctx<'a>, stream: impl IntoIterator<Item = TokenTree>) -> Self {
        let mut labels = Vec::new();
        // TODO optimize this
        let stream = stream.into_iter().collect::<Vec<_>>();
        {
            let mut i = 0;
            while i < stream.len() {
                if let Some((label, n)) = parse_label(stream[i..].iter().cloned()) {
                    labels.push(label);
                    i += n;
                } else {
                    i += 1;
                }
            }
        }
        

        Self {
            parent: Some(parent),
            labels,
        }
    }

    fn level(&self) -> usize {
        if let Some(parent) = self.parent {
            parent.level() + 1
        } else {
            0
        }
    }

    fn make_goto(&self, label: i32) -> TokenStream {
        let has_label = self.labels.iter().any(|l| *l == label);
        if has_label {
            let set_name = format!("_fortran_goto_set_{}", self.level());
            let set_label = TokenStream::from_str(&format!("'{}", set_name)).unwrap();
            let label_name = format!("Label{}", label);
            let label_name = Ident::new(&label_name, proc_macro2::Span::call_site());
            let enum_name = enum_name(self);
            quote! {
                break #set_label #enum_name::#label_name;
            }
        } else {
            if let Some(parent) = self.parent {
                parent.make_goto(label)
            } else {
                panic!("Label {} not found", label);
            }
        }
    }
}
