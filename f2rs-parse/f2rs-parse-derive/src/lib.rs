use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse::Parse, punctuated::Punctuated, Token, Stmt};


#[proc_macro_attribute]
pub fn syntax_rule(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut f: syn::ItemFn = syn::parse(item).unwrap();
    let attrs = syn::parse_macro_input!(attr as Attributes);

    let mut doc_string = "\n".to_string();

    if attrs.standards.is_empty() {
        return syn::Error::new_spanned(f, "expected at least one compatible standard").to_compile_error().into();
    } else {
        doc_string += "\nSyntax rule\n";
        doc_string += "\n# Compatible with:\n";
        for s in &attrs.standards {
            let mut line = format!(" - [`{}`]", s.standard.to_string());
            if let Some(rule) = &s.rule {
                line += &format!(" rule _{}_", rule);
            }
            if let Some(number) = &s.number {
                line += &format!(" #{}", number);
            }
            if let Some(clause) = &s.clause {
                line += &format!(" clause {}", clause);
            }
            if let Some(section) = &s.section {
                line += &format!(" §{}", section);
            }
            doc_string += &format!("{}\n", line);
        }
    }

    f.attrs.push(syn::parse_quote!(#[doc = #doc_string]));

    let function_name = &f.sig.ident;

    // add dbg_compatible_with(cfg, ...);
    let mut list = quote!();
    for s in &attrs.standards {
        let s = &s.standard;
        list = quote!(#list #s,);
    }
    //let stmts = f.block.brace_token.
    let content = {
        let mut content = quote!();
        for stmt in f.block.stmts {
            content = quote!(#content #stmt);
        }
        content
    };
    let enclosed: Vec<Stmt> = syn::parse_quote!(
        ({
            #content
        }).named(stringify!(#function_name))
    );
    f.block.stmts = enclosed;
    f.block.stmts.insert(0, syn::parse_quote!(crate::dbg_compatible_with!(cfg, #list);));
    //let function_name = &f.sig.ident;
    //f.block.stmts.insert(0, syn::parse_quote!(
    //    // in debug mode, if RUST_BACKTRACE is set, print the stack trace
    //    if cfg!(debug_assertions) && std::env::var("RUST_BACKTRACE").is_ok() || true {
    //        //eprintln!("Syntax rule {}", stringify!(#function_name));
    //        //eprintln!("Backtrace:\n{:?}", std::backtrace::Backtrace::capture());
    //    }
    //));
    
    quote!(#f).into()
}

/// Example:
/// `(&F18V007r1.into()) => ["a", "b", "c"]`
struct Example {
    args: Vec<syn::Expr>,
    cases: Vec<syn::LitStr>,
}

impl Parse for Example {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let args;
        syn::parenthesized!(args in input);
        let args = Punctuated::<syn::Expr, Token![,]>::parse_terminated(&args)?.into_iter().collect::<Vec<_>>();

        input.parse::<Token![=>]>()?;

        let cases;
        syn::bracketed!(cases in input);
        let cases = Punctuated::<syn::LitStr, Token![,]>::parse_terminated(&cases)?.into_iter().collect::<Vec<_>>();

        Ok(Self {
            args,
            cases,
        })
    }
}

/// Example:
/// `F18V007r1 rule "letter" #601 section "R601" clause 623``
/// where rule, number, section, and clause are optional
struct Standard {
    standard: syn::Ident,
    rule: Option<String>,
    number: Option<String>,
    clause: Option<String>,
    section: Option<String>,
}

impl Parse for Standard {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let standard = input.parse()?;
        let mut rule = None;
        let mut number = None;
        let mut section = None;
        let mut clause = None;
        while !input.is_empty() {
            // stop if ","
            if input.peek(Token![,]) {
                break;
            }

            let lh = input.lookahead1();
            if lh.peek(kw::rule) {
                input.parse::<kw::rule>()?;
                rule = Some(input.parse::<syn::LitStr>()?.value());
            } else if lh.peek(kw::clause) {
                input.parse::<kw::clause>()?;
                clause = Some(input.parse::<syn::LitInt>()?.to_string());
            } else if lh.peek(kw::section) {
                input.parse::<kw::section>()?;
                section = Some(input.parse::<syn::LitStr>()?.value());
            } else if lh.peek(Token![#]) {
                input.parse::<Token![#]>()?;
                number = Some(input.parse::<syn::LitInt>()?.to_string());
            } else {
                return Err(input.error("expected `rule`, `clause`, `section`, or `#`"));
            }
        }
        Ok(Self {
            standard,
            rule,
            number,
            section,
            clause,
        })
    }
}

struct Attributes {
    standards: Vec<Standard>,
}

impl Parse for Attributes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            standards: Punctuated::<Standard, Token![,]>::parse_terminated(&input)?.into_iter().collect::<Vec<_>>(),
        })
    }
}

mod kw {
    syn::custom_keyword!(section);
    syn::custom_keyword!(rule);
    syn::custom_keyword!(clause);
    syn::custom_keyword!(non_examples);
    syn::custom_keyword!(examples);
}
