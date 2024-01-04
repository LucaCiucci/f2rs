use super::*;

#[derive(Debug, Clone)]
pub struct SpecificationPart<Span> {
    pub use_stmts: Vec<UseStmt<Span>>,
    pub import_stmts: Vec<ImportStmt<Span>>,
    pub implicit_part: Option<ImplicitPart<Span>>,
    pub declaration_constructs: Vec<DeclarationConstruct<Span>>,
}

#[syntax_rule(
    F18V007r1 rule "specification-part" #504 :
    "is [ use-stmt ] ..."
    "    [ import-stmt ] ..."
    "    [ implicit-part ]"
    "    [ declaration-construct ] ...",
)]
pub fn specification_part<'a, S: TextSource + 'a, U: 'a>(
    cfg: &'a Cfg,
    until: impl Parser<S, Token = U> + 'a,
) -> impl Parser<S, Token = SpecificationPart<S::Span>> + 'a {
    |_| todo!("TODO: \"specification_part\" parser not implemented yet")
}