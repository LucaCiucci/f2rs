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
    move |source: S| {
        let mut result = SpecificationPart {
            use_stmts: Vec::new(),
            import_stmts: Vec::new(),
            implicit_part: None,
            declaration_constructs: Vec::new(),
        };

        let ((use_stmts, u), source) = many_until(use_stmt(cfg), until, 0..)?;
        result.use_stmts = use_stmts;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((import_stmts, u), source) = many_until(import_stmt(cfg), until, u..)?;
        result.import_stmts = import_stmts;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((implicit_part, u), source) = implicit_part(cfg, until).optional().parse(source)?;
        result.implicit_part = implicit_part;
        if u.is_some() {
            return Some(((result, u), source));
        }

        let ((declaration_constructs, u), source) = many_until(declaration_construct(cfg), until, u..)?;
        result.declaration_constructs = declaration_constructs;
        if u.is_some() {
            return Some(((result, u), source));
        }

        Some(((result, None), source))
    }
}