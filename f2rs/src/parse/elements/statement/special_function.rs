use riddle::provided::text::rusty::Identifier;

use crate::parse::eol_or_comment;

use super::*;

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, EnumAsInner)]
pub enum StarOrExpr<Span> {
    Star,
    Expression(Expression<Span>),
    // TODO id = expr
}

pub fn star_or_expr<S: TextSource>() -> impl Parser<S, Token = StarOrExpr<S::Span>> {
    alt! {
        spaced('*').map(|_| StarOrExpr::Star),
        spaced(expression()).map(StarOrExpr::Expression),
    }
}

// fortran
const SPECIAL_FUNCTIONS: [&str; 4] = ["print", "write", "read", "format"];

pub fn special_function_name<S: TextSource>() -> impl Parser<S, Token = Identifier<S::Span>> {
    spaced(identifier()).condition(|id, _| SPECIAL_FUNCTIONS.contains(&id.value.as_str()))
}

#[derive(Debug, Clone)]
pub struct SpecialFunction<Span> {
    pub name: Identifier<Span>,
    pub special_args: Option<Vec<StarOrExpr<Span>>>,
    pub items: Vec<StarOrExpr<Span>>,
}

pub fn special_function<S: TextSource>() -> impl Parser<S, Token = SpecialFunction<S::Span>> {
    (
        spaced(special_function_name()),
        (
            spaced('('),
            separated(spaced(star_or_expr()), spaced(','), 0..),
            spaced(')'),
        )
            .map(|(_, s, _)| s)
            .optional(),
        spaced(',').optional(),
        separated(star_or_expr(), spaced(','), 0..),
        eol_or_comment(),
    )
        .map(|(name, special_args, _, items, _)| SpecialFunction {
            name,
            special_args,
            items,
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_special_function() {
        let r = special_function().parse("print *, 1").0.unwrap();

        assert_eq!(r.name.value, "print");
        assert!(r.special_args.is_none());
        assert!(r.items.len() == 2);
        assert!(r.items[0].is_star());
        assert_eq!(
            r.items[1]
                .as_expression()
                .unwrap()
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "1"
        );

        let r = special_function().parse("write(*, *) 1").0.unwrap();

        assert_eq!(r.name.value, "write");

        assert!(r.special_args.is_some());
        assert!(r.special_args.as_ref().unwrap().len() == 2);
        assert!(r.special_args.as_ref().unwrap()[0].is_star());
        assert!(r.special_args.as_ref().unwrap()[1].is_star());

        assert!(r.items.len() == 1);
        assert_eq!(
            r.items[0]
                .as_expression()
                .unwrap()
                .as_literal()
                .unwrap()
                .as_number()
                .unwrap()
                .value,
            "1"
        );
    }
}
