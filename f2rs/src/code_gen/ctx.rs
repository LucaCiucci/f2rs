use std::collections::HashMap;

use enum_as_inner::EnumAsInner;

use crate::parse::{Element, elements::{Statement, TypeWithModifiers, Type}, function::Function};

use super::Span;

#[derive(Debug, Clone, Copy, EnumAsInner, PartialEq, Eq, Hash)]
pub enum ContextType {
    Code,
    NonCode,
    InsideTypeDefinition,
}

pub struct Ctx<'a> {
    parent: Option<&'a Ctx<'a>>,
    ty: ContextType,
    symbols: HashMap<String, SymbolData>,
}

impl<'a> Ctx<'a> {
    pub fn new(
        parent: Option<&'a Ctx<'a>>,
        ty: ContextType,
        symbols: HashMap<String, SymbolData>,
    ) -> Self {
        Ctx {
            ty,
            parent,
            symbols,
        }
    }

    pub fn new_with_parent(parent: &'a Ctx<'a>, ty: ContextType) -> Self {
        Ctx::new(Some(parent), ty, HashMap::new())
    }

    pub fn root() -> Self {
        Ctx::new(None, ContextType::NonCode, HashMap::new())
    }

    pub fn code_root() -> Self {
        Ctx::new(None, ContextType::Code, HashMap::new())
    }

    pub fn in_body(&'a self, elements: &[Element<Span>]) -> Self {
        Ctx::new(
            Some(self),
            ContextType::Code,
            find_items(elements)
        )
    }

    pub fn in_non_code_body(&'a self, elements: &[Element<Span>]) -> Self {
        Ctx::new(
            Some(self),
            ContextType::NonCode,
            find_items(elements)
        )
    }

    pub fn in_type_definition(&'a self, elements: &[Element<Span>]) -> Self {
        Ctx::new(
            Some(self),
            ContextType::InsideTypeDefinition,
            find_items(elements)
        )
    }

    pub fn context_type(&self) -> ContextType {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub enum SymbolData {
    Undetermined,
    Variable {
        ty: Option<TypeWithModifiers<Span>>,
    },
    FunctionOrSubroutine {
        args: Option<Vec<(String, Option<TypeWithModifiers<Span>>)>>,
        return_type: Option<Type<Span>>,
    },
    Struct {
        body: Vec<(String, TypeWithModifiers<Span>)>,
    },
    Module,
}

pub fn find_items(elements: &[Element<Span>]) -> HashMap<String, SymbolData> {
    let mut items = HashMap::new();
    for element in elements {
        match element {
            Element::EmptyLines(_) => {},
            Element::LineComment(_) => {},
            Element::Program(program) => {
                items.insert(program.name.clone(), SymbolData::FunctionOrSubroutine {
                    args: Some(Vec::new()),
                    return_type: None,
                });
            },
            Element::Statement(s) => {
                match s {
                    Statement::Implicit(_) => {},
                    Statement::UseStatement(use_statement) => {
                        for (item, alias) in &use_statement.only {
                            let name = alias.as_ref().unwrap_or(item);
                            items.insert(name.clone(), SymbolData::Undetermined);
                        }
                    },
                    Statement::VariablesDeclaration(decl) => {
                        for (ty, name, _) in &decl.vars {
                            if ty.modifiers.external {
                                items.insert(name.clone(), SymbolData::FunctionOrSubroutine {
                                    args: None,
                                    return_type: Some(ty.ty.clone()),
                                });
                            } else {
                                items.insert(name.clone(), SymbolData::Variable {
                                    ty: Some(ty.clone()),
                                });
                            }
                        }
                    },
                    Statement::Expression(_, _) => {},
                    Statement::DoLoop(_) => {},
                    Statement::If(_) => {},
                    Statement::CallStatement(_) => {},
                    Statement::SpecialFunction(_) => {},
                    Statement::FormatStatement(_) => {},
                    Statement::Label(_) => {},
                    Statement::GoTo(_, _) => {},
                    Statement::Continue => {},
                }
            }
            Element::Subroutine(subroutine) => {
                let args = resolve_arguments(
                    subroutine.args.iter().map(|arg| arg.value.clone()),
                    &subroutine.body,
                );

                items.insert(subroutine.name.clone(), SymbolData::FunctionOrSubroutine {
                    args: Some(args),
                    return_type: None,
                });
            },
            Element::Module(module) => {
                items.insert(module.name.clone(), SymbolData::Module);
            },
            Element::Contains => {},
            Element::Function(function) => {
                let args = resolve_arguments(
                    function.args.iter().map(|arg| arg.value.clone()),
                    &function.body,
                );

                //let return_type = if function.return_type.is_some() {
                //    Some(function.return_type.clone().unwrap())
                //} else {
                //    let return_name = if let Some(return_name) = &function.return_variable {
                //        return_name.value.clone()
                //    } else {
                //        function.name.value.clone()
                //    };
//
                //    let r = find_arguments_types(vec![return_name], &function.body);
                //    r.into_iter().next().unwrap().1.map(|ty| ty.ty.clone())
                //};

                let return_type = resolve_return_type(function);

                items.insert(function.name.value.clone(), SymbolData::FunctionOrSubroutine {
                    args: Some(args),
                    return_type,
                });
            },
            Element::StructType(s) => {
                items.insert(s.name.clone(), SymbolData::Struct {
                    body: s.body.iter().filter_map(|e| {
                        if let Element::Statement(Statement::VariablesDeclaration(decl)) = e {
                            Some(decl.vars.iter().map(|(ty, name, _)| (name.clone(), ty.clone())))
                        } else {
                            None
                        }
                    }).flatten().collect(),
                });
            },
            Element::Public(_) => {},
            Element::Private => {},
            Element::UnclassifiedLine(_, _) => {},
        }
    }

    // TODO find all external declarations and transform mentioned items into function/subroutine

    items
}

pub fn resolve_arguments(
    args: impl IntoIterator<Item = String>,
    elements: &[Element<Span>],
) -> Vec<(String, Option<TypeWithModifiers<Span>>)> {
    let args = args.into_iter().collect::<Vec<_>>();
    find_arguments_types(args, elements)
}

pub fn resolve_return_type(
    function: &Function<Span>,
) -> Option<Type<Span>> {
    let args = function.args.iter().map(|arg| arg.value.clone()).collect::<Vec<_>>();
    let args = find_arguments_types(args, &function.body);

    let return_type = if function.return_type.is_some() {
        Some(function.return_type.clone().unwrap())
    } else {
        let return_name = if let Some(return_name) = &function.return_variable {
            return_name.value.clone()
        } else {
            function.name.value.clone()
        };

        let r = find_arguments_types(vec![return_name], &function.body);
        r.into_iter().next().unwrap().1.map(|ty| ty.ty.clone())
    };

    return_type
}

pub fn find_arguments_types(
    args: Vec<String>,
    elements: &[Element<Span>],
) -> Vec<(String, Option<TypeWithModifiers<Span>>)> {
    let mut args = args.into_iter().map(|arg| (arg, None)).collect::<Vec<_>>();

    for element in elements {
        if let Element::Statement(Statement::VariablesDeclaration(decl)) = element {
            for (ty, name, _) in &decl.vars {
                if let Some((_, arg)) = args.iter_mut().find(|(arg, _)| arg == name) {
                    *arg = Some(ty.clone());
                }
            }
        }
    }

    args
}