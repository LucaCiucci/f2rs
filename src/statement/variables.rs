use std::{rc::{Rc, Weak}, cell::RefCell};

use proc_macro2::{TokenTree, Delimiter};

use crate::{parse_token_vec, Parsed};

#[derive(Clone, Debug)]
pub enum ArrayDimension {
    Fixed(usize),
    Dynamic(Option<String>),
}

#[derive(Clone, Debug)]
pub enum Intent {
    In,
    Out,
    InOut,
}

#[derive(Clone, Debug)]
pub enum Type {
    Logical,
    Integer,
    Real,
    DoublePrecision,
    Character,
    Type(String),
    Array(Rc<Type>, ArrayDimension),
}

impl Type {
    pub fn rust_variable_type(&self) -> String {
        match self {
            Self::Logical => "bool".to_string(),
            Self::Integer => "i32".to_string(),
            Self::Real => "f32".to_string(),
            Self::DoublePrecision => "f64".to_string(),
            Self::Character => "char".to_string(),
            Self::Type(name) => name.to_string(),
            Self::Array(ty, dim) => {
                let mut s = ty.rust_variable_type();
                match dim {
                    ArrayDimension::Fixed(size) => {
                        s = format!("[{}; {}]", s, size);
                    },
                    ArrayDimension::Dynamic(_) => {
                        s = format!("Vec<{}>", s);
                    },
                }
                s
            },
        }
    }
}

pub struct VariablesDeclaration {
    pub basic_type: Type,
    pub intent: Option<Intent>,
    pub variables: Vec<Variable>,
    pub allocatable: bool,
    pub is_value: bool,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub declaration: Weak<RefCell<VariablesDeclaration>>,
    // TODO static initialization value
    pub ty: Type,
}

pub fn parse_variables_declaration(line: &str) -> Option<Rc<RefCell<VariablesDeclaration>>> {
    let tokens = parse_token_vec(line)?;
    let primitive = parse_type_primitive(&tokens, 0)?;
    let mut pos = primitive.next;
    let mut ty = primitive.parsed;
    let mut allocatable = false;
    let mut is_value = false;

    let mut intent = None;

    // parse options
    while let Some(TokenTree::Punct(punct)) = tokens.get(pos) {
        if punct.as_char() == ',' {
            pos += 1;
        } else {
            break;
        }

        if let Some(TokenTree::Ident(ident)) = tokens.get(pos) {
            match ident.to_string().as_str() {
                "intent" => todo!(),
                "parameter" => todo!(),
                "allocatable" => {
                    allocatable = true;
                    pos += 1;
                },
                "dimension" => todo!(),
                "value" => {
                    is_value = true;
                    pos += 1;
                },
                "save" => todo!(),
                _ => return None,
            }
        } else {
            return None;
        }

        // TODO intent
        // TODO parameter
        // TODO allocatable
        // TODO dimensions
        // TODO save
    }

    // parse "::" (optional)
    if let Some(TokenTree::Punct(punct)) = tokens.get(pos) {
        if punct.as_char() == ':' {
            if let Some(TokenTree::Punct(punct)) = tokens.get(pos + 1) {
                if punct.as_char() == ':' {
                    pos += 2;
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None
        }
    } else {
        // ok
    }

    let decl = VariablesDeclaration {
        basic_type: ty,
        intent,
        variables: Vec::new(),
        allocatable,
        is_value,
    };

    let decl = Rc::new(RefCell::new(decl));

    loop {
        // parse name
        let name = if let Some(TokenTree::Ident(ident)) = tokens.get(pos) {
            ident.to_string()
        } else {
            break;
        };
        pos += 1;

        let mut ty = decl.borrow().basic_type.clone();

        if let Some(TokenTree::Group(group)) = tokens.get(pos) {
            if group.delimiter() == Delimiter::Parenthesis {
                let tokens = group.stream().into_iter().collect::<Vec<_>>();
                for token in tokens.iter().rev() {
                    if let TokenTree::Punct(punct) = token {
                        if punct.as_char() == '*' || punct.as_char() == ':' {
                            ty = Type::Array(Rc::new(ty), ArrayDimension::Dynamic(None));
                        } else if punct.as_char() == ',' {
                            continue;
                        } else {
                            return None;
                        }
                    } else if let TokenTree::Ident(ident) = token {
                        ty = Type::Array(Rc::new(ty), ArrayDimension::Dynamic(Some(ident.to_string())));
                    } else if let TokenTree::Literal(literal) = token {
                        let size = literal.to_string().parse::<usize>().ok()?;
                        ty = Type::Array(Rc::new(ty), ArrayDimension::Fixed(size));
                    } else {
                        return None;
                    }
                }
            }
            // TODO parse array dimensions
            pos += 1;
        }

        // TODO parse "= value"

        decl.borrow_mut().variables.push(Variable {
            name,
            declaration: Rc::downgrade(&decl),
            ty,
        });

        // if we are at the end of the tokens, we are done, otherwise we expect a comma
        if pos >= tokens.len() {
            break;
        } else if let Some(TokenTree::Punct(punct)) = tokens.get(pos) {
            if punct.as_char() == ',' {
                pos += 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Some(decl)
}

fn parse_type_primitive(tokens: &[TokenTree], pos: usize) -> Option<Parsed<Type>> {
    // derived type
    let (possibly_derived, skip) = if let Some(TokenTree::Ident(ident)) = &tokens.get(pos) {
        match ident.to_string().as_str() {
            "logical" | "integer" | "real" | "character" => (true, 0),
            "double" => (true, 1),
            "type" => (true, 0),
            _ => (false, 0),
        }
    } else {
        (false, 0)
    };
    if possibly_derived {
        if let Some(TokenTree::Group(group)) = tokens.get(pos + skip + 1) {
            if group.delimiter() == Delimiter::Parenthesis {
                let content =  group.stream().into_iter().collect::<Vec<_>>();
                if content.len() == 1 {
                    if let TokenTree::Ident(ident) = &content[0] {
                        return Some(Parsed::new(
                            Type::Type(ident.to_string()),
                            pos + skip + 2
                        ));
                    }
                }
            }
        }
    }

    if let Some(TokenTree::Ident(ident)) = &tokens.get(pos) {
        match ident.to_string().as_str() {
            "logical" => Some(Parsed::new(Type::Logical, pos + 1)),
            "integer" => Some(Parsed::new(Type::Integer, pos + 1)),
            "real" => Some(Parsed::new(Type::Real, pos + 1)),
            "double" => Some(Parsed::new(Type::DoublePrecision, pos + 2)),
            "character" => {
                match tokens.get(pos + 1) {
                    Some(TokenTree::Group(group)) => {
                        if group.delimiter() == Delimiter::Parenthesis {
                            todo!()
                            // handle character(len=...), character(...) or character(:)
                        } else {
                            None
                        }
                    },
                    _ => Some(Parsed::new(Type::Character, pos + 1)),
                }
            },
            "type" => None,
            _ => None,
        }
    } else {
        None
    }
}