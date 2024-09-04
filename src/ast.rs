use std::rc::Rc;

use crate::tokens::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LiteralKind {
    U8, U16, U32, U64, Usz,
    I8, I16, I32, I64, AnyInt,
    F32, F64, AnyFloat,
    RawString, String, Char,
    Void
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct FunctionParam {
    pub name: Option<Token>,
    pub type_: Expression,
    pub is_const: bool
}

impl FunctionParam {
    pub fn new(name: Option<Token>, type_: Expression, is_const: bool) -> Self {
        FunctionParam { name, type_, is_const }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructField {
    pub name: Token,
    pub expr: Expression,
    pub is_const: bool
}

impl StructField {
    pub fn new(name: Token, expr: Expression, is_const: bool) -> Self {
        StructField { name, expr, is_const }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct EnumVariant {
    pub name: Token,
    pub expr: Expression,
}

impl EnumVariant {
    pub fn new(name: Token, expr: Expression) -> Self {
        EnumVariant { name, expr }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SwitchCase {
    pub cases: Option<Vec<Expression>>, // is none when default
    pub code: Vec<Statement>
}

impl SwitchCase {
    pub fn new(cases: Option<Vec<Expression>>, code: Vec<Statement>) -> Self {
        SwitchCase { cases, code }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BitfieldField {
    pub name: Token,
    pub bits: u8
}

impl BitfieldField {
    pub fn new(name: Token, bits: u8) -> Self {
        BitfieldField { name, bits }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>), // left op right
    Literal(Rc<str>, Token, LiteralKind), 
    Unary(Token, Box<Expression>, bool), // op expr is_prefix
    Grouping(Box<Expression>),
    Variable(Token), // name
    Assign(Box<Expression>, Token, Box<Expression>), // target op value
    Call(Box<Expression>, Token, Vec<Expression>), // callee paren arguments
    FnPtr(Token, Box<Expression>, Vec<FunctionParam>), // kw return_type params
    Ternary(Token, Box<Expression>, Box<Expression>, Box<Expression>), // question_tok cond then else
    CompoundLiteral(Box<Expression>, Token, Vec<StructField>), // struct closing_brace fields
    Subscript(Box<Expression>, Token, Vec<Expression>), // subscripted paren arguments
    Get(Box<Expression>, Token), // object name
    StaticGet(Box<Expression>, Token), // object name
    Slice(Token, Vec<Expression>), // opening_brace items 
}

pub struct AstPos {
    pub source: Rc<str>,
    pub filename: Rc<str>,
    pub start: usize,
    pub end: usize,
    pub line: usize
}

impl AstPos {
    pub fn new(source: Rc<str>, filename: Rc<str>, start: usize, end: usize, line: usize) -> Self {
        AstPos { source, filename, start, end, line }
    }
}

impl Expression {
    pub fn get_pos(&self) -> AstPos {
        match self {
            Expression::Grouping(expr) => expr.get_pos(),
            Expression::Literal(_, tok, _) | Expression::Variable(tok) => {
                AstPos::new(Rc::clone(&tok.source), Rc::clone(&tok.filename), tok.pos, tok.pos + tok.lexeme.len(), tok.line)
            }
            Expression::Binary(left, op, right) => {
                let left_pos = left.get_pos();
                let right_pos = right.get_pos();

                if left_pos.line != right_pos.line {
                    left_pos
                } else {
                    AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), left_pos.start, right_pos.end, left_pos.line)
                }
            }
            Expression::Unary(op, expr, is_prefix) => {
                let expr_pos = expr.get_pos();

                if *is_prefix {
                    if op.line != expr_pos.line {
                        AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), op.pos, op.pos + op.lexeme.len(), op.line)
                    } else {
                        AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), op.pos, expr.get_pos().end, op.line)
                    }
                } else {
                    if expr_pos.line != op.line {
                        expr_pos
                    } else {
                        AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), expr_pos.start, op.pos + op.lexeme.len(), expr_pos.line)
                    }
                }
            }
            Expression::Assign(target, op, value) => {
                let target_pos = target.get_pos();
                let value_pos = value.get_pos();

                if target_pos.line != value_pos.line {
                    target_pos
                } else {
                    AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), target_pos.start, value_pos.end, target_pos.line)
                }
            }
            Expression::Call(callee, paren, _) => {
                let callee_pos = callee.get_pos();

                if callee_pos.line != paren.line {
                    callee_pos
                } else {
                    AstPos::new(Rc::clone(&paren.source), Rc::clone(&paren.filename), callee_pos.start, paren.pos + paren.lexeme.len(), callee_pos.line)
                }
            }
            Expression::FnPtr(kw, return_type, _) => {
                let return_type_pos = return_type.get_pos();

                if kw.line != return_type_pos.line {
                    AstPos::new(Rc::clone(&kw.source), Rc::clone(&kw.filename), kw.pos, kw.pos + kw.lexeme.len(), kw.line)
                } else { 
                    AstPos::new(Rc::clone(&kw.source), Rc::clone(&kw.filename), kw.pos, return_type_pos.end, kw.line)
                }
            }
            Expression::Ternary(question, cond, _, else_) => {
                let cond_pos = cond.get_pos();
                let else_pos = else_.get_pos();

                if cond_pos.line != else_pos.line {
                    cond_pos
                } else {
                    AstPos::new(Rc::clone(&question.source), Rc::clone(&question.filename), cond_pos.start, else_pos.end, cond_pos.line)
                }
            }
            Expression::CompoundLiteral(struct_, closing_brace, _) => {
                let struct_pos = struct_.get_pos();

                if struct_pos.line != closing_brace.line {
                    struct_pos
                } else {
                    AstPos::new(Rc::clone(&closing_brace.source), Rc::clone(&closing_brace.filename), struct_pos.start, closing_brace.pos + closing_brace.lexeme.len(), struct_pos.line)
                }
            }
            Expression::Subscript(subscripted, paren, _) => {
                let subscripted_pos = subscripted.get_pos();

                if subscripted_pos.line != paren.line {
                    subscripted_pos
                } else {
                    AstPos::new(Rc::clone(&paren.source), Rc::clone(&paren.filename), subscripted_pos.start, paren.pos + paren.lexeme.len(), subscripted_pos.line)
                }
            }
            Expression::Get(object, name) | Expression::StaticGet(object, name)=> {
                let object_pos = object.get_pos();

                if object_pos.line != name.line {
                    object_pos
                } else {
                    AstPos::new(Rc::clone(&name.source), Rc::clone(&name.filename), object_pos.start, name.pos + name.lexeme.len(), object_pos.line)
                }
            },
            Expression::Slice(tok, exprs) => {
                match exprs.len() {
                    0 => unreachable!(), // guaranteed by parser
                    1 => exprs[0].get_pos(),
                    _ => {
                        let first_pos = exprs[0].get_pos();
                        let last_pos = exprs.last().unwrap().get_pos();

                        if first_pos.line != last_pos.line {
                            first_pos
                        } else {
                            AstPos::new(Rc::clone(&tok.source), Rc::clone(&tok.filename), first_pos.start, last_pos.end, first_pos.line)
                        }
                    }
                }
            }
        }
    }

    pub fn is_valid_assignment_target(&self) -> bool {
        match self {
            Expression::Variable(_) | Expression::Get(..) | Expression::StaticGet(..) | Expression::Subscript(..) => true,
            Expression::Unary(op, _, is_prefix) => *is_prefix && op.type_ == TokenType::Star,
            Expression::Grouping(inner) => inner.is_valid_assignment_target(),
            _ => false
        }
    }

    pub fn replace_variable(&self, name: &Rc<str>, replace_expr: &Expression) -> Expression {
        match self {
            Expression::Grouping(expr) => expr.replace_variable(name, replace_expr),
            Expression::Literal(..) => self.clone(),
            Expression::Variable(tok) => {
                if tok.lexeme.as_ref() == name.as_ref() {
                    replace_expr.clone()
                } else {
                    self.clone()
                }
            }
            Expression::Binary(left, op, right) => {
                Expression::Binary(Box::new(left.replace_variable(name, replace_expr)), op.clone(), Box::new(right.replace_variable(name, replace_expr)))
            }
            Expression::Unary(op, expr, is_prefix) => {
                Expression::Unary(op.clone(), Box::new(expr.replace_variable(name, replace_expr)), *is_prefix)
            }
            Expression::Assign(target, op, value) => {
                Expression::Assign(Box::new(target.replace_variable(name, replace_expr)), op.clone(), Box::new(value.replace_variable(name, replace_expr)))
            }
            Expression::Ternary(question, cond, then, else_) => {
                Expression::Ternary(
                    question.clone(),
                    Box::new(cond.replace_variable(name, replace_expr)), 
                    Box::new(then.replace_variable(name, replace_expr)), 
                    Box::new(else_.replace_variable(name, replace_expr))
                )
            }
            Expression::Call(callee, paren, args) => {
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(arg.replace_variable(name, replace_expr));
                }

                Expression::Call(Box::new(callee.replace_variable(name, replace_expr)), paren.clone(), new_args)
            }
            Expression::FnPtr(kw, return_type, params) => {
                let mut new_params = Vec::new();
                for param in params {
                    new_params.push(FunctionParam::new(param.name.clone(), param.type_.replace_variable(name, replace_expr), param.is_const))
                }

                Expression::FnPtr(kw.clone(), Box::new(return_type.replace_variable(name, replace_expr)), new_params)
            }
            Expression::CompoundLiteral(struct_, closing_brace, fields) => {
                let mut new_fields = Vec::new();
                for field in fields {
                    new_fields.push(StructField::new(field.name.clone(), field.expr.replace_variable(name, replace_expr), field.is_const))
                }

                Expression::CompoundLiteral(Box::new(struct_.replace_variable(name, replace_expr)), closing_brace.clone(), new_fields)
            }
            Expression::Subscript(subscripted, paren, args) => {
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(arg.replace_variable(name, replace_expr));
                }

                Expression::Subscript(Box::new(subscripted.replace_variable(name, replace_expr)), paren.clone(), new_args)
            }
            Expression::Get(object, get_name) => {
                Expression::Get(Box::new(object.replace_variable(name, replace_expr)), get_name.clone())
            }
            Expression::StaticGet(object, get_name) => {
                Expression::StaticGet(Box::new(object.replace_variable(name, replace_expr)), get_name.clone())
            }
            Expression::Slice(opening_brace, items) => {
                let mut new_items = Vec::new();
                for item in items {
                    new_items.push(item.replace_variable(name, replace_expr));
                }

                Expression::Slice(opening_brace.clone(), new_items)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ImportType {
    Default,
    Ang,
    Lib
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Generic {
    pub name: Token,
    pub bounds: Option<Expression>,
    pub default: Option<Expression>
}

impl Generic {
    pub fn new(name: Token, bounds: Option<Expression>, default: Option<Expression>) -> Self {
        Generic { name, bounds, default }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    Expression(Expression),
    VarDecl(Token, Option<Expression>, Option<Expression>, bool, Vec<Token>), // name initializer type is_const qualifiers
    Block(Token, Vec<Statement>), // kw statements
    If(Token, Expression, Box<Statement>, Option<Box<Statement>>), // kw cond then else
    While(Token, Expression, Box<Statement>), // kw cond body
    DoWhile(Token, Expression, Box<Statement>), // kw cond body
    For(Token, Option<Box<Statement>>, Expression, Option<Expression>, Box<Statement>), // kw init cond increment body
    Function(Token, Vec<FunctionParam>, Expression, Option<Vec<Statement>>, Vec<Token>, Vec<Token>, bool), // name params return_type body qualifiers generics_names bind
    Return(Token, Option<Expression>), // kw value
    Struct(Token, Vec<StructField>, bool, Option<Token>, Vec<Token>, bool), // name fields has_body binding generics_names bind_typedefed
    Impl(Expression, Vec<Statement>), // struct declarations
    Namespace(Token, Vec<Statement>), // name body
    Use(Expression, Token), // use_expr as
    Enum(Token, Expression, Vec<EnumVariant>, bool, bool, Option<Token>, Vec<Token>, bool), // name kind_type variants is_simple has_body binding generics_names bind_typedefed
    Defer(Token, Box<Statement>), // kw statement
    Switch(Token, Expression, Vec<SwitchCase>), // kw expr cases
    Template(Token, Box<Statement>, Vec<Generic>, Vec<Token>), // name templated_expr generics generics_names
    Empty, // placeholder
    Undef(Rc<str>), // internal
    Break(Token), // kw
    Continue(Token), // kw
    Import(Token, ImportType), // path is_ang
    Union(Token, Vec<StructField>, bool, Option<Token>, bool), // name fields has_body binding bind_typedefed
    Bitfield(Token, Vec<BitfieldField>, bool, Option<Token>, bool), // name fields has_body binding bind_typedefed
    Macro(Token, Option<Vec<Token>>, Option<Expression>, Option<Expression>), // name params return_expr return_type
    Foreach(Token, Token, Expression, Box<Statement>) // kw variable iterator body
}

impl Statement {
    pub fn get_pos(&self) -> AstPos {
        match self {
            Statement::Empty | Statement::Undef(_) => AstPos::new(Rc::from(""), Rc::from(""), 0, 0, 0),
            Statement::Expression(expr) | 
            Statement::Impl(expr, _) | 
            Statement::Use(expr, _) => expr.get_pos(),
            
            Statement::VarDecl(tok, ..) | 
            Statement::Block(tok, _) |
            Statement::If(tok, ..) |
            Statement::While(tok, ..) |
            Statement::For(tok, ..) |
            Statement::DoWhile(tok, ..) |
            Statement::Function(tok, ..) |
            Statement::Return(tok, _) |
            Statement::Struct(tok, ..) |
            Statement::Namespace(tok, _) |
            Statement::Enum(tok, ..) |
            Statement::Defer(tok, _) |
            Statement::Switch(tok, ..) |
            Statement::Template(tok, ..) |
            Statement::Break(tok) |
            Statement::Continue(tok) |
            Statement::Import(tok, _) |
            Statement::Union(tok, ..) |
            Statement::Bitfield(tok, ..) |
            Statement::Macro(tok, ..) |
            Statement::Foreach(tok, ..) => {
                AstPos::new(Rc::clone(&tok.source), Rc::clone(&tok.filename), tok.pos, tok.pos + tok.lexeme.len(), tok.line)
            } 
        }
    }
}