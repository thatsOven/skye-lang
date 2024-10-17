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
    StaticGet(Box<Expression>, Token, bool), // object name gets_macro
    Slice(Token, Vec<Expression>), // opening_brace items 
}

pub struct AstPos {
    pub source: Rc<str>,
    pub filename: Rc<str>,
    pub start: usize,
    pub end: usize,
    pub line: usize
}

impl std::fmt::Debug for AstPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstPos").field("filename", &self.filename).field("start", &self.start).field("end", &self.end).field("line", &self.line).finish()
    }
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
                AstPos::new(Rc::clone(&tok.source), Rc::clone(&tok.filename), tok.pos, tok.end, tok.line)
            }
            Expression::Binary(left, op, right) => {
                let left_pos = left.get_pos();
                let right_pos = right.get_pos();

                if left_pos.line != right_pos.line || left_pos.filename != right_pos.filename {
                    left_pos
                } else {
                    AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), left_pos.start, right_pos.end, left_pos.line)
                }
            }
            Expression::Unary(op, expr, is_prefix) => {
                let expr_pos = expr.get_pos();

                if *is_prefix {
                    if op.line != expr_pos.line || op.filename != expr_pos.filename {
                        AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), op.pos, op.end, op.line)
                    } else {
                        AstPos::new(Rc::clone(&op.source), Rc::clone(&op.filename), op.pos, expr.get_pos().end, op.line)
                    }
                } else {
                    if expr_pos.line != op.line || op.filename != expr_pos.filename {
                        expr_pos
                    } else {
                        AstPos::new(Rc::clone(&expr_pos.source), Rc::clone(&expr_pos.filename), expr_pos.start, op.end, expr_pos.line)
                    }
                }
            }
            Expression::Assign(target, _, value) => {
                let target_pos = target.get_pos();
                let value_pos = value.get_pos();

                if target_pos.line != value_pos.line || target_pos.filename != value_pos.filename {
                    target_pos
                } else {
                    AstPos::new(Rc::clone(&target_pos.source), Rc::clone(&target_pos.filename), target_pos.start, value_pos.end, target_pos.line)
                }
            }
            Expression::Call(callee, paren, _) => {
                let callee_pos = callee.get_pos();

                if callee_pos.line != paren.line || callee_pos.filename != paren.filename {
                    callee_pos
                } else {
                    AstPos::new(Rc::clone(&callee_pos.source), Rc::clone(&callee_pos.filename), callee_pos.start, paren.end, callee_pos.line)
                }
            }
            Expression::FnPtr(kw, return_type, _) => {
                let return_type_pos = return_type.get_pos();

                if kw.line != return_type_pos.line || kw.filename != return_type_pos.filename {
                    AstPos::new(Rc::clone(&kw.source), Rc::clone(&kw.filename), kw.pos, kw.end, kw.line)
                } else { 
                    AstPos::new(Rc::clone(&kw.source), Rc::clone(&kw.filename), kw.pos, return_type_pos.end, kw.line)
                }
            }
            Expression::Ternary(_, cond, _, else_) => {
                let cond_pos = cond.get_pos();
                let else_pos = else_.get_pos();

                if cond_pos.line != else_pos.line || cond_pos.filename != else_pos.filename {
                    cond_pos
                } else {
                    AstPos::new(Rc::clone(&cond_pos.source), Rc::clone(&cond_pos.filename), cond_pos.start, else_pos.end, cond_pos.line)
                }
            }
            Expression::CompoundLiteral(struct_, closing_brace, _) => {
                let struct_pos = struct_.get_pos();

                if struct_pos.line != closing_brace.line || struct_pos.filename != closing_brace.filename {
                    struct_pos
                } else {
                    AstPos::new(Rc::clone(&struct_pos.source), Rc::clone(&struct_pos.filename), struct_pos.start, closing_brace.end, struct_pos.line)
                }
            }
            Expression::Subscript(subscripted, paren, _) => {
                let subscripted_pos = subscripted.get_pos();

                if subscripted_pos.line != paren.line || subscripted_pos.filename != paren.filename {
                    subscripted_pos
                } else {
                    AstPos::new(Rc::clone(&subscripted_pos.source), Rc::clone(&subscripted_pos.filename), subscripted_pos.start, paren.end, subscripted_pos.line)
                }
            }
            Expression::Get(object, name) | Expression::StaticGet(object, name, _)=> {
                let object_pos = object.get_pos();

                if object_pos.line != name.line || object_pos.filename != name.filename {
                    AstPos::new(Rc::clone(&name.source), Rc::clone(&name.filename), name.pos, name.end, name.line)
                } else {
                    AstPos::new(Rc::clone(&object_pos.source), Rc::clone(&object_pos.filename), object_pos.start, name.end, object_pos.line)
                }
            },
            Expression::Slice(_, exprs) => {
                match exprs.len() {
                    0 => unreachable!(), // guaranteed by parser
                    1 => exprs[0].get_pos(),
                    _ => {
                        let first_pos = exprs[0].get_pos();
                        let last_pos = exprs.last().unwrap().get_pos();

                        if first_pos.line != last_pos.line || first_pos.filename != last_pos.filename {
                            first_pos
                        } else {
                            AstPos::new(Rc::clone(&first_pos.source), Rc::clone(&first_pos.filename), first_pos.start, last_pos.end, first_pos.line)
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
                Expression::Call(
                    Box::new(callee.replace_variable(name, replace_expr)), 
                    paren.clone(), 
                    args.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                )
            }
            Expression::FnPtr(kw, return_type, params) => {
                Expression::FnPtr(
                    kw.clone(), 
                    Box::new(return_type.replace_variable(name, replace_expr)), 
                    params.iter().map(
                        |x| FunctionParam::new(
                            x.name.clone(), x.type_.replace_variable(name, replace_expr), x.is_const
                        )
                    ).collect()
                )
            }
            Expression::CompoundLiteral(struct_, closing_brace, fields) => {
                Expression::CompoundLiteral(
                    Box::new(struct_.replace_variable(name, replace_expr)), 
                    closing_brace.clone(), 
                    fields.iter().map(
                        |x| StructField::new(
                            x.name.clone(), x.expr.replace_variable(name, replace_expr), x.is_const
                        )
                    ).collect()
                )
            }
            Expression::Subscript(subscripted, paren, args) => {
                Expression::Subscript(
                    Box::new(subscripted.replace_variable(name, replace_expr)), 
                    paren.clone(), 
                    args.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                )
            }
            Expression::Get(object, get_name) => {
                Expression::Get(Box::new(object.replace_variable(name, replace_expr)), get_name.clone())
            }
            Expression::StaticGet(object, get_name, gets_macro) => {
                Expression::StaticGet(Box::new(object.replace_variable(name, replace_expr)), get_name.clone(), *gets_macro)
            }
            Expression::Slice(opening_brace, items) => {
                Expression::Slice(
                    opening_brace.clone(), 
                    items.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                )
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
pub enum MacroParams {
    None,
    Some(Vec<Token>),
    Variable(Token)
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MacroBody {
    Binding(Expression),
    Expression(Expression),
    Block(Vec<Statement>)
}

impl MacroBody {
    pub fn replace_variable(&self, name: &Rc<str>, replace_expr: &Expression) -> Self {
        match self {
            MacroBody::Binding(expression) => MacroBody::Binding(expression.replace_variable(name, replace_expr)),
            MacroBody::Expression(expression) => MacroBody::Expression(expression.replace_variable(name, replace_expr)),
            MacroBody::Block(statements) => {
                MacroBody::Block(statements.iter().map(|x| x.replace_variable(name, replace_expr)).collect())
            }
        }
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
    Use(Expression, Token, bool, bool), // use_expr as typedef bind
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
    Macro(Token, MacroParams, MacroBody), // name params body
    Foreach(Token, Token, Expression, Box<Statement>) // kw variable iterator body
}

impl Statement {
    pub fn get_pos(&self) -> AstPos {
        match self {
            Statement::Empty | Statement::Undef(_) => AstPos::new(Rc::from(""), Rc::from(""), 0, 0, 0),
            Statement::Expression(expr) | 
            Statement::Impl(expr, _) | 
            Statement::Use(expr, ..) => expr.get_pos(),
            
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
                AstPos::new(Rc::clone(&tok.source), Rc::clone(&tok.filename), tok.pos, tok.end, tok.line)
            } 
        }
    }

    pub fn replace_variable(&self, name: &Rc<str>, replace_expr: &Expression) -> Statement {
        match self {
            Statement::Empty | Statement::Undef(_) |  Statement::Break(_) | Statement::Continue(_) | 
            Statement::Import(..) | Statement::Bitfield(..) => self.clone(),

            Statement::Expression(expression) => Statement::Expression(expression.replace_variable(name, replace_expr)),
            Statement::VarDecl(var_name, initializer, type_, is_const, qualifiers) => {
                Statement::VarDecl(
                    var_name.clone(), 
                    initializer.as_ref().map(|x| x.replace_variable(name, replace_expr)), 
                    type_.as_ref().map(|x| x.replace_variable(name, replace_expr)), 
                    *is_const, qualifiers.clone()
                )
            }
            Statement::Block(kw, statements) => {
                Statement::Block(kw.clone(), statements.iter().map(|x| x.replace_variable(name, replace_expr)).collect())
            }
            Statement::If(kw, cond, then_branch, else_branch) => {
                Statement::If(
                    kw.clone(), cond.replace_variable(name, replace_expr), 
                    Box::new(then_branch.replace_variable(name, replace_expr)), 
                    else_branch.as_ref().map(|x| Box::new(x.replace_variable(name, replace_expr)))
                )
            }
            Statement::While(kw, cond, body) => {
                Statement::While(kw.clone(), cond.replace_variable(name, replace_expr), Box::new(body.replace_variable(name, replace_expr)))
            }
            Statement::DoWhile(kw, cond, body) => {
                Statement::DoWhile(kw.clone(), cond.replace_variable(name, replace_expr), Box::new(body.replace_variable(name, replace_expr)))
            }
            Statement::For(kw, initializer, cond, increment, body) => {
                Statement::For(
                    kw.clone(), 
                    initializer.as_ref().map(|x| Box::new(x.replace_variable(name, replace_expr))), 
                    cond.replace_variable(name, replace_expr), 
                    increment.as_ref().map(|x| x.replace_variable(name, replace_expr)), 
                    Box::new(body.replace_variable(name, replace_expr))
                )
            }
            Statement::Function(kw, params, return_type, body, qualifiers, generics_names, bind) => {
                Statement::Function(
                    kw.clone(), 
                    params.iter().map(|x| FunctionParam::new(x.name.clone(), x.type_.replace_variable(name, replace_expr), x.is_const)).collect(),
                    return_type.replace_variable(name, replace_expr),
                    body.as_ref().map(|x| x.iter().map(|statement| statement.replace_variable(name, replace_expr)).collect()),
                    qualifiers.clone(),
                    generics_names.clone(), 
                    *bind
                )
            }
            Statement::Return(kw, return_expr) => {
                Statement::Return(kw.clone(), return_expr.as_ref().map(|x| x.replace_variable(name, replace_expr)))
            }
            Statement::Struct(struct_name, fields, has_body, binding, generics_names, bind_typedefed) => {
                Statement::Struct(
                    struct_name.clone(), 
                    fields.iter().map(|x| StructField::new(x.name.clone(), x.expr.replace_variable(name, replace_expr), x.is_const)).collect(),
                    *has_body, binding.clone(), generics_names.clone(), *bind_typedefed
                )
            }
            Statement::Impl(struct_, declarations) => {
                Statement::Impl(
                    struct_.replace_variable(name, replace_expr),
                    declarations.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                )
            }
            Statement::Namespace(namespace_name, declarations) => {
                Statement::Namespace(
                    namespace_name.clone(), 
                    declarations.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                )
            }
            Statement::Use(expression, alias, typedef, bind) => {
                Statement::Use(expression.replace_variable(name, replace_expr), alias.clone(), *typedef, *bind)
            }
            Statement::Enum(enum_name, kind_type, variants, is_simple, has_body, binding, generics_names, bind_typedefed) => {
                Statement::Enum(
                    enum_name.clone(),
                    kind_type.replace_variable(name, replace_expr),
                    variants.iter().map(|x| EnumVariant::new(x.name.clone(), x.expr.replace_variable(name, replace_expr))).collect(),
                    *is_simple, *has_body, binding.clone(), generics_names.clone(), *bind_typedefed
                )
            }
            Statement::Defer(kw, statement) => {
                Statement::Defer(kw.clone(), Box::new(statement.replace_variable(name, replace_expr)))
            }
            Statement::Switch(kw, cond, cases) => {
                Statement::Switch(
                    kw.clone(), cond.replace_variable(name, replace_expr),
                    cases.iter().map(
                        |x| SwitchCase::new(
                            x.cases.as_ref().map(
                                |orig_cases| orig_cases.iter().map(
                                    |case| case.replace_variable(name, replace_expr)
                                ).collect()
                            ),
                            x.code.iter().map(|x| x.replace_variable(name, replace_expr)).collect()
                        )
                    ).collect()
                )
            }
            Statement::Template(template_name, declaration, generics, generics_names) => {
                Statement::Template(
                    template_name.clone(), Box::new(declaration.replace_variable(name, replace_expr)),
                    generics.iter().map(
                        |x| Generic::new(
                            x.name.clone(), 
                            x.bounds.as_ref().map(|bounds| bounds.replace_variable(name, replace_expr)),
                            x.default.as_ref().map(|default| default.replace_variable(name, replace_expr))
                        )
                    ).collect(),
                    generics_names.clone()
                )
            }
            Statement::Union(union_name, fields, has_body, binding, bind_typedefed) => {
                Statement::Union(
                    union_name.clone(), 
                    fields.iter().map(|x| StructField::new(x.name.clone(), x.expr.replace_variable(name, replace_expr), x.is_const)).collect(),
                    *has_body, binding.clone(), *bind_typedefed
                )
            }
            Statement::Macro(macro_name, macro_params, macro_body) => {
                Statement::Macro(macro_name.clone(), macro_params.clone(), macro_body.replace_variable(name, replace_expr))
            }
            Statement::Foreach(kw, var_name, iterator, body) => {
                Statement::Foreach(
                    kw.clone(), var_name.clone(), iterator.replace_variable(name, replace_expr),
                    Box::new(body.replace_variable(name, replace_expr))
                )
            }
        }
    }
}