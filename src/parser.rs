use convert_case::{Case, Casing};
use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{BitfieldField, EnumVariant, Expression, FunctionParam, Generic, ImportType, LiteralKind, MacroParams, Statement, StructField, SwitchCase}, 
    ast_error, ast_note, token_error, token_note, 
    tokens::{Token, TokenType}, 
    utils::is_valid_variant
};

macro_rules! match_literal {
    ($parser: expr, $type_: tt) => {
        if $parser.match_(&[TokenType::$type_]) {
            let prev = $parser.previous();
            return Some(Expression::Literal(prev.lexeme.clone(), prev.clone(), LiteralKind::$type_))
        }
    };
}

macro_rules! left_associativity_binary {
    ($name: tt, $next: tt, $types: expr) => {
        fn $name(&mut self) -> Option<Expression> {
            let mut expr = self.$next()?;

            while self.match_($types) {
                let op = self.previous().clone();
                let right = self.$next()?;
                expr = Expression::Binary(
                    Box::new(expr), op, Box::new(right)
                );
            }

            Some(expr)
        }
    };
}

macro_rules! prefix_unary {
    ($name: tt, $next: tt, $types: expr) => {
        fn $name(&mut self) -> Option<Expression> {
            if self.match_($types) {
                let op = self.previous().clone();
                let right = self.$name()?;
                return Some(Expression::Unary(op, Box::new(right), true));
            }
    
            self.$next()
        }
    };
}

macro_rules! suffix_unary {
    ($name: tt, $next: tt, $types: expr) => {
        fn $name(&mut self) -> Option<Expression> {
            let expr = self.$next()?;
    
            if self.match_($types) {
                let op = self.previous().clone();
                return Some(Expression::Unary(op, Box::new(expr), false));
            }
    
            Some(expr)
        }
    };
}

pub struct Parser {
    tokens: Vec<Token>,
    curr_qualifiers: HashMap<Rc<str>, Token>,
    curr: usize,
    pub had_error: bool
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, curr_qualifiers: HashMap::new(), curr: 0, had_error: false }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.curr]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.curr - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_ == TokenType::EOF
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.curr += 1;
        }

        self.previous()
    }

    fn check(&self, type_: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().type_ == type_
    } 

    fn match_(&mut self, types: &[TokenType]) -> bool {
        for type_ in types {
            if self.check(*type_) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn syncronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().type_ == TokenType::Semicolon {
                return;
            }

            match self.peek().type_ {
                TokenType::Struct | TokenType::Fn | TokenType::If |
                TokenType::Return | TokenType::Let | TokenType::While |
                TokenType::Enum | TokenType::Import | TokenType::Defer | 
                TokenType::Impl | TokenType::Namespace | TokenType::Switch | 
                TokenType::Continue | TokenType::Break | TokenType::Do | 
                TokenType::Macro | TokenType::Use => return,
                _ => ()
            }

            self.advance();
        }
    }

    fn consume(&mut self, type_: TokenType, msg: &str) -> Option<&Token> {
        if self.check(type_) {
            Some(self.advance())
        } else {
            let tok = self.peek();
            token_error!(self, tok, msg);
            None
        }
    }

    fn fn_ptr(&mut self) -> Option<Expression> {
        let kw = self.previous().clone();
        self.consume(TokenType::LeftParen, "Expecting '(' after 'fn' in function pointer type")?;

        let mut params = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                let is_const = self.match_(&[TokenType::Const]);
                let type_ = self.expression()?;
                params.push(FunctionParam::new(None, type_, is_const));

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expecting ')' after function parameters")?;

        let return_type = self.type_expression()?;
        Some(Expression::FnPtr(kw, Box::new(return_type), params))
    }

    fn primary(&mut self) -> Option<Expression> {
        if self.match_(&[TokenType::Void]) {
            return Some(Expression::Literal(Rc::from(""), self.previous().clone(), LiteralKind::Void));
        }

        if self.match_(&[TokenType::Fn]) {
            return self.fn_ptr();
        }

        if self.match_(&[TokenType::Identifier]) {
            return Some(Expression::Variable(self.previous().clone()));
        }

        if self.match_(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Missing ')' after expression")?;
            return Some(Expression::Grouping(Box::new(expr)));
        }

        if self.match_(&[TokenType::LeftBrace]) {
            let opening_brace = self.previous().clone();

            let mut items = Vec::new();

            loop {
                items.push(self.expression()?);

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }

            self.consume(TokenType::RightBrace, "Missing '}' after slice")?;
            return Some(Expression::Slice(opening_brace, items));
        }

        if self.match_(&[TokenType::LeftSquare]) {
            let opening_brace = self.previous().clone();

            let mut items = Vec::new();

            loop {
                items.push(self.expression()?);

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }

            self.consume(TokenType::RightSquare, "Missing ']' after array")?;

            let mut method_tok = opening_brace.clone();
            method_tok.set_lexeme("core_DOT_Array_DOT_from");

            return Some(Expression::Call(
                Box::new(Expression::Variable(method_tok)), 
                opening_brace.clone(), 
                vec![Expression::Slice(opening_brace, items)]
            ));
        }

        match_literal!(self, U8);     match_literal!(self, I8);
        match_literal!(self, U16);    match_literal!(self, I16);
        match_literal!(self, U32);    match_literal!(self, I32); match_literal!(self, F32);
        match_literal!(self, U64);    match_literal!(self, I64); match_literal!(self, F64);
        match_literal!(self, Usz);
        match_literal!(self, AnyInt);    match_literal!(self, AnyFloat);
        match_literal!(self, RawString); match_literal!(self, String); match_literal!(self, Char);
        
        let last = self.peek();
        token_error!(self, last, "Expecting expression");

        None
    }

    fn static_access(&mut self) -> Option<Expression> {
        let mut expr = self.primary()?;

        while self.match_(&[TokenType::ColonColon]) {
            let name = self.consume(TokenType::Identifier, "Expecting property name after '::'")?.clone();
            expr = Expression::StaticGet(Box::new(expr), name);
        }

        Some(expr)
    }

    fn macro_call(&mut self) -> Option<Expression> {
        if self.match_(&[TokenType::At]) {
            let op = self.previous().clone();
            let name = self.consume(TokenType::Identifier, "Expecting macro name after '@'")?.clone();
            return Some(Expression::Unary(op, Box::new(Expression::Variable(name)), true));
        }

        self.static_access()
    }

    fn finish_call(&mut self, callee: Expression) -> Option<Expression> {
        let mut arguments = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expecting ')' after arguments.")?.clone();
        Some(Expression::Call(Box::new(callee), paren, arguments))
    }

    fn finish_subscript(&mut self, subscripted: Expression) -> Option<Expression> {
        let mut arguments = Vec::new();

        if !self.check(TokenType::RightSquare) {
            loop {
                arguments.push(self.expression()?);

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightSquare, "Expecting ']' after subscript operation")?.clone();
        Some(Expression::Subscript(Box::new(subscripted), paren, arguments))
    }

    fn finish_struct_literal(&mut self, expr: Expression) -> Option<Expression> {
        let mut fields = Vec::new();
        if !self.check(TokenType::RightBrace) {
            loop {
                let name = self.consume(TokenType::Identifier, "Expecting field name")?.clone();

                if self.match_(&[TokenType::Colon]) {
                    fields.push(StructField::new(name, self.expression()?, true));
                } else {
                    fields.push(StructField::new(name.clone(), Expression::Variable(name), true));
                }

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightBrace, "Expecting '}' after struct literal fields")?.clone();
        Some(Expression::CompoundLiteral(Box::new(expr), paren, fields))
    }

    fn call(&mut self) -> Option<Expression> {
        let mut expr = self.macro_call()?;

        loop {
            if self.match_(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_(&[TokenType::LeftSquare]) {
                expr = self.finish_subscript(expr)?;
            } else if self.match_(&[TokenType::Dot]) {
                if self.match_(&[TokenType::LeftBrace]) {
                    expr = self.finish_struct_literal(expr)?;
                } else {
                    let name = self.consume(TokenType::Identifier, "Expecting property name after '.'")?.clone();
                    expr = Expression::Get(Box::new(expr), name);
                }
            } else {
                break;
            }
        }

        Some(expr)
    }

    suffix_unary!(suffix_unary, call, &[TokenType::PlusPlus, TokenType::MinusMinus]);

    fn prefix_unary(&mut self) -> Option<Expression> {
        if self.match_(&[
            TokenType::PlusPlus, TokenType::MinusMinus, TokenType::Plus, 
            TokenType::Minus, TokenType::Tilde, TokenType::Star, 
            TokenType::Bang, TokenType::BitwiseAnd
        ]) {
            let mut op = self.previous().clone();

            let star = op.type_ == TokenType::Star;
            let and = op.type_ == TokenType::BitwiseAnd;

            if (star || and) && self.match_(&[TokenType::Const]) {
                if star {
                    op.set_type(TokenType::StarConst);
                } else if and {
                    op.set_type(TokenType::RefConst);
                }
            }

            let right = self.prefix_unary()?;
            return Some(Expression::Unary(op, Box::new(right), true));
        }

        self.suffix_unary()
    }

    left_associativity_binary!(factor, prefix_unary, &[TokenType::Slash, TokenType::Star, TokenType::Mod]);
    left_associativity_binary!(term, factor, &[TokenType::Minus, TokenType::Plus]);
    left_associativity_binary!(shift, term, &[TokenType::ShiftLeft, TokenType::ShiftRight]);
    left_associativity_binary!(comparison, shift, &[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]);
    left_associativity_binary!(equality, comparison, &[TokenType::BangEqual, TokenType::EqualEqual]);
    left_associativity_binary!(bitwise_and, equality, &[TokenType::BitwiseAnd]);
    left_associativity_binary!(bitwise_xor, bitwise_and, &[TokenType::BitwiseXor]);
    left_associativity_binary!(result, bitwise_xor, &[TokenType::Bang]);

    prefix_unary!(optional, result, &[TokenType::Question]);
    prefix_unary!(try_operator, optional, &[TokenType::Try]);

    left_associativity_binary!(bitwise_or, try_operator, &[TokenType::BitwiseOr]);
    left_associativity_binary!(logic_and, bitwise_or, &[TokenType::LogicAnd]);
    left_associativity_binary!(logic_or, logic_and, &[TokenType::LogicOr]);

    fn ternary(&mut self) -> Option<Expression> {
        let cond = self.logic_or()?;

        if !self.match_(&[TokenType::Question]) {
            return Some(cond);
        }

        let question = self.previous().clone();
        let then = self.expression()?;
        self.consume(TokenType::Colon, "Expecting ':' after 'then' branch of ternary expression")?;
        let else_ = self.expression()?;

        Some(Expression::Ternary(question, Box::new(cond), Box::new(then), Box::new(else_)))
    }

    fn assignment(&mut self) -> Option<Expression> {
        let expr = self.ternary()?;

        if self.match_(&[
            TokenType::Equal, TokenType::PlusEquals, TokenType::MinusEquals,
            TokenType::StarEquals, TokenType::SlashEquals, TokenType::ModEquals,
            TokenType::ShiftLeftEquals, TokenType::ShiftRightEquals, TokenType::AndEquals,
            TokenType::XorEquals, TokenType::OrEquals
        ]) {
            let op = self.previous().clone();
            let value = self.assignment()?;

            if !expr.is_valid_assignment_target() {
                token_error!(self, op, "Invalid assignment target");
                return None;
            }

            return Some(Expression::Assign(Box::new(expr), op, Box::new(value)));
        }

        Some(expr)
    }

    pub fn expression(&mut self) -> Option<Expression> {
        self.assignment()
    }

    pub fn type_expression(&mut self) -> Option<Expression> {
        self.bitwise_or()
    }

    pub fn switch_case_expression(&mut self) -> Option<Expression> {
        self.bitwise_xor()
    }

    fn expression_statement(&mut self) -> Option<Statement> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expecting ';' after expression")?;
        Some(Statement::Expression(expr))
    }

    fn block(&mut self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();

        while (!self.check(TokenType::RightBrace)) && (!self.is_at_end()) {
            statements.push(self.declaration(false, &Vec::new(), &Vec::new())?);
        }

        self.consume(TokenType::RightBrace, "Expecting '}' after block")?;
        Some(statements)
    }

    fn get_block(&mut self) -> Option<Statement> {
        let bracket = self.previous().clone();
        Some(Statement::Block(bracket, self.block()?))
    }

    fn if_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();

        let cond = self.expression()?;
        
        let then_branch = {
            if let Expression::Grouping(_) = cond {
                self.statement()?
            } else {
                self.consume(TokenType::LeftBrace, "Expecting '{' after if condition")?;
                self.get_block()?
            }
        };

        let else_branch = {
            if self.match_(&[TokenType::Else]) {
                Some(Box::new(self.statement()?))
            } else {
                None
            }
        };

        Some(Statement::If(kw, cond, Box::new(then_branch), else_branch))
    }

    fn while_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();

        let cond = self.expression()?;

        let body = {
            if let Expression::Grouping(_) = cond {
                if self.match_(&[TokenType::Semicolon]) {
                    Statement::Block(self.previous().clone(), Vec::new())
                } else {
                    self.statement()?
                }
            } else {
                self.consume(TokenType::LeftBrace, "Expecting '{' after while condition")?;
                self.get_block()?
            }
        };

        Some(Statement::While(kw, cond, Box::new(body)))
    }

    fn do_while_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();

        let body = self.statement()?;
        self.consume(TokenType::While, "Expecting 'while' after 'do' body")?;

        let cond = self.expression()?;
        self.consume(TokenType::Semicolon, "Expecting ';' after condition")?;

        Some(Statement::DoWhile(kw, cond, Box::new(body)))
    }

    fn for_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();

        let has_paren = self.match_(&[TokenType::LeftParen]);

        let initializer = {
            if self.match_(&[TokenType::Semicolon]) {
                None
            } else if self.match_(&[TokenType::Let, TokenType::Const]) {
                Some(Box::new(self.var_decl()?))
            } else {
                Some(Box::new(self.expression_statement()?))
            }
        };

        let cond = {
            if self.check(TokenType::Semicolon) {
                Expression::Literal(Rc::from("1"), self.previous().clone(), LiteralKind::U8)
            } else {
                self.expression()?
            }
        };

        if self.match_(&[TokenType::Semicolon]) { // C-like for
            let increment = {
                if has_paren {
                    let r = {
                        if self.check(TokenType::RightParen) {
                            None 
                        } else {
                            Some(self.expression()?)
                        }
                    };
    
                    self.consume(TokenType::RightParen, "Expecting ')' after for increment")?;
                    r
                } else if self.check(TokenType::LeftBrace) {
                    None
                } else {
                    Some(self.expression()?)
                }
            };
    
            let body = {
                if has_paren {
                    if self.match_(&[TokenType::Semicolon]) {
                        Statement::Block(kw.clone(), Vec::new())
                    } else {
                        self.statement()?
                    }
                } else {
                    self.consume(TokenType::LeftBrace, "Expecting '{' after for")?;
                    self.get_block()?
                }
            };
    
            Some(Statement::For(kw, initializer, cond, increment, Box::new(body)))
        } else { // foreach
            let body = {
                if has_paren {
                    self.consume(TokenType::RightParen, "Expecting ')' after for iterator")?;

                    if self.match_(&[TokenType::Semicolon]) {
                        Statement::Block(kw.clone(), Vec::new())
                    } else {
                        self.statement()?
                    }
                } else {
                    self.consume(TokenType::LeftBrace, "Expecting '{' after for")?;
                    self.get_block()?
                }
            };

            if let Some(init) = &initializer {
                if let Statement::Expression(Expression::Variable(var_name)) = &**init {
                    return Some(Statement::Foreach(kw, var_name.clone(), cond, Box::new(body)))
                } else {
                    ast_error!(self, init, "Expecting variable name in foreach loop");
                }
            } else {
                token_error!(self, kw, "Expecting variable name in foreach loop");
            }

            None
        }
    }

    fn return_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();

        let value = {
            if self.check(TokenType::Semicolon) {
                None
            } else {
                Some(self.expression()?)
            }
        };

        self.consume(TokenType::Semicolon, "Expecting ';' after return value")?;
        Some(Statement::Return(kw, value))
    }

    fn defer_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();
        Some(Statement::Defer(kw, Box::new(self.statement()?)))
    }

    fn switch_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();
        let expr = self.expression()?;

        self.consume(TokenType::LeftBrace, "Expecting '{' before switch body")?;
        let mut cases = Vec::new();
        if !self.check(TokenType::RightBrace) {
            loop {
                let expressions = {
                    if self.match_(&[TokenType::Default]) {
                        None
                    } else {
                        let mut exprs = Vec::new();
                        loop {
                            exprs.push(self.switch_case_expression()?);

                            if !self.match_(&[TokenType::BitwiseOr]) {
                                break;
                            }
                        }

                        Some(exprs)
                    }
                };

                self.consume(TokenType::LeftBrace, "Expecting '{' after case body")?;
                cases.push(SwitchCase::new(expressions, self.block()?));

                if self.check(TokenType::RightBrace) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightBrace, "Expecting '}' after switch body")?;
        Some(Statement::Switch(kw, expr, cases))
    }

    fn break_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();
        self.consume(TokenType::Semicolon, "Expecting ';' after break")?;
        Some(Statement::Break(kw))
    }

    fn continue_statement(&mut self) -> Option<Statement> {
        let kw = self.previous().clone();
        self.consume(TokenType::Semicolon, "Expecting ';' after continue")?;
        Some(Statement::Continue(kw))
    }

    fn statement(&mut self) -> Option<Statement> {
        if self.curr_qualifiers.len() != 0 {
            let kw = self.peek();
            token_error!(self, kw, "Can only use qualifiers on declarations");

            for (_, qualifier) in self.curr_qualifiers.iter() {
                token_note!(qualifier, "Qualifier specified here");
            }

            self.curr_qualifiers.clear();
        }

        if self.match_(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_(&[TokenType::While]) {
            return self.while_statement();
        }

        if self.match_(&[TokenType::Do]) {
            return self.do_while_statement();
        }

        if self.match_(&[TokenType::For]) {
            return self.for_statement();
        }

        if self.match_(&[TokenType::Return]) {
            return self.return_statement();
        }

        if self.match_(&[TokenType::Defer]) {
            return self.defer_statement();
        }

        if self.match_(&[TokenType::Switch]) {
            return self.switch_statement();
        }

        if self.match_(&[TokenType::Break]) {
            return self.break_statement();
        }

        if self.match_(&[TokenType::Continue]) {
            return self.continue_statement();
        }

        if self.match_(&[TokenType::LeftBrace]) {
            return self.get_block();
        }

        self.expression_statement()
    }

    fn var_decl(&mut self) -> Option<Statement> {
        let mut qualifiers = Vec::new();

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "static" | "extern" | "volatile" => qualifiers.push(qualifier.clone()),
                _ => token_error!(self, qualifier, "Unsupported qualifier for variable declaration")
            }
        }

        self.curr_qualifiers.clear();

        let kw = self.previous().clone();
        let name = self.consume(TokenType::Identifier, "Expecting variable name")?.clone();

        let type_ = {
            if self.match_(&[TokenType::Colon]) {
                Some(self.type_expression()?) 
            } else {
                None
            }
        };

        let initializer = {
            if self.match_(&[TokenType::Equal]) {
                Some(self.expression()?)
            } else {
                None
            }
        };

        self.consume(TokenType::Semicolon, "Expecting ';' after variable declaration")?;
        Some(Statement::VarDecl(name, initializer, type_, kw.type_ == TokenType::Const, qualifiers))
    }

    fn parse_generics(&mut self, incoming_generics: &Vec<Generic>) -> Option<Vec<Generic>> {
        let mut generics = Vec::new();
        generics.append(&mut incoming_generics.clone());

        let mut had_default = false;
        let mut has_default = false;
        if self.match_(&[TokenType::LeftSquare]) {
            loop {
                let name = self.consume(TokenType::Identifier, "Expecting generic type name")?.clone();

                let bounds = {
                    if self.match_(&[TokenType::Colon]) {
                        Some(self.type_expression()?)
                    } else {
                        None
                    }
                };

                let default = {
                    if self.match_(&[TokenType::Equal]) {
                        if had_default && !has_default {
                            token_error!(self, name, "Cannot alternate generic with no default type with generic with default type");
                            None
                        } else {
                            has_default = true;
                            had_default = true;
                            Some(self.expression()?)
                        }
                    } else {
                        has_default = false;
                        None
                    }
                };

                generics.push(Generic::new(name, bounds, default));

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }

            self.consume(TokenType::RightSquare, "Expecting ']' after generic types declaration")?;
        }

        Some(generics)
    }

    fn function(&mut self, method: bool, incoming_generics: &Vec<Generic>, self_generics: &Vec<Expression>) -> Option<Statement> {
        let mut qualifiers = Vec::new();
        let mut bind = false;

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "static" | "extern" | "inline" => qualifiers.push(qualifier.clone()),
                "bind" => bind = true,
                _ => token_error!(self, qualifier, "Unsupported qualifier for function definition")
            }
        }

        self.curr_qualifiers.clear();

        let name = self.consume(TokenType::Identifier, "Expecting function name")?.clone();
        let generics = self.parse_generics(incoming_generics)?;
        if bind && generics.len() != 0 {
            token_error!(self, self.previous(), "Generics are not allowed in function bindings");
        }

        self.consume(TokenType::LeftParen, "Expecting '(' after function name")?;

        let mut params = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                let is_const = self.match_(&[TokenType::Const]);
                let p_name = self.consume(TokenType::Identifier, "Expecting parameter name")?.clone();
                
                if method && p_name.lexeme.as_ref() == "self" {
                    if self.match_(&[TokenType::Colon]) {
                        let type_expr = self.expression()?;
                        ast_error!(self, type_expr, "Type is not required for \"self\" inside methods");
                        ast_note!(type_expr, "Remove the type");
                    }

                    let mut custom_tok = p_name.clone();
                    custom_tok.set_lexeme("Self");
                    let mut type_ = Expression::Variable(custom_tok.clone());

                    if self_generics.len() != 0 {
                        type_ = Expression::Subscript(Box::new(type_), custom_tok, self_generics.clone());
                    }

                    let mut star_tok = p_name.clone();
                    if is_const {
                        star_tok.set_type(TokenType::StarConst);
                    } else {
                        star_tok.set_type(TokenType::Star)
                    }

                    type_ = Expression::Unary(star_tok, Box::new(type_), true);
                    params.push(FunctionParam::new(Some(p_name), type_, true));
                } else {
                    self.consume(TokenType::Colon, "Expecting ':' after parameter name")?;
                    let type_ = self.expression()?;
                    
                    params.push(FunctionParam::new(Some(p_name), type_, is_const));
                }

                if !self.match_(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expecting ')' after function parameters")?;

        let return_type = {
            if !(self.check(TokenType::LeftBrace) || self.check(TokenType::Semicolon)) {
                self.expression()?
            } else {
                Expression::Literal(Rc::from(""), self.previous().clone(), LiteralKind::Void)
            }
        };

        let body = {
            if self.match_(&[TokenType::LeftBrace]) {
                if bind {
                    token_error!(self, self.previous(), "Cannot define function body for binding");
                }

                Some(self.block()?)
            } else {
                self.consume(TokenType::Semicolon, "Expecting '{' or ';' after function declaration")?;
                None
            }
        };

        if generics.len() == 0 {
            Some(Statement::Function(name, params, return_type, body, qualifiers, Vec::new(), bind))
        } else {
            let mut generic_names = Vec::new();
            for generic in &generics {
                generic_names.push(generic.name.clone());
            }

            Some(Statement::Template(
                name.clone(), 
                Box::new(Statement::Function(name, params, return_type, body, qualifiers, generic_names.clone(), false)),
                generics, generic_names
            ))
        }
    }

    fn struct_decl(&mut self, incoming_generics: &Vec<Generic>) -> Option<Statement> {
        let mut typedefed = false;

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "typedef" => typedefed = true,
                _ => token_error!(self, qualifier, "Unsupported qualifier for struct definition")
            }
        }

        self.curr_qualifiers.clear();

        let name = self.consume(TokenType::Identifier, "Expecting struct name")?.clone();
        let generics = self.parse_generics(incoming_generics)?;

        let binding = {
            if self.match_(&[TokenType::Colon]) {
                if generics.len() != 0 {
                    token_error!(self, self.previous(), "Cannot use generics in a C struct binding");
                    None
                } else {
                    Some(self.consume(TokenType::Identifier, "Expecting C struct name after struct binding")?.clone())
                }
            } else {
                if typedefed {
                    token_error!(self, self.previous(), "Cannot use #typedef qualifier on struct that is not a binding");
                }

                None
            }
        };

        let mut fields = Vec::new();

        let has_body = {
            if self.match_(&[TokenType::LeftBrace]) {
                if !self.check(TokenType::RightBrace) {
                    loop {
                        let is_const = self.match_(&[TokenType::Const]);
                        let field_name = self.consume(TokenType::Identifier, "Expecting field name")?.clone();
                        self.consume(TokenType::Colon, "Expecting ':' after field name")?;
                        let type_ = self.type_expression()?;

                        fields.push(StructField::new(field_name, type_, is_const));

                        if !self.match_(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
        
                self.consume(TokenType::RightBrace, "Expecting '}' after struct body")?;
                true
            } else {
                self.consume(TokenType::Semicolon, "Expecting '{' or ';' after struct declaration")?;
                false
            }
        };
        
        if generics.len() == 0 {
            Some(Statement::Struct(name, fields, has_body, binding, Vec::new(), typedefed))
        } else {
            let mut generic_names = Vec::new();
            for generic in &generics {
                generic_names.push(generic.name.clone());
            }

            Some(Statement::Template(
                name.clone(), 
                Box::new(Statement::Struct(name, fields, has_body, binding, generic_names.clone(), typedefed)),
                generics, generic_names
            ))
        } 
    }

    fn impl_decl(&mut self) -> Option<Statement> {
        if self.curr_qualifiers.len() != 0 {
            let kw = self.previous();
            token_error!(self, kw, "Cannot use qualifiers on \"impl\" statement");

            for (_, qualifier) in self.curr_qualifiers.iter() {
                token_note!(qualifier, "Qualifier specified here");
            }

            self.curr_qualifiers.clear();
        }

        let generics = self.parse_generics(&Vec::new())?;

        let mut struct_impl = self.type_expression()?;

        let self_generics = {
            if let Expression::Subscript(subscripted, _, self_generics) = struct_impl {
                struct_impl = *subscripted;
                self_generics
            } else {
                Vec::new()
            }
        };

        self.consume(TokenType::LeftBrace, "Expecting '{' before impl body")?;

        let mut declarations = Vec::new();
        while (!self.check(TokenType::RightBrace)) && (!self.is_at_end()) {
            declarations.push(self.declaration(true, &generics, &self_generics)?);
        }

        self.consume(TokenType::RightBrace, "Expecting '}' after impl body")?;
        Some(Statement::Impl(struct_impl, declarations))
    }

    fn namespace(&mut self) -> Option<Statement> {
        if self.curr_qualifiers.len() != 0 {
            let kw = self.previous();
            token_error!(self, kw, "Cannot use qualifiers on \"namespace\" statement");

            for (_, qualifier) in self.curr_qualifiers.iter() {
                token_note!(qualifier, "Qualifier specified here");
            }

            self.curr_qualifiers.clear();
        }

        let name = self.consume(TokenType::Identifier, "Expecting namespace name")?.clone();
        self.consume(TokenType::LeftBrace, "Expecting '{' before namespace body")?;
        Some(Statement::Namespace(name, self.block()?))
    }

    fn use_statement(&mut self) -> Option<Statement> {
        if self.curr_qualifiers.len() != 0 {
            let kw = self.previous();
            token_error!(self, kw, "Cannot use qualifiers on \"use\" statement");

            for (_, qualifier) in self.curr_qualifiers.iter() {
                token_note!(qualifier, "Qualifier specified here");
            }

            self.curr_qualifiers.clear();
        }

        let use_expr = self.expression()?;
        self.consume(TokenType::As, "Expecting \"as\" after use expression")?;
        let as_ = self.consume(TokenType::Identifier, "Expecting identifier after \"as\"")?.clone();
        self.consume(TokenType::Semicolon, "Expecting ';' after use statement")?;
        Some(Statement::Use(use_expr, as_))
    }

    fn enum_decl(&mut self, incoming_generics: &Vec<Generic>) -> Option<Statement> {
        let mut typedefed = false;

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "typedef" => typedefed = true,
                _ => token_error!(self, qualifier, "Unsupported qualifier for enum definition")
            }
        }

        self.curr_qualifiers.clear();

        let name = self.consume(TokenType::Identifier, "Expecting enum name")?.clone();
        let generics = self.parse_generics(incoming_generics)?;

        let binding = {
            if self.match_(&[TokenType::Colon]) {
                if generics.len() != 0 {
                    token_error!(self, self.previous(), "Cannot use generics in a C enum binding");
                    None
                } else {
                    Some(self.consume(TokenType::Identifier, "Expecting C enum name after enum binding")?.clone())
                }
            } else {
                if typedefed {
                    token_error!(self, self.previous(), "Cannot use #typedef qualifier on enum that is not a binding");
                }

                None
            }
        };

        let type_ = {
            if self.match_(&[TokenType::As]) {
                self.type_expression()?
            } else {
                let mut custom_tok = name.clone();
                custom_tok.set_lexeme("i32");
                Expression::Variable(custom_tok)
            }
        };

        let mut variants = Vec::new();
        let mut is_simple = true;

        let has_body = {
            if self.match_(&[TokenType::LeftBrace]) {
                if !self.check(TokenType::RightBrace) {
                    loop {
                        let variant_name = self.consume(TokenType::Identifier, "Expecting field name")?.clone();
                        
                        let type_ = {
                            if self.match_(&[TokenType::LeftParen]) {
                                let res = self.expression()?;

                                if is_simple {
                                    if let Expression::Literal(.., kind) = &res {
                                        is_simple = *kind == LiteralKind::Void;
                                    } else {
                                        is_simple = false;
                                    }
                                }

                                self.consume(TokenType::RightParen, "Expecting ')' after enum variant type")?;
                                res
                            } else {
                                Expression::Literal(Rc::from(""), variant_name.clone(), LiteralKind::Void)
                            }
                        };
        
                        variants.push(EnumVariant::new(variant_name, type_));
        
                        if !self.match_(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                self.consume(TokenType::RightBrace, "Expecting '}' after enum body")?;
                true
            } else {
                self.consume(TokenType::Semicolon, "Expecting '{' or ';' after enum declaration")?;
                false
            }
        };

        if !is_simple {
            if binding.is_some() {
                token_error!(self, binding.as_ref().unwrap(), "Cannot use enum as sum type when creating a C binding");
            }

            for variant in &variants {
                if !is_valid_variant(&variant.name.lexeme) {
                    token_error!(self, variant.name, "Enum variant names must be PascalCase or UPPER_SNAKE_CASE when the enum acts as a sum type");
                    token_note!(variant.name, format!("Rename this variant to \"{}\"", variant.name.lexeme.as_ref().to_case(Case::Pascal)).as_ref());
                }
            }
        }

        if generics.len() == 0 {
            Some(Statement::Enum(name, type_, variants, is_simple, has_body, binding, Vec::new(), typedefed))
        } else {
            let mut generic_names = Vec::new();
            for generic in &generics {
                generic_names.push(generic.name.clone());
            }

            Some(Statement::Template(
                name.clone(), 
                Box::new(Statement::Enum(name, type_, variants, is_simple, has_body, binding, generic_names.clone(), typedefed)),
                generics, generic_names
            ))
        }        
    }

    fn import_statement(&mut self) -> Option<Statement> {
        let import_type = {
            if self.match_(&[TokenType::Less]) {
                ImportType::Ang
            } else if self.match_(&[TokenType::ShiftLeft]) {
                ImportType::Lib
            } else {
                ImportType::Default
            }
        };

        let path = {
            if let Some(tok) = self.consume(TokenType::String, "Expecting path in string format after \"import\"") {
                tok.clone()
            } else {
                token_note!(self.peek(), "Add quotation marks around the file");
                return None;
            }
        };

        match import_type {
            ImportType::Default => (),
            ImportType::Ang => {
                self.consume(TokenType::Greater, "Expecting '>' after import with angular brackets")?;
            }
            ImportType::Lib => {
                self.consume(TokenType::ShiftRight, "Expecting '>>' after Skye lib C import")?;
            }
        }

        self.consume(TokenType::Semicolon, "Expecting ';' after import statement")?;
        Some(Statement::Import(path, import_type))
    }

    fn union_decl(&mut self) -> Option<Statement> {
        let mut typedefed = false;

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "typedef" => typedefed = true,
                _ => token_error!(self, qualifier, "Unsupported qualifier for union definition")
            }
        }

        self.curr_qualifiers.clear();

        let name = self.consume(TokenType::Identifier, "Expecting union name")?.clone();
        if self.parse_generics(&Vec::new())?.len() != 0 {
            token_error!(self, self.previous(), "Generics are not allowed in unions");
        }

        let binding = {
            if self.match_(&[TokenType::Colon]) {
                Some(self.consume(TokenType::Identifier, "Expecting C union name after union binding")?.clone())
            } else {
                if typedefed {
                    token_error!(self, self.previous(), "Cannot use #typedef qualifier on union that is not a binding");
                }

                None
            }
        };

        let mut fields = Vec::new();

        let has_body = {
            if self.match_(&[TokenType::LeftBrace]) {
                if !self.check(TokenType::RightBrace) {
                    loop {
                        let field_name = self.consume(TokenType::Identifier, "Expecting field name")?.clone();
                        self.consume(TokenType::Colon, "Expecting ':' after field name")?;
                        let type_ = self.type_expression()?;

                        fields.push(StructField::new(field_name, type_, false));

                        if !self.match_(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
        
                self.consume(TokenType::RightBrace, "Expecting '}' after union body")?;
                true
            } else {
                self.consume(TokenType::Semicolon, "Expecting '{' or ';' after union declaration")?;
                false
            }
        };
        
        Some(Statement::Union(name, fields, has_body, binding, typedefed))
    }

    fn bitfield_decl(&mut self) -> Option<Statement> {
        let mut typedefed = false;

        for (name, qualifier) in self.curr_qualifiers.iter() {
            match name.as_ref() {
                "typedef" => typedefed = true,
                _ => token_error!(self, qualifier, "Unsupported qualifier for union definition")
            }
        }

        self.curr_qualifiers.clear();

        let name = self.consume(TokenType::Identifier, "Expecting bitfield name")?.clone();
        if self.parse_generics(&Vec::new())?.len() != 0 {
            token_error!(self, self.previous(), "Generics are not allowed in bitfields");
        }

        let binding = {
            if self.match_(&[TokenType::Colon]) {
                Some(self.consume(TokenType::Identifier, "Expecting C bitfield name after bitfield binding")?.clone())
            } else {
                if typedefed {
                    token_error!(self, self.previous(), "Cannot use #typedef qualifier on bitfield that is not a binding");
                }

                None
            }
        };

        let mut fields = Vec::new();

        let has_body = {
            if self.match_(&[TokenType::LeftBrace]) {
                if !self.check(TokenType::RightBrace) {
                    loop {
                        let field_name = self.consume(TokenType::Identifier, "Expecting field name")?.clone();
                        self.consume(TokenType::Colon, "Expecting ':' after field name")?;
                        let bits_tok = self.consume(TokenType::AnyInt, "Expecting integer containing field size after field name")?.clone();

                        if let Ok(bits) = bits_tok.lexeme.parse::<u8>() {
                            if bits > 64 {
                                token_error!(self, bits_tok, "Invalid field size");
                                token_note!(bits_tok, "Field size should be an integer [0, 64]");
                            } else {
                                fields.push(BitfieldField::new(field_name, bits));
                            }
                        } else {
                            token_error!(self, bits_tok, "Invalid field size");
                            token_note!(bits_tok, "Field size should be an integer [0, 64]");
                        }

                        if !self.match_(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
        
                self.consume(TokenType::RightBrace, "Expecting '}' after bitfield body")?;
                true
            } else {
                self.consume(TokenType::Semicolon, "Expecting '{' or ';' after bitfield declaration")?;
                false
            }
        };
        
        Some(Statement::Bitfield(name, fields, has_body, binding, typedefed))
    }

    fn macro_decl(&mut self) -> Option<Statement> {
        if self.curr_qualifiers.len() != 0 {
            let kw = self.previous();
            token_error!(self, kw, "Cannot use qualifiers on \"macro\" statement");

            for (_, qualifier) in self.curr_qualifiers.iter() {
                token_note!(qualifier, "Qualifier specified here");
            }

            self.curr_qualifiers.clear();
        }

        let name = self.consume(TokenType::Identifier, "Expecting macro name")?.clone();

        let params = {
            if self.match_(&[TokenType::LeftParen]) {
                let mut params = Vec::new();
                if !self.check(TokenType::RightParen) {
                    loop {
                        params.push(self.consume(TokenType::Identifier, "Expecting parameter name")?.clone());

                        if !self.match_(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                let ret = {
                    if params.len() == 1 && self.match_(&[TokenType::Star]) {
                        MacroParams::Variable(params[0].clone())
                    } else {
                        MacroParams::Some(params)
                    }
                };

                self.consume(TokenType::RightParen, "Expecting ')' after macro parameters")?;
                ret
            } else {
                MacroParams::None
            }
        };

        let (return_type, return_expr) = {
            if self.match_(&[TokenType::Arrow]) {
                let type_expr = self.type_expression()?;
                self.consume(TokenType::Semicolon, "Expecting ';' after macro binding")?;
                (Some(type_expr), None)
            } else if self.match_(&[TokenType::LeftBrace]) {
                let expr = self.expression()?;
                self.consume(TokenType::RightBrace, "Expecting '}' after macro body")?;
                (None, Some(expr))
            } else {
                let expr = self.expression()?;
                self.consume(TokenType::Semicolon, "Expecting ';' after macro expression")?;
                (None, Some(expr))
            }
        };

        Some(Statement::Macro(name, params, return_expr, return_type))
    }

    fn declaration(&mut self, method: bool, incoming_generics: &Vec<Generic>, self_generics: &Vec<Expression>) -> Option<Statement> {
        if self.match_(&[TokenType::Fn]) {
            return self.function(method, incoming_generics, self_generics);
        }

        if self.match_(&[TokenType::Let, TokenType::Const]) {
            return self.var_decl();
        }

        if self.match_(&[TokenType::Struct]) {
            return self.struct_decl(incoming_generics);
        }

        if self.match_(&[TokenType::Impl]) {
            return self.impl_decl();
        }

        if self.match_(&[TokenType::Namespace]) {
            return self.namespace();
        }

        if self.match_(&[TokenType::Use]) {
            return self.use_statement();
        }

        if self.match_(&[TokenType::Enum]) {
            return self.enum_decl(incoming_generics);
        }

        if self.match_(&[TokenType::Import]) {
            return self.import_statement();
        }

        if self.match_(&[TokenType::Union]) {
            return self.union_decl();
        }

        if self.match_(&[TokenType::Bitfield]) {
            return self.bitfield_decl();
        }

        if self.match_(&[TokenType::Macro]) {
            return self.macro_decl();
        }

        if self.match_(&[TokenType::Hash]) {
            let name = self.consume(TokenType::Identifier, "Expecting qualifier name after '#'")?.clone();

            if let Some(old_tok) = self.curr_qualifiers.get(&name.lexeme) {
                token_error!(self, name, "Cannot use same qualifier twice");
                token_note!(old_tok, "Previously specified here");
            } else {
                self.curr_qualifiers.insert(Rc::clone(&name.lexeme), name);
            }
            
            return self.declaration(method, incoming_generics, self_generics);
        }

        self.statement()
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            if let Some(stmt) = self.declaration(false, &Vec::new(), &Vec::new()) {
                statements.push(stmt);
            } else {
                self.syncronize();
            }

            self.curr_qualifiers.clear();
        }

        statements
    }
}