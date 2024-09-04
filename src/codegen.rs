use std::{cell::RefCell, collections::HashMap, env, ffi::OsString, path::{Path, PathBuf}, rc::Rc};

use crate::{
    ast::{Expression, FunctionParam, ImportType, LiteralKind, Statement}, ast_error, ast_info, ast_note, ast_warning, environment::{Environment, SkyeVariable}, parse_file, skye_type::{EqualsLevel, GetResult, ImplementsHow, Operator, SkyeEnumVariant, SkyeFunctionParam, SkyeGeneric, SkyeType}, token_error, token_note, token_warning, tokens::{Token, TokenType}, utils::{fix_raw_string, get_real_string_length}, SKYE_PATH_VAR
};

const OUTPUT_INDENT_SPACES: usize = 4;

const VOID_MAIN: &str = concat!(
    "int main() {\n",
    "    _SKYE_INIT();\n",
    "    _SKYE_MAIN();\n",
    "    return 0;\n",
    "}\n\n"
);
const VOID_MAIN_PLUS_STD_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    _SKYE_MAIN(argc, argv);\n",
    "    return 0;\n",
    "}\n\n"
);
const VOID_MAIN_PLUS_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    _SKYE_MAIN(_SKYE_CONVERT_ARGS(argc, argv));\n",
    "    return 0;\n",
    "}\n\n"
);
const RESULT_VOID_MAIN: &str = concat!(
    "int main() {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_void_GENEND_ result = _SKYE_MAIN();\n",
    "    return result.kind != core_DOT_Result_DOT_Kind_DOT_Ok;\n",
    "}\n\n"
);
const RESULT_VOID_MAIN_PLUS_STD_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_void_GENEND_ result = _SKYE_MAIN(argc, argv);\n",
    "    return result.kind != core_DOT_Result_DOT_Kind_DOT_Ok;\n",
    "}\n\n"
);
const RESULT_VOID_MAIN_PLUS_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_void_GENEND_ result = _SKYE_MAIN(_SKYE_CONVERT_ARGS(argc, argv));\n",
    "    return result.kind != core_DOT_Result_DOT_Kind_DOT_Ok;\n",
    "}\n\n"
);
const RESULT_I32_MAIN: &str = concat!(
    "int main() {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_i32_GENEND_ result = _SKYE_MAIN();\n",
    "    if (result.kind == core_DOT_Result_DOT_Kind_DOT_Ok) return 0;\n",
    "    return result.error;\n",
    "}\n\n"
);
const RESULT_I32_MAIN_PLUS_STD_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_i32_GENEND_ result = _SKYE_MAIN(argc, argv);\n",
    "    if (result.kind == core_DOT_Result_DOT_Kind_DOT_Ok) return 0;\n",
    "    return result.error;\n",
    "}\n\n"
);
const RESULT_I32_MAIN_PLUS_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    core_DOT_Result_GENOF_void_GENAND_i32_GENEND_ result = _SKYE_MAIN(_SKYE_CONVERT_ARGS(argc, argv));\n",
    "    if (result.kind == core_DOT_Result_DOT_Kind_DOT_Ok) return 0;\n",
    "    return result.error;\n",
    "}\n\n"
);
const I32_MAIN: &str = concat!(
    "int main() {\n",
    "    _SKYE_INIT();\n",
    "    return _SKYE_MAIN();\n",
    "}\n\n"
);
const I32_MAIN_PLUS_STD_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    return _SKYE_MAIN(argc, argv);\n",
    "}\n\n"
);
const I32_MAIN_PLUS_ARGS: &str = concat!(
    "int main(int argc, char** argv) {\n",
    "    _SKYE_INIT();\n",
    "    return _SKYE_MAIN(_SKYE_CONVERT_ARGS(argc, argv));\n",
    "}\n\n"
);

struct SkyeValue {
    value: Rc<str>,
    type_: SkyeType,
    is_const: bool,
    pub self_info: Option<(Rc<str>, SkyeType)>
}

impl SkyeValue {
    pub fn new(value: Rc<str>, type_: SkyeType, is_const: bool) -> Self {
        SkyeValue { value, type_, is_const, self_info: None }
    }

    pub fn special(type_: SkyeType) -> Self {
        SkyeValue { value: Rc::from(""), type_, is_const: true, self_info: None }
    }

    pub fn with_self_info(value: Rc<str>, type_: SkyeType, is_const: bool, self_info: (Rc<str>, SkyeType)) -> Self {
        SkyeValue { value, type_, is_const, self_info: Some(self_info) }
    }
}

#[derive(Clone, Debug)]
enum CurrentFn {
    None,
    Some(SkyeType, Expression)
}

#[derive(Clone, Debug)]
pub struct CodeOutput {
    pub code: String,
    indent: usize
}

impl CodeOutput {
    pub fn new() -> Self {
        CodeOutput { code: String::new(), indent: 0 }
    }

    pub fn push_indent(&mut self) {
        for _ in 0 .. self.indent * OUTPUT_INDENT_SPACES {
            self.code.push(' ');
        }
    }

    pub fn push(&mut self, string: &str) {
        self.code.push_str(string);
    }

    pub fn inc_indent(&mut self) {
        self.indent += 1;
    }

    pub fn dec_indent(&mut self) {
        self.indent -= 1;
    }

    pub fn get_indent(&self) -> usize {
        self.indent
    }

    pub fn set_indent(&mut self, indent: usize) {
        self.indent = indent;
    }
}

pub enum ExecutionInterrupt {
    Error,
    Interrupt(Rc<str>),
    Return(Rc<str>)
}

pub struct CodeGen {
    source_path: Option<Box<PathBuf>>,

    strings:            HashMap<Rc<str>, usize>,
    strings_code:       CodeOutput,
    includes:           CodeOutput,
    declarations:       Vec<CodeOutput>,
    struct_definitions: HashMap<Rc<str>, CodeOutput>,
    struct_defs_order:  Vec<Rc<str>>,
    definitions:        Vec<CodeOutput>,
    string_type:        Option<SkyeType>,
    tmp_var_cnt:        usize,

    globals:     Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,

    deferred:      Rc<RefCell<Vec<Vec<Statement>>>>,
    curr_function: CurrentFn,
    curr_name:     String,
    curr_loop:     Option<Rc<str>>,

    had_error: bool
}

impl CodeGen {
    pub fn new(path: Option<&Path>) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));

        let cloned = Rc::clone(&globals);
        let mut globals_ref = cloned.borrow_mut();
        globals_ref.define(Rc::from("u8"),  SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::U8)),  true, None));
        globals_ref.define(Rc::from("i8"),  SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::I8)),  true, None));
        globals_ref.define(Rc::from("u16"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::U16)), true, None));
        globals_ref.define(Rc::from("i16"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::I16)), true, None));
        globals_ref.define(Rc::from("u32"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::U32)), true, None));
        globals_ref.define(Rc::from("i32"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::I32)), true, None));
        globals_ref.define(Rc::from("f32"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::F32)), true, None));
        globals_ref.define(Rc::from("u64"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::U64)), true, None));
        globals_ref.define(Rc::from("i64"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::I64)), true, None));
        globals_ref.define(Rc::from("f64"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::F64)), true, None));
        globals_ref.define(Rc::from("usz"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::Usz)), true, None));
        
        globals_ref.define(Rc::from("char"),      SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::Char)),      true, None));
        globals_ref.define(Rc::from("rawstring"), SkyeVariable::new(SkyeType::Type(Box::new(SkyeType::RawString)), true, None));

        globals_ref.define(
            Rc::from("voidptr"), 
            SkyeVariable::new(
                SkyeType::Type(
                    Box::new(SkyeType::Pointer(
                        Box::new(SkyeType::Void), false
                    ))
                ), 
                true, None
            )
        );

        CodeGen { 
            definitions: Vec::new(), 
            struct_definitions: HashMap::new(),
            struct_defs_order: Vec::new(),
            declarations: Vec::new(),
            strings_code: CodeOutput::new(),
            includes: CodeOutput::new(),
            strings: HashMap::new(),
            curr_name: String::new(),
            environment: Rc::clone(&globals), 
            deferred: Rc::new(RefCell::new(Vec::new())),
            curr_function: CurrentFn::None, 
            string_type: None, tmp_var_cnt: 0,
            curr_loop: None, had_error: false, 
            globals, source_path: {
                if let Some(real_path) = path {
                    Some(Box::new(PathBuf::from(real_path)))
                } else {
                    None
                }
            }
        }
    }

    fn get_name(&self, name: &Rc<str>) -> Rc<str> {
        if self.curr_name == "" {
            Rc::clone(&name)
        } else {
            Rc::from(format!("{}_DOT_{}", self.curr_name, name))
        }
    }

    fn get_generics(&self, name: &Rc<str>, generics: &Vec<Token>, env: &Rc<RefCell<Environment>>) -> Result<Rc<str>, ExecutionInterrupt> {
        if generics.len() == 0 {
            Ok(Rc::clone(name))
        } else {
            let mut buf = String::new();
            buf.push_str(name);
            buf.push_str("_GENOF_");

            for (i, generic) in generics.iter().enumerate() {
                if let Some(var) = env.borrow().get(generic) {
                    match var.type_ {
                        SkyeType::Type(inner_type) => buf.push_str(&inner_type.mangle()),
                        SkyeType::Void => buf.push_str("void"),
                        SkyeType::Unknown(_) => buf.push_str("_UNKNOWN_"),
                        _ => return Err(ExecutionInterrupt::Error)
                    }
                } else {
                    return Err(ExecutionInterrupt::Error);
                }

                if i != generics.len() - 1 {
                    buf.push_str("_GENAND_");
                }
            }

            buf.push_str("_GENEND_");
            Ok(Rc::from(buf))
        }
    }

    fn get_return_type(&mut self, return_type_expr: &Expression, index: usize, allow_unknown: bool) -> Result<SkyeType, ExecutionInterrupt> {
        let val = self.evaluate(return_type_expr, index, allow_unknown)?;

        match val.type_ {
            SkyeType::Type(inner_type) => {
                if inner_type.check_completeness() {
                    Ok(*inner_type)
                } else {
                    ast_error!(self, return_type_expr, "Cannot use incomplete type directly");
                    ast_note!(return_type_expr, "Define this type or reference it through a pointer");
                    Err(ExecutionInterrupt::Error)
                }
            }
            SkyeType::Void => Ok(val.type_),
            _ => {
                ast_error!(self, return_type_expr, format!("Expecting type as return type (got {})", val.type_.stringify_native()).as_ref());
                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn get_params(&mut self, params: &Vec<FunctionParam>, existing: Option<SkyeVariable>, has_decl: bool, index: usize, allow_unknown: bool) -> Result<(String, Vec<SkyeFunctionParam>), ExecutionInterrupt> {
        let mut params_string = String::new();
        let mut params_output = Vec::new();
        for i in 0 .. params.len() {
            let param_type: SkyeType = {
                let inner_param_type = self.evaluate(&params[i].type_, index, allow_unknown)?.type_;
                if inner_param_type.check_completeness() {
                    if let SkyeType::Type(inner_type) = inner_param_type {
                        if has_decl {
                            if let SkyeType::Function(existing_params, ..) = &existing.as_ref().unwrap().type_ {
                                if !existing_params[i].type_.equals(&inner_type, EqualsLevel::Typewise) {
                                    ast_error!(
                                        self, params[i].type_, 
                                        format!(
                                            "Function parameter type does not match declaration parameter type (expecting {} but got {})",
                                            inner_type.stringify_native(), existing_params[i].type_.stringify_native()
                                        ).as_ref()
                                    );
                                }
                            }
                        }
                                
                        *inner_type
                    } else {
                        ast_error!(
                            self, params[i].type_, 
                            format!(
                                "Expecting type as parameter type (got {})", 
                                inner_param_type.stringify_native()
                            ).as_ref()
                        );
    
                        SkyeType::Void
                    }
                } else {
                    ast_error!(self, params[i].type_, "Cannot use incomplete type directly");
                    ast_note!(params[i].type_, "Define this type or reference it through a pointer");

                    SkyeType::Void
                }
            };

            params_output.push(SkyeFunctionParam::new(param_type.clone(), params[i].is_const));
            params_string.push_str(&param_type.stringify());

            if let Some(param) = &params[i].name {
                params_string.push(' ');
                params_string.push_str(&param.lexeme);
            }

            if i != params.len() - 1 {
                params_string.push_str(", ");
            }
        }

        Ok((params_string, params_output))
    }

    fn generate_fn_signature(&mut self, tok: &Token, inner_type: &SkyeType, return_stringified: &String, params_string: &String) -> (String, SkyeType) {
        let mangled = inner_type.mangle();
        let type_ = SkyeType::Type(Box::new(inner_type.clone()));
                
        let env = self.globals.borrow();
        
        let existing = env.get(&Token::dummy(mangled.clone().into()));
        if let Some(fnptr) = existing {
            if !fnptr.type_.equals(&type_, EqualsLevel::Typewise) {
                if let Some(orig_tok) = fnptr.tok {
                    token_error!(self, tok, "This function pointer's mangled type resolves to a different type");
                    token_note!(orig_tok, "This definition is invalid. Change the name of this symbol");
                } else {
                    token_error!(self, tok, "This function pointer's mangled type resolves to a different type. An invalid symbol definition is present in the code");
                }
            }
        } else {
            drop(env);
        
            self.declarations.push(CodeOutput::new());
            self.declarations.last_mut().unwrap().push("typedef ");
            self.declarations.last_mut().unwrap().push(return_stringified);
            self.declarations.last_mut().unwrap().push(" (*");
            self.declarations.last_mut().unwrap().push(&mangled);
            self.declarations.last_mut().unwrap().push(")(");
            self.declarations.last_mut().unwrap().push(&params_string);
            self.declarations.last_mut().unwrap().push(");\n");

            let mut env = self.globals.borrow_mut();
            env.define(
                mangled.clone().into(), 
                SkyeVariable::new(
                    type_.clone(), true,
                    Some(Box::new(tok.clone()))
                )
            );
        }

        (mangled, type_)
    }

    fn get_temporary_var(&mut self) -> String {
        let res = format!("SKYE_TMP_{}", self.tmp_var_cnt);
        self.tmp_var_cnt += 1;
        res
    }

    fn get_method(&mut self, object: &SkyeValue, name: &Token, strict: bool) -> Option<SkyeValue> {
        match object.type_.get_method(name, strict) {
            GetResult::Ok(value, ..) => {                
                let search_tok = Token::dummy(Rc::clone(&value));

                let result = self.globals.borrow().get(&search_tok);

                if let Some(var) = result {
                    return Some(SkyeValue::with_self_info(
                        value, var.type_, true, 
                        object.type_.get_self(&object.value, object.is_const).expect("get_self failed")
                    ))
                } else {
                    None
                }
            }
            _ => None
        }
    }

    fn output_call(&mut self, return_type: &SkyeType, callee_value: &str, args: &str, index: usize) -> String {
        self.definitions[index].push_indent();

        let tmp_var_name = {
            if matches!(return_type, SkyeType::Void) {
                String::new()      
            } else {
                let tmp_var = self.get_temporary_var();
                
                self.definitions[index].push(&return_type.stringify());
                self.definitions[index].push(" ");
                self.definitions[index].push(&tmp_var);
                self.definitions[index].push(" = ");

                tmp_var
            }
        };
        
        self.definitions[index].push(callee_value);
        self.definitions[index].push("(");
        self.definitions[index].push(args);
        self.definitions[index].push(");\n");

        tmp_var_name
    }

    fn call(&mut self, callee: &SkyeValue, expr: &Expression, callee_expr: &Expression, arguments: &Vec<Expression>, index: usize, allow_unknown: bool) -> Result<SkyeValue, ExecutionInterrupt>  {
        let (arguments_len, arguments_mod) = {
            if callee.self_info.is_some() {
                (arguments.len() + 1, 1 as usize)
            } else {
                (arguments.len(), 0 as usize)
            }
        };
        
        match &callee.type_ {
            SkyeType::Function(params, return_type, _) => {
                if params.len() != arguments_len {
                    ast_error!(
                        self, expr, 
                        format!(
                            "Expecting {} arguments for function call but got {}", 
                            params.len(), arguments_len
                        ).as_str()
                    );

                    return Err(ExecutionInterrupt::Error);
                }

                let mut args = String::new();
                for i in 0 .. arguments_len {
                    let arg = 'argblock: {
                        if i == 0 {
                            if let Some((info_val, info_type)) = &callee.self_info {
                                if let SkyeType::Pointer(_, is_const) = info_type {
                                    break 'argblock SkyeValue::new(
                                        Rc::clone(info_val), 
                                        info_type.clone(), 
                                        *is_const
                                    );
                                } else {
                                    unreachable!()
                                }
                            }
                        }

                        self.evaluate(&arguments[i - arguments_mod], index, allow_unknown)?
                    };
                    
                    if !params[i].type_.equals(&arg.type_, EqualsLevel::Strict) {
                        if i == 0 && arguments_mod == 1 {
                            // the only way self info is wrong is if constness is not respected
                            ast_error!(self, callee_expr, "This method cannot be called from a const source");
                        } else {
                            ast_error!(
                                self, arguments[i - arguments_mod], 
                                format!(
                                    "Argument type does not match parameter type (expecting {} but got {})",
                                    params[i].type_.stringify_native(), arg.type_.stringify_native()
                                ).as_ref()
                            );
                        }
                    }

                    let search_tok = Token::dummy(Rc::from("__copy__"));
                    if let Some(value) = self.get_method(&arg, &search_tok, true) {
                        let copy_constructor = self.call(&value, expr, &arguments[i - arguments_mod], &Vec::new(), index, allow_unknown)?;
                        args.push_str(&copy_constructor.value);

                        ast_info!(arguments[i - arguments_mod], "Skye inserted a copy constructor call for this expression"); // +I-copies
                    } else {
                        args.push_str(&arg.value);
                    }                    

                    if i != arguments_len - 1 {
                        args.push_str(", ");
                    }
                }

                let call_output = self.output_call(return_type, &callee.value, &args, index);
                Ok(SkyeValue::new(Rc::from(call_output.as_ref()), *return_type.clone(), false))
            }
            SkyeType::Template(name, definition, generics, generics_names, curr_name, read_env) => {
                if let Statement::Function(_, params, return_type_expr, ..) = definition {
                    if params.len() != arguments_len {
                        ast_error!(
                            self, expr, 
                            format!(
                                "Expecting {} arguments for function call but got {}", 
                                params.len(), arguments_len
                            ).as_str()
                        );
    
                        return Err(ExecutionInterrupt::Error);
                    }
                    
                    let mut generics_to_find = HashMap::new();
                    let mut generics_map = HashMap::new();
                    for generic in generics {
                        generics_to_find.insert(Rc::clone(&generic.name.lexeme), None);
                        generics_map.insert(Rc::clone(&generic.name.lexeme), generic.clone());
                    }

                    let tmp_env = Rc::new(RefCell::new(
                        Environment::with_enclosing(Rc::clone(&read_env))
                    ));

                    let mut args = String::new();
                    for i in 0 .. arguments_len {
                        let call_evaluated = 'argblock: {
                            if i == 0 {
                                if let Some((info_val, info_type)) = &callee.self_info {
                                    if let SkyeType::Pointer(_, is_const) = info_type {
                                        break 'argblock SkyeValue::new(
                                            Rc::clone(info_val), 
                                            info_type.clone(), 
                                            *is_const
                                        );
                                    } else {
                                        unreachable!()
                                    }
                                }
                            }

                            self.evaluate(&arguments[i - arguments_mod], index, false)?
                        };

                        // definition type evaluation has to be performed in definition environment
                        let previous = Rc::clone(&self.environment);
                        self.environment = Rc::clone(&tmp_env);

                        let previous_name = self.curr_name.clone();
                        self.curr_name = curr_name.clone();

                        let def_evaluated = {
                            if let Ok(evaluated) = self.evaluate(&params[i].type_, index, true) {
                                evaluated
                            } else {
                                self.curr_name   = previous_name;
                                self.environment = previous;

                                ast_error!(self, callee_expr, "Skye cannot infer the generic types for this function");
                                ast_note!(callee_expr, "Manually specify the generic types");
                                ast_note!(params[i].type_, "This type is too complex for Skye to be able to infer types");
                                return Err(ExecutionInterrupt::Error);
                            }
                        };

                        self.curr_name   = previous_name;
                        self.environment = previous;

                        let def_type = {
                            if matches!(def_evaluated.type_, SkyeType::Unknown(_)) {
                                SkyeType::Type(Box::new(def_evaluated.type_))
                            } else {
                                def_evaluated.type_
                            }
                        };

                        if !def_type.check_completeness() {
                            ast_error!(self, params[i].type_, "Cannot use incomplete type directly");
                            ast_note!(params[i].type_, "Define this type or reference it through a pointer");
                            ast_note!(expr, "This error is a result of template generation originating from this call");
                        }
                        
                        if let SkyeType::Type(inner_type) = &def_type {
                            if inner_type.equals(&call_evaluated.type_, EqualsLevel::Permissive) {
                                for (generic_name, generic_type) in inner_type.infer_type_from_similar(&call_evaluated.type_) {
                                    if generics_to_find.get(&generic_name).unwrap().is_none() {
                                        let wrapped = SkyeType::Type(Box::new(generic_type));
                                        let mapped_generic = generics_map.get(&generic_name).unwrap();

                                        if let Some(bounds) = &mapped_generic.bounds {
                                            if !bounds.is_respected_by(&wrapped) {
                                                if i != 0 || arguments_mod != 1 {
                                                    ast_error!(
                                                        self, arguments[i - arguments_mod], 
                                                        format!(
                                                            "Generic bound is not respected by this type (expecting {} but got {})",
                                                            bounds.stringify_native(), wrapped.stringify_native()
                                                        ).as_ref()
                                                    );

                                                    token_note!(mapped_generic.name, "Generic defined here");
                                                }   
                                            }
                                        }

                                        generics_to_find.insert(generic_name, Some(wrapped));
                                    }
                                }
                            } else {    
                                if i == 0 && arguments_mod == 1 {
                                    // the only way self info is wrong is if constness is not respected
                                    ast_error!(self, callee_expr, "This method cannot be called from a const source");
                                } else {
                                    ast_error!(
                                        self, arguments[i - arguments_mod], 
                                        format!(
                                            "Argument type does not match parameter type (expecting {} but got {})",
                                            inner_type.stringify_native(), call_evaluated.type_.stringify_native()
                                        ).as_ref()
                                    );

                                    ast_note!(params[i].type_, "Parameter type defined here");
                                }
                            }
                        } else {
                            ast_error!(
                                self, params[i].type_, 
                                format!(
                                    "Expecting type as parameter type (got {})",
                                    def_type.stringify_native()
                                ).as_ref()
                            );

                            ast_note!(expr, "This error is a result of template generation originating from this call");
                        }

                        let search_tok = Token::dummy(Rc::from("__copy__"));
                        if let Some(value) = self.get_method(&call_evaluated, &search_tok, true) {
                            let loc_callee_expr = {
                                if i != 0 || arguments_mod != 1 {
                                    &arguments[i - arguments_mod]
                                } else {
                                    callee_expr
                                }
                            };

                            let copy_constructor = self.call(&value, expr, loc_callee_expr, &Vec::new(), index, allow_unknown)?;
                            args.push_str(&copy_constructor.value);

                            ast_info!(loc_callee_expr, "Skye inserted a copy constructor call for this expression"); // +I-copies
                        } else {
                            args.push_str(&call_evaluated.value);
                        } 

                        if i != arguments_len - 1 {
                            args.push_str(", ");
                        }
                    }

                    for (name, generic_type) in generics_to_find {
                        let mapped = generics_map.get(&name).unwrap();

                        let type_ = {
                            if let Some(type_) = generic_type {
                                Some(type_)
                            } else {
                                mapped.default.clone()
                            }
                        };

                        if let Some(inner_type) = type_ {
                            let mut env = tmp_env.borrow_mut();
                            env.define(
                                Rc::clone(&name),
                                SkyeVariable::new(
                                    inner_type, true, 
                                    Some(Box::new(mapped.name.clone()))
                                )
                            );
                        } else {
                            ast_error!(self, callee_expr, "Skye cannot infer the generic types for this function");
                            ast_note!(callee_expr, "This expression is a template and requires generic typing");
                            ast_note!(callee_expr, "Manually specify the generic types");
                            return Err(ExecutionInterrupt::Error);
                        }
                    }

                    let previous = Rc::clone(&self.environment);
                    self.environment = Rc::clone(&tmp_env);

                    let previous_name = self.curr_name.clone();
                    self.curr_name = curr_name.clone();

                    let return_evaluated = {
                        let ret_type = self.evaluate(&return_type_expr, index, false)?.type_;
                        match ret_type {
                            SkyeType::Type(inner_type) => {
                                if inner_type.check_completeness() {
                                    *inner_type.clone()
                                } else {
                                    ast_error!(self, return_type_expr, "Cannot use incomplete type directly");
                                    ast_note!(return_type_expr, "Define this type or reference it through a pointer");
                                    ast_note!(expr, "This error is a result of template generation originating from this call");

                                    return Err(ExecutionInterrupt::Error);
                                }
                            }
                            SkyeType::Void => ret_type,
                            _ => {
                                ast_error!(
                                    self, return_type_expr, 
                                    format!(
                                        "Expecting type as return type (got {})",
                                        ret_type.stringify_native()
                                    ).as_ref()
                                );

                                ast_note!(expr, "This error is a result of template generation originating from this call");
                                return Err(ExecutionInterrupt::Error);
                            }
                        }
                    };

                    let final_name = self.get_generics(&name, &generics_names, &self.environment)?;
                    let search_tok = Token::dummy(Rc::clone(&final_name));

                    let mut env = self.globals.borrow_mut();
                    if let Some(existing) = env.get(&search_tok) {
                        if let SkyeType::Function(.., has_body) = existing.type_ {
                            if has_body {
                                env = tmp_env.borrow_mut();

                                for generic in generics {
                                    env.undef(Rc::clone(&generic.name.lexeme));
                                }

                                self.curr_name   = previous_name;
                                self.environment = previous;
                                
                                let call_output = self.output_call(&return_evaluated, &final_name, &args, index);
                                return Ok(SkyeValue::new(Rc::from(call_output.as_ref()), return_evaluated, true));
                            }
                        } else {
                            if let Some(tok) = existing.tok {
                                ast_error!(self, callee_expr, "Template generation for this call resulted in an invalid type");
                                token_note!(tok, "This definition is invalid. Change the name of this symbol");
                            } else {
                                ast_error!(self, callee_expr, "Template generation for this call resulted in an invalid type. An invalid symbol definition is present in the code");
                            }
                        }
                    }

                    drop(env);
                    
                    let old_had_error = self.had_error;
                    let type_ = self.execute(&definition, 0)?.expect("wrong type was generic-subscripted");

                    if self.had_error && !old_had_error {
                        ast_note!(expr, "This error is a result of template generation originating from this call");
                    }

                    self.curr_name   = previous_name;
                    self.environment = previous;

                    env = tmp_env.borrow_mut();
                    for generic in generics {
                        env.undef(Rc::clone(&generic.name.lexeme));
                    }

                    env.define(
                        Rc::clone(&final_name), 
                        SkyeVariable::new(
                            type_.clone(), true, 
                            None
                        )
                    );

                    let call_output = self.output_call(&return_evaluated, &final_name, &args, index);
                    Ok(SkyeValue::new(Rc::from(call_output.as_ref()), return_evaluated, false))
                } else {
                    ast_error!(self, callee_expr, "Cannot call this expression");
                    ast_note!(
                        callee_expr, 
                        format!(
                            "This expression has type {}",
                            callee.type_.stringify_native()
                        ).as_ref()
                    );

                    return Err(ExecutionInterrupt::Error);
                }
            }
            SkyeType::Macro(_, params_opt, return_expr_opt, return_type) => {
                if let Some(params) = params_opt {
                    if params.len() != arguments_len {
                        ast_error!(
                            self, expr, 
                            format!(
                                "Expecting {} arguments for macro call but got {}", 
                                params.len(), arguments_len
                            ).as_str()
                        );
    
                        return Err(ExecutionInterrupt::Error);
                    }

                    if let Some(return_expr) = return_expr_opt {
                        // native macro call
                        let mut curr_expr = return_expr.clone();
                        for i in 0 .. arguments_len {
                            curr_expr = curr_expr.replace_variable(&params[i].lexeme, &arguments[i]);
                        }

                        self.evaluate(&curr_expr, index, allow_unknown)
                    } else {
                        // C macro binding call
                        let tmp_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(&self.environment))));
                        let mut env = tmp_env.borrow_mut();

                        let mut args = String::new();
                        for i in 0 .. arguments_len {
                            let arg = self.evaluate(&arguments[i], index, allow_unknown)?;

                            if let SkyeType::Type(inner_type) = &arg.type_ {
                                args.push_str(&inner_type.stringify());
                            } else {
                                args.push_str(&arg.value);
                            }

                            env.define(
                                Rc::clone(&params[i].lexeme),
                                SkyeVariable::new(
                                    arg.type_, true,
                                    Some(Box::new(params[i].clone()))
                                )
                            );

                            if i != arguments_len - 1 {
                                args.push_str(", ");
                            }
                        }

                        drop(env);
                        let previous = Rc::clone(&self.environment);
                        self.environment = tmp_env;

                        let call_return_type = self.evaluate(return_type.as_ref().unwrap(), index, allow_unknown)?;

                        self.environment = previous;

                        if let SkyeType::Type(inner_type) = call_return_type.type_ {
                            Ok(SkyeValue::new(Rc::from(format!("{}({})", callee.value, args)), *inner_type, false))
                        } else {
                            ast_error!(
                                self, return_type.as_ref().unwrap(), 
                                format!(
                                    "Expecting type as return type (got {})",
                                    call_return_type.type_.stringify_native()
                                ).as_ref()
                            );
                            ast_note!(expr, "This error is a result of this macro expansion");
                            Err(ExecutionInterrupt::Error)
                        }
                    }
                } else {
                    unreachable!() // covered by unary '@' evaluation
                }
            }
            _ => {
                ast_error!(self, callee_expr, "Cannot call this expression");
                ast_note!(
                    callee_expr, 
                    format!(
                        "This expression has type {}",
                        callee.type_.stringify_native()
                    ).as_ref()
                );

                return Err(ExecutionInterrupt::Error);
            }
        }
    }

    fn unary_operator(
        &mut self, inner: SkyeValue, inner_expr: &Expression, 
        expr: &Expression, op_stringified: &str, op_method: &str, 
        op_type: Operator, op: &Token, index: usize, allow_unknown: bool
    ) -> Result<SkyeValue, ExecutionInterrupt> {
        match inner.type_.implements_op(op_type) {
            ImplementsHow::Native => Ok(SkyeValue::new(Rc::from(format!("{}{}", op_stringified, inner.value)), inner.type_, false)),
            ImplementsHow::ThirdParty => {
                let search_tok = Token::dummy(Rc::from(op_method));
                if let Some(value) = self.get_method(&inner, &search_tok, true) {
                    self.call(&value, expr, inner_expr, &Vec::new(), index, allow_unknown)
                } else {
                    token_error!(
                        self, op, 
                        format!(
                            "This operator is not implemented for type {}",
                            inner.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::No => {
                token_error!(
                    self, op, 
                    format!(
                        "Type {} cannot use this operator",
                        inner.type_.stringify_native()
                    ).as_ref()
                );
                
                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn binary_operator(
        &mut self, left: SkyeValue, return_type: SkyeType, 
        left_expr: &Expression, right_expr: &Expression, expr: &Expression, 
        op_stringified: &str, op_method: &str, op_type: Operator, 
        index: usize, allow_unknown: bool
    ) -> Result<SkyeValue, ExecutionInterrupt> {
        match left.type_.implements_op(op_type) {
            ImplementsHow::Native => {
                let right = self.evaluate(right_expr, index, allow_unknown)?;

                if left.type_.equals(&right.type_, EqualsLevel::Typewise) {
                    Ok(SkyeValue::new(Rc::from(format!("{} {} {}", left.value, op_stringified, right.value)), return_type, false))
                } else {
                    ast_error!(
                        self, right_expr, 
                        format!(
                            "Left operand type ({}) does not match right operand type ({})",
                            left.type_.stringify_native(), right.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::ThirdParty => {
                let search_tok = Token::dummy(Rc::from(op_method));
                if let Some(value) = self.get_method(&left, &search_tok, true) {
                    self.call(&value, expr, left_expr, &vec![right_expr.clone()], index, allow_unknown)
                } else {
                    ast_error!(
                        self, left_expr, 
                        format!(
                            "This operator is not implemented for type {}",
                            left.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::No => {
                ast_error!(
                    self, left_expr, 
                    format!(
                        "Type {} cannot use this operator",
                        left.type_.stringify_native()
                    ).as_ref()
                );

                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn binary_operator_no_check(
        &mut self, left: SkyeValue, return_type: SkyeType, 
        left_expr: &Expression, right_expr: &Expression, expr: &Expression, 
        op_stringified: &str, op_method: &str, op_type: Operator, 
        index: usize, allow_unknown: bool
    ) -> Result<SkyeValue, ExecutionInterrupt> {
        match left.type_.implements_op(op_type) {
            ImplementsHow::Native => {
                let right = self.evaluate(right_expr, index, allow_unknown)?;
                Ok(SkyeValue::new(Rc::from(format!("{} {} {}", left.value, op_stringified, right.value)), return_type, false))
            }
            ImplementsHow::ThirdParty => {
                let search_tok = Token::dummy(Rc::from(op_method));
                if let Some(value) = self.get_method(&left, &search_tok, true) {
                    self.call(&value, expr, left_expr, &vec![right_expr.clone()], index, allow_unknown)
                } else {
                    ast_error!(
                        self, left_expr, 
                        format!(
                            "This operator is not implemented for type {}",
                            left.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::No => {
                ast_error!(
                    self, left_expr, 
                    format!(
                        "Type {} cannot use this operator",
                        left.type_.stringify_native()
                    ).as_ref()
                );
                
                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn binary_operator_int_on_right(
        &mut self, left: SkyeValue, return_type: SkyeType, 
        left_expr: &Expression, right_expr: &Expression, expr: &Expression, 
        op_stringified: &str, op_method: &str, op_type: Operator, 
        index: usize, allow_unknown: bool
    ) -> Result<SkyeValue, ExecutionInterrupt> {
        match left.type_.implements_op(op_type) {
            ImplementsHow::Native => {
                let right = self.evaluate(right_expr, index, allow_unknown)?;

                if right.type_.equals(&SkyeType::AnyInt, EqualsLevel::Typewise) {
                    Ok(SkyeValue::new(Rc::from(format!("{} {} {}", left.value, op_stringified, right.value)), return_type, false))
                } else {
                    ast_error!(
                        self, right_expr, 
                        format!(
                            "Expecting right operand type to be integer but got {}",
                            right.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::ThirdParty => {
                let search_tok = Token::dummy(Rc::from(op_method));
                if let Some(value) = self.get_method(&left, &search_tok, true) {
                    self.call(&value, expr, left_expr, &vec![right_expr.clone()], index, allow_unknown)
                } else {
                    ast_error!(
                        self, left_expr, 
                        format!(
                            "This operator is not implemented for type {}",
                            left.type_.stringify_native()
                        ).as_ref()
                    );

                    Err(ExecutionInterrupt::Error)
                }
            }
            ImplementsHow::No => {
                ast_error!(
                    self, left_expr, 
                    format!(
                        "Type {} cannot use this operator",
                        left.type_.stringify_native()
                    ).as_ref()
                );

                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn suffix_unary_native_only(&mut self, inner: SkyeValue, op_stringified: &str, op_type: Operator, op: &Token) -> Result<SkyeValue, ExecutionInterrupt> {
        match inner.type_.implements_op(op_type) {
            ImplementsHow::Native => Ok(SkyeValue::new(Rc::from(format!("{}{}", inner.value, op_stringified)), inner.type_, false)),
            ImplementsHow::No | ImplementsHow::ThirdParty => {
                token_error!(
                    self, op, 
                    format!(
                        "Type {} cannot use this operator",
                        inner.type_.stringify_native()
                    ).as_ref()
                );

                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn evaluate(&mut self, expr: &Expression, index: usize, allow_unknown: bool) -> Result<SkyeValue, ExecutionInterrupt> {
        match expr {
            Expression::Grouping(inner_expr) => {
                let inner = self.evaluate(&inner_expr, index, allow_unknown)?;
                Ok(SkyeValue::new(Rc::from(format!("({})", inner.value)), inner.type_, inner.is_const))
            } 
            Expression::Slice(opening_brace, items) => {
                let first_item = self.evaluate(&items[0], index, allow_unknown)?;
                let mut items_stringified = String::from("{");
                items_stringified.push_str(&first_item.value);

                if items.len() != 1 {
                    items_stringified.push_str(", ");
                }

                for i in 1 .. items.len() {
                    let evaluated = self.evaluate(&items[i], index, allow_unknown)?;

                    if !evaluated.type_.equals(&first_item.type_, EqualsLevel::Typewise) {
                        ast_error!(
                            self, items[i], 
                            format!(
                                "Items inside array do not have matching types (expecting {} but got {})",
                                first_item.type_.stringify_native(), evaluated.type_.stringify_native()
                            ).as_ref()
                        );
                        ast_note!(items[0], "First item defined here");
                    }

                    items_stringified.push_str(&evaluated.value);

                    if i != items.len() - 1 {
                        items_stringified.push_str(", ");
                    }
                }

                items_stringified.push('}');

                let mut slice_tok = opening_brace.clone();
                slice_tok.set_lexeme("core_DOT_Slice");

                let mut type_tok = opening_brace.clone();
                type_tok.set_lexeme(&first_item.type_.mangle());
                
                let return_type = self.evaluate(
                    &Expression::Subscript(
                        Box::new(Expression::Variable(slice_tok)),
                        opening_brace.clone(),
                        vec![Expression::Variable(type_tok)]
                    ), index, allow_unknown
                )?;

                if let SkyeType::Type(inner_type) = return_type.type_ {
                    Ok(SkyeValue::new(
                        Rc::from(format!(
                            "({}) {{ .ptr = ({}[]) {}, .length = {} }}", 
                            return_type.value, 
                            first_item.type_.stringify(),
                            items_stringified, 
                            items.len()
                        )), 
                        *inner_type, 
                        true
                    ))
                } else {
                    panic!("struct template generation resulted in not a type");
                }
            }
            Expression::Literal(value, _, kind) => {
                match kind {
                    LiteralKind::Void => Ok(SkyeValue::special(SkyeType::Void)),
                    
                    LiteralKind::U8  => Ok(SkyeValue::new(Rc::from(format!("UINT8_C({})",  value)), SkyeType::U8, true)),
                    LiteralKind::I8  => Ok(SkyeValue::new(Rc::from(format!("INT8_C({})",   value)), SkyeType::I8, true)),
                    LiteralKind::U16 => Ok(SkyeValue::new(Rc::from(format!("UINT16_C({})", value)), SkyeType::U16, true)),
                    LiteralKind::I16 => Ok(SkyeValue::new(Rc::from(format!("INT16_C({})",  value)), SkyeType::I16, true)),
                    LiteralKind::U32 => Ok(SkyeValue::new(Rc::from(format!("UINT32_C({})", value)), SkyeType::U32, true)),
                    LiteralKind::I32 => Ok(SkyeValue::new(Rc::from(format!("INT32_C({})",  value)), SkyeType::I32, true)),
                    LiteralKind::U64 => Ok(SkyeValue::new(Rc::from(format!("UINT64_C({})", value)), SkyeType::U64, true)),
                    LiteralKind::I64 => Ok(SkyeValue::new(Rc::from(format!("INT64_C({})",  value)), SkyeType::I64, true)),
                    LiteralKind::Usz => Ok(SkyeValue::new(Rc::from(format!("SIZE_T_C({})", value)), SkyeType::Usz, true)),
                    LiteralKind::F32 => Ok(SkyeValue::new(Rc::from(format!("(float){}", value)), SkyeType::F32, true)),
                    LiteralKind::F64 => Ok(SkyeValue::new(Rc::from(format!("(double){}", value)), SkyeType::F64, true)),

                    LiteralKind::AnyInt   => Ok(SkyeValue::new(Rc::clone(value), SkyeType::AnyInt, true)),
                    LiteralKind::AnyFloat => Ok(SkyeValue::new(Rc::clone(value), SkyeType::AnyFloat, true)),
                    
                    LiteralKind::Char => Ok(SkyeValue::new(Rc::from(format!("'{}'", value)), SkyeType::Char, true)),
                    LiteralKind::RawString => {
                        if let Some(string_const) = self.strings.get(value) {
                            Ok(SkyeValue::new(Rc::from(format!("SKYE_STRING_{}", string_const)), SkyeType::RawString, true))
                        } else {
                            let str_index = self.strings.len();
                            self.strings_code.push(format!(
                                "const char SKYE_STRING_{}[{}] = \"{}\";\n", 
                                str_index, get_real_string_length(value), fix_raw_string(value)
                            ).as_ref());

                            self.strings.insert(Rc::clone(value), str_index);
                            Ok(SkyeValue::new(Rc::from(format!("SKYE_STRING_{}", str_index)), SkyeType::RawString, true))
                        }
                    }
                    LiteralKind::String => {
                        if self.string_type.is_none() {
                            if let SkyeType::Type(inner_type) = &self.globals.borrow().get(
                                &Token::dummy(Rc::from("core_DOT_String"))
                            ).as_ref().expect("No String type is defined yet").type_ 
                            {
                                self.string_type = Some(*inner_type.clone());
                            } else {
                                panic!("The default String type was overwritten with an invalid type");
                            }
                        }

                        if let Some(string_const) = self.strings.get(value) {
                            Ok(SkyeValue::new(
                                Rc::from(format!(
                                    "(core_DOT_String) {{ .raw = SKYE_STRING_{}, .length = sizeof(SKYE_STRING_{}) }}", 
                                    string_const, string_const
                                )), 
                                self.string_type.as_ref().unwrap().clone(), 
                                true)
                            )
                        } else {
                            let str_index = self.strings.len();
                            let string_len = get_real_string_length(value);
                            self.strings_code.push(format!(
                                "const char SKYE_STRING_{}[{}] = \"{}\";\n", 
                                str_index, string_len, value
                            ).as_ref());

                            self.strings.insert(Rc::clone(value), str_index);

                            Ok(SkyeValue::new(
                                Rc::from(format!(
                                    "(core_DOT_String) {{ .raw = SKYE_STRING_{}, .length = {} }}", 
                                    str_index, string_len
                                )), 
                                self.string_type.as_ref().unwrap().clone(),
                                true
                            ))
                        }
                    }
                }
            }
            Expression::Unary(op, inner_expr, is_prefix) => {
                let inner = self.evaluate(&inner_expr, index, allow_unknown)?;

                if *is_prefix {
                    match op.type_ {
                        TokenType::PlusPlus => {
                            if inner.is_const {
                                ast_error!(self, inner_expr, "Cannot apply '++' operator on const value");
                                Err(ExecutionInterrupt::Error)
                            } else {
                                self.unary_operator(
                                    inner, inner_expr, expr, "++", 
                                    "__inc__", Operator::Inc, op, index, allow_unknown
                                )
                            }
                        }
                        TokenType::MinusMinus => {
                            if inner.is_const {
                                ast_error!(self, inner_expr, "Cannot apply '--' operator on const value");
                                Err(ExecutionInterrupt::Error)
                            } else {
                                self.unary_operator(
                                    inner, inner_expr, expr, "--", 
                                    "__dec__", Operator::Dec, op, index, allow_unknown
                                )
                            }  
                        }
                        TokenType::Plus => {
                            self.unary_operator(inner, inner_expr, expr, "+", "__pos__", Operator::Pos, op, index, allow_unknown)
                        }
                        TokenType::Minus => {
                            self.unary_operator(inner, inner_expr, expr, "-", "__neg__", Operator::Neg, op, index, allow_unknown)
                        }
                        TokenType::Bang => {
                            if matches!(inner.type_, SkyeType::Type(_)) | matches!(inner.type_, SkyeType::Void) | matches!(inner.type_, SkyeType::Unknown(_)) {
                                // !type syntax for void!type (result operator)
                                
                                if !inner.type_.check_completeness() {
                                    ast_error!(self, inner_expr, "Cannot use incomplete type directly");
                                    ast_note!(inner_expr, "Define this type or reference it through a pointer");
                                    return Err(ExecutionInterrupt::Error);
                                }

                                let mut custom_token = op.clone();
                                custom_token.set_lexeme("core_DOT_Result");

                                self.evaluate(
                                    &Expression::Subscript(
                                        Box::new(Expression::Variable(custom_token)), 
                                        op.clone(), 
                                        vec![
                                            Expression::Literal(
                                                Rc::from(""), 
                                                op.clone(), 
                                                LiteralKind::Void
                                            ),
                                            *inner_expr.clone()
                                        ]
                                    ), 
                                    index, allow_unknown
                                )
                            } else {
                                self.unary_operator(inner, inner_expr, expr, "!", "__not__", Operator::Not, op, index, allow_unknown)
                            }
                        }
                        TokenType::Question => {
                            if matches!(inner.type_, SkyeType::Type(_)) | matches!(inner.type_, SkyeType::Void) | matches!(inner.type_, SkyeType::Unknown(_)) {
                                // option operator

                                if !inner.type_.check_completeness() {
                                    ast_error!(self, inner_expr, "Cannot use incomplete type directly");
                                    ast_note!(inner_expr, "Define this type or reference it through a pointer");
                                    return Err(ExecutionInterrupt::Error);
                                }

                                let mut custom_token = op.clone();
                                custom_token.set_lexeme("core_DOT_Option");

                                self.evaluate(
                                    &Expression::Subscript(
                                        Box::new(Expression::Variable(custom_token)), 
                                        op.clone(), 
                                        vec![*inner_expr.clone()]
                                    ), 
                                    index, allow_unknown
                                )
                            } else {
                                ast_error!(
                                    self, inner_expr, 
                                    format!(
                                        "Invalid operand for option operator (expecting type but got {})",
                                        inner.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                            }
                        }
                        TokenType::Tilde => {
                            self.unary_operator(inner, inner_expr, expr, "~", "__inv__", Operator::Inv, op, index, allow_unknown)
                        }
                        TokenType::BitwiseAnd => {
                            match inner.type_.implements_op(Operator::Ref) {
                                ImplementsHow::Native | ImplementsHow::ThirdParty => {
                                    let value = {
                                        if inner_expr.is_valid_assignment_target() {
                                            inner.value
                                        } else {
                                            let tmp_var = self.get_temporary_var();

                                            self.definitions[index].push_indent();
                                            self.definitions[index].push(&inner.type_.stringify());
                                            self.definitions[index].push(" ");
                                            self.definitions[index].push(&tmp_var);
                                            self.definitions[index].push(" = ");
                                            self.definitions[index].push(&inner.value);
                                            self.definitions[index].push(";\n");
    
                                            Rc::from(tmp_var)
                                        }
                                    };

                                    Ok(SkyeValue::new(Rc::from(format!("&{}", value)), SkyeType::Pointer(Box::new(inner.type_), inner.is_const), true))
                                }
                                ImplementsHow::No => {
                                    token_error!(
                                        self, op, 
                                        format!(
                                            "Type {} cannot use this operator",
                                            inner.type_.stringify_native()
                                        ).as_ref()
                                    );

                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                        }
                        TokenType::RefConst => {
                            match inner.type_.implements_op(Operator::ConstRef) {
                                ImplementsHow::Native | ImplementsHow::ThirdParty => {
                                    let value = {
                                        if inner_expr.is_valid_assignment_target() {
                                            inner.value
                                        } else {
                                            let tmp_var = self.get_temporary_var();

                                            self.definitions[index].push_indent();
                                            self.definitions[index].push(&inner.type_.stringify());
                                            self.definitions[index].push(" ");
                                            self.definitions[index].push(&tmp_var);
                                            self.definitions[index].push(" = ");
                                            self.definitions[index].push(&inner.value);
                                            self.definitions[index].push(";\n");
    
                                            Rc::from(tmp_var)
                                        }
                                    };

                                    Ok(SkyeValue::new(Rc::from(format!("&{}", value)), SkyeType::Pointer(Box::new(inner.type_), true), true))
                                }
                                ImplementsHow::No => {
                                    token_error!(
                                        self, op, 
                                        format!(
                                            "Type {} cannot use this operator",
                                            inner.type_.stringify_native()
                                        ).as_ref()
                                    );

                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                        }
                        TokenType::Star => {
                            match inner.type_ {
                                SkyeType::Pointer(ptr_type, is_const) => {
                                    if matches!(*ptr_type, SkyeType::Void) {
                                        ast_error!(self, inner_expr, "Cannot dereference a voidptr");
                                        Err(ExecutionInterrupt::Error)
                                    } else {
                                        Ok(SkyeValue::new(Rc::from(format!("*{}", inner.value)), *ptr_type, is_const))
                                    }
                                }
                                SkyeType::Type(type_type) => {
                                    Ok(SkyeValue::new(
                                        Rc::from(format!("{}*", inner.value)), 
                                        SkyeType::Type(Box::new(SkyeType::Pointer(type_type, false))), 
                                        true
                                    ))
                                }
                                SkyeType::Unknown(_) => {
                                    Ok(SkyeValue::special(SkyeType::Type(Box::new(SkyeType::Pointer(Box::new(inner.type_), false)))))
                                }
                                _ => {
                                    match inner.type_.implements_op(Operator::Deref) {
                                        ImplementsHow::Native => {
                                            return Ok(SkyeValue::new(Rc::from(format!("*{}", inner.value)), inner.type_, false));
                                        }
                                        ImplementsHow::ThirdParty => {
                                            let search_tok = Token::dummy(Rc::from("__deref__"));
                                            if let Some(value) = self.get_method(&inner, &search_tok, true) {
                                                return self.call(&value, expr, inner_expr, &Vec::new(), index, allow_unknown);
                                            } 
                                        }
                                        ImplementsHow::No => (),
                                    }

                                    match inner.type_.implements_op(Operator::AsPtr) {
                                        ImplementsHow::Native => unreachable!(),
                                        ImplementsHow::ThirdParty => {
                                            let search_tok = Token::dummy(Rc::from("__asptr__"));
                                            if let Some(value) = self.get_method(&inner, &search_tok, true) {
                                                let value = self.call(&value, expr, inner_expr, &Vec::new(), index, allow_unknown)?;
            
                                                let (inner_type, is_const) = {
                                                    if let SkyeType::Pointer(inner, ptr_is_const) = &value.type_ {
                                                        (*inner.clone(), *ptr_is_const)
                                                    } else {
                                                        token_error!(
                                                            self, op, 
                                                            format!(
                                                                "Expecting pointer as return type of __asptr__ (got {})",
                                                                value.type_.stringify_native()
                                                            ).as_ref()
                                                        );

                                                        return Err(ExecutionInterrupt::Error);
                                                    }
                                                };
            
                                                Ok(SkyeValue::new(Rc::from(format!("*{}", value.value)), inner_type, is_const))
                                            } else {
                                                token_error!(
                                                    self, op, 
                                                    format!(
                                                        "This operator is not implemented for type {}",
                                                        inner.type_.stringify_native()
                                                    ).as_ref()
                                                );
                                                
                                                Err(ExecutionInterrupt::Error)
                                            }
                                        }
                                        ImplementsHow::No => {
                                            token_error!(
                                                self, op, 
                                                format!(
                                                    "Type {} cannot use this operator",
                                                    inner.type_.stringify_native()
                                                ).as_ref()
                                            );
                                            
                                            Err(ExecutionInterrupt::Error)
                                        }
                                    }
                                }
                            }
                        }
                        TokenType::StarConst => {
                            match inner.type_ {
                                SkyeType::Pointer(ptr_type, _) => { // readonly dereference
                                    if matches!(*ptr_type, SkyeType::Void) {
                                        ast_error!(self, inner_expr, "Cannot dereference a voidptr");
                                        Err(ExecutionInterrupt::Error)
                                    } else {
                                        Ok(SkyeValue::new(Rc::from(format!("*{}", inner.value)), *ptr_type, true))
                                    }
                                }
                                SkyeType::Type(type_type) => {
                                    Ok(SkyeValue::new(
                                        Rc::from(format!("{}*", inner.value)), 
                                        SkyeType::Type(Box::new(SkyeType::Pointer(type_type, true))), 
                                        true
                                    ))
                                }
                                SkyeType::Unknown(_) => {
                                    Ok(SkyeValue::special(SkyeType::Type(Box::new(SkyeType::Pointer(Box::new(inner.type_), true)))))
                                }
                                _ => {
                                    self.unary_operator(inner, inner_expr, expr, "*", "__constderef__", Operator::ConstDeref, op, index, allow_unknown)
                                }
                            }
                        }
                        TokenType::Try => {
                            if matches!(self.curr_function, CurrentFn::None) {
                                token_error!(self, op, "Can only use \"try\" operator inside functions");
                                return Err(ExecutionInterrupt::Error);
                            }

                            if let SkyeType::Enum(full_name, variants, name) = &inner.type_ {
                                let (return_type, return_expr) = {
                                    if let CurrentFn::Some(return_type, return_type_expr) = &self.curr_function {
                                        if let SkyeType::Enum(_, return_variants, return_type_name) = return_type {
                                            if return_variants.is_some() && name.as_ref() == return_type_name.as_ref() {
                                                (return_type.clone(), return_type_expr.clone())
                                            } else {
                                                token_error!(
                                                    self, op, 
                                                    format!(
                                                        "Can only use \"try\" operator inside functions returning core::Result or core::Option (got {})",
                                                        return_type.stringify_native()
                                                    ).as_ref()
                                                );

                                                ast_note!(return_type_expr, "Return type defined here");
                                                return Err(ExecutionInterrupt::Error);
                                            }
                                        } else {
                                            token_error!(
                                                self, op, 
                                                format!(
                                                    "Can only use \"try\" operator inside functions returning core::Result or core::Option (got {})",
                                                    return_type.stringify_native()
                                                ).as_ref()
                                            );

                                            ast_note!(return_type_expr, "Return type defined here");
                                            return Err(ExecutionInterrupt::Error);
                                        }
                                    } else {
                                        unreachable!();
                                    }
                                };

                                let tmp_var_name = self.get_temporary_var();

                                self.definitions[index].push_indent();
                                self.definitions[index].push(&full_name);
                                self.definitions[index].push(" ");
                                self.definitions[index].push(&tmp_var_name);
                                self.definitions[index].push(" = ");
                                self.definitions[index].push(&inner.value);
                                self.definitions[index].push(";\n");

                                self.definitions[index].push_indent();
                                self.definitions[index].push("if (");
                                self.definitions[index].push(&tmp_var_name);
                                self.definitions[index].push(".kind == ");

                                match name.as_ref() {
                                    "core_DOT_Option" => {
                                        self.definitions[index].push("core_DOT_Option_DOT_Kind_DOT_None)\n");
                                        self.definitions[index].inc_indent();
                                        
                                        self.definitions[index].push_indent();
                                        self.definitions[index].push("return ");

                                        if return_type.equals(&inner.type_, EqualsLevel::Typewise) {
                                            self.definitions[index].push(&tmp_var_name);
                                            self.definitions[index].push(";\n");
                                        } else if let SkyeType::Enum(full_name, ..) = &return_type {
                                            self.definitions[index].push(&full_name);
                                            self.definitions[index].push("_DOT_None;\n");
                                        } else {
                                            unreachable!();
                                        }

                                        self.definitions[index].dec_indent();

                                        if let Some(variant) = variants.as_ref().unwrap().get("some") {
                                            Ok(SkyeValue::new(
                                                Rc::from(format!("{}.some", tmp_var_name)), 
                                                variant.clone(), 
                                                true
                                            ))
                                        } else {
                                            // when variant is void
                                            Ok(SkyeValue::special(SkyeType::Void))
                                        }
                                    }
                                    "core_DOT_Result" => {
                                        self.definitions[index].push("core_DOT_Result_DOT_Kind_DOT_Error)\n");
                                        self.definitions[index].inc_indent();
                                        
                                        self.definitions[index].push_indent();
                                        self.definitions[index].push("return ");
                                        
                                        if return_type.equals(&inner.type_, EqualsLevel::Typewise) {
                                            self.definitions[index].push(&tmp_var_name);
                                            self.definitions[index].push(";\n");
                                        } else if let SkyeType::Enum(full_name, return_variants, _) = &return_type {
                                            if let Some(return_variant) = return_variants.as_ref().unwrap().get("error") {
                                                if let Some(variant) = variants.as_ref().unwrap().get("error") {
                                                    if variant.equals(return_variant, EqualsLevel::Typewise) {
                                                        self.definitions[index].push(&full_name);
                                                        self.definitions[index].push("_DOT_Error(");
                                                        self.definitions[index].push(&tmp_var_name);
                                                        self.definitions[index].push(".error);\n");
                                                    } else {
                                                        ast_error!(
                                                            self, expr, 
                                                            format!(
                                                                "core::Result \"Error\" variant type ({}) does not match with return type's \"Error\" variant type ({})",
                                                                variant.stringify_native(), return_variant.stringify_native(), 
                                                            ).as_ref()
                                                        );

                                                        ast_note!(return_expr, "Return type defined here");
                                                    }
                                                } else {
                                                    ast_error!(
                                                        self, expr, 
                                                        format!(
                                                            "core::Result \"Error\" variant type (void) does not match with return type's \"Error\" variant type ({})",
                                                            return_variant.stringify_native(), 
                                                        ).as_ref()
                                                    );

                                                    ast_note!(return_expr, "Return type defined here");
                                                }
                                            } else if let Some(variant) = variants.as_ref().unwrap().get("error") {
                                                ast_error!(
                                                    self, expr, 
                                                    format!(
                                                        "core::Result \"Error\" variant type ({}) does not match with return type's \"Error\" variant type (void)",
                                                        variant.stringify_native(), 
                                                    ).as_ref()
                                                );

                                                ast_note!(return_expr, "Return type defined here");
                                            } else {
                                                self.definitions[index].push(&full_name);
                                                self.definitions[index].push("_DOT_Error;\n");
                                            }
                                        } else {
                                            unreachable!();
                                        }

                                        self.definitions[index].dec_indent();

                                        if let Some(variant) = variants.as_ref().unwrap().get("ok") {
                                            Ok(SkyeValue::new(
                                                Rc::from(format!("{}.ok", tmp_var_name)), 
                                                variant.clone(), 
                                                true
                                            ))
                                        } else {
                                            // when variant is void
                                            Ok(SkyeValue::special(SkyeType::Void))
                                        }
                                    }
                                    _ => {
                                        ast_error!(
                                            self, inner_expr, 
                                            format!(
                                                "Can only use \"try\" operator on expressions returning core::Result or core::Option (got {})",
                                                inner.type_.stringify_native()
                                            ).as_ref()
                                        );
                                        Err(ExecutionInterrupt::Error)
                                    }
                                }
                            } else {
                                ast_error!(
                                    self, inner_expr, 
                                    format!(
                                        "Can only use \"try\" operator on expressions returning core::Result or core::Option (got {})",
                                        inner.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                            }
                        }
                        TokenType::At => {
                            if let SkyeType::Type(inner_type) = inner.type_ {
                                if let SkyeType::Macro(name, params, return_expr, return_type) = &*inner_type {
                                    if params.is_none() {
                                        if let Some(real_return_expr) = return_expr {
                                            self.evaluate(real_return_expr, index, allow_unknown)
                                        } else {
                                            let ret_type = self.evaluate(return_type.as_ref().unwrap(), index, allow_unknown)?;

                                            if let SkyeType::Type(inner_type) = ret_type.type_ {
                                                if !inner_type.check_completeness() {
                                                    ast_error!(self, return_type.as_ref().unwrap(), "Cannot use incomplete type directly");
                                                    ast_note!(return_type.as_ref().unwrap(), "Define this type or reference it through a pointer");
                                                    return Err(ExecutionInterrupt::Error);
                                                }

                                                Ok(SkyeValue::new(Rc::clone(name), *inner_type, true))
                                            } else {
                                                ast_error!(
                                                    self, return_type.as_ref().unwrap(), 
                                                    format!(
                                                        "Expecting type as return type (got {})",
                                                        ret_type.type_.stringify_native()
                                                    ).as_ref()
                                                );
                                                
                                                ast_note!(expr, "This error is a result of this macro expansion");
                                                Err(ExecutionInterrupt::Error)
                                            }
                                        }
                                    } else {
                                        Ok(SkyeValue::new(Rc::clone(name), *inner_type, true))
                                    }
                                } else {
                                    token_error!(
                                        self, op, 
                                        format!(
                                            "'@' can only be used on macros (got {})",
                                            inner_type.stringify_native()
                                        ).as_ref()
                                    );

                                    Err(ExecutionInterrupt::Error)
                                }
                            } else {
                                token_error!(
                                    self, op, 
                                    format!(
                                        "'@' can only be used on macros (got {})",
                                        inner.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                            }
                        }
                        _ => unreachable!()
                    }
                } else {
                    match op.type_ {
                        TokenType::PlusPlus => {
                            if inner.is_const {
                                ast_error!(self, inner_expr, "Cannot apply '++' operator on const value");
                                Err(ExecutionInterrupt::Error)
                            } else {
                                self.suffix_unary_native_only(inner, "++", Operator::Inc, op)
                            }
                        }
                        TokenType::MinusMinus => {
                            if inner.is_const {
                                ast_error!(self, inner_expr, "Cannot apply '--' operator on const value");
                                Err(ExecutionInterrupt::Error)
                            } else {
                                self.suffix_unary_native_only(inner, "--", Operator::Inc, op)
                            }
                        }
                        _ => unreachable!()
                    }
                }
            }
            Expression::Binary(left_expr, op, right_expr) => {
                let left  = self.evaluate(&left_expr, index, allow_unknown)?;
                let left_type = left.type_.clone();

                match op.type_ {
                    TokenType::Plus => {
                        self.binary_operator_no_check(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "+", "__add__", Operator::Add, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Minus => {
                        self.binary_operator_no_check(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "-", "__sub__", Operator::Sub, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Slash => {
                        self.binary_operator(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "/", "__div__", Operator::Div, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Star => {
                        self.binary_operator(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "*", "__mul__", Operator::Mul, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Mod => {
                        self.binary_operator(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "%", "__mod__", Operator::Mod, 
                            index, allow_unknown
                        )
                    }
                    TokenType::ShiftLeft => {
                        self.binary_operator_int_on_right(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "<<", "__shl__", Operator::Shl, 
                            index, allow_unknown
                        )
                    }
                    TokenType::ShiftRight => {
                        self.binary_operator_int_on_right(
                            left, left_type, &left_expr, &right_expr, 
                            expr, ">>", "__shr__", Operator::Shr, 
                            index, allow_unknown
                        )
                    }
                    TokenType::LogicOr => {
                        match left.type_.implements_op(Operator::Or) {
                            ImplementsHow::Native => {
                                // needed so short circuiting can work
                                let tmp_var = self.get_temporary_var();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("u8 ");
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(";\n");

                                self.definitions[index].push_indent();
                                self.definitions[index].push("if (");
                                self.definitions[index].push(&left.value);
                                self.definitions[index].push(") {\n");
                                self.definitions[index].inc_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(" = 1;\n");
                                self.definitions[index].dec_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("} else {\n");
                                self.definitions[index].inc_indent();

                                let right = self.evaluate(right_expr, index, allow_unknown)?;

                                if !left.type_.equals(&right.type_, EqualsLevel::Typewise) {
                                    ast_error!(
                                        self, right_expr, 
                                        format!(
                                            "Left operand type ({}) does not match right operand type ({})",
                                            left.type_.stringify_native(), right.type_.stringify_native()
                                        ).as_ref()
                                    );
                
                                    return Err(ExecutionInterrupt::Error);
                                }

                                self.definitions[index].push_indent();
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(" = ");
                                self.definitions[index].push(&right.value);
                                self.definitions[index].push(";\n");
                                self.definitions[index].dec_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("}\n");

                                Ok(SkyeValue::new(Rc::from(tmp_var), SkyeType::U8, false))
                            }
                            ImplementsHow::ThirdParty => {
                                let search_tok = Token::dummy(Rc::from("__or__"));
                                if let Some(value) = self.get_method(&left, &search_tok, true) {
                                    self.call(&value, expr, left_expr, &vec![*right_expr.clone()], index, allow_unknown)
                                } else {
                                    ast_error!(
                                        self, left_expr, 
                                        format!(
                                            "This operator is not implemented for type {}",
                                            left.type_.stringify_native()
                                        ).as_ref()
                                    );
                
                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            ImplementsHow::No => {
                                ast_error!(
                                    self, left_expr, 
                                    format!(
                                        "Type {} cannot use this operator",
                                        left.type_.stringify_native()
                                    ).as_ref()
                                );
                
                                Err(ExecutionInterrupt::Error)
                            }
                        }
                    }
                    TokenType::LogicAnd => {
                        match left.type_.implements_op(Operator::And) {
                            ImplementsHow::Native => {
                                // needed so short circuiting can work
                                let tmp_var = self.get_temporary_var();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("u8 ");
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(";\n");

                                self.definitions[index].push_indent();
                                self.definitions[index].push("if (");
                                self.definitions[index].push(&left.value);
                                self.definitions[index].push(") {\n");
                                self.definitions[index].inc_indent();

                                let right = self.evaluate(right_expr, index, allow_unknown)?;

                                if !left.type_.equals(&right.type_, EqualsLevel::Typewise) {
                                    ast_error!(
                                        self, right_expr, 
                                        format!(
                                            "Left operand type ({}) does not match right operand type ({})",
                                            left.type_.stringify_native(), right.type_.stringify_native()
                                        ).as_ref()
                                    );
                
                                    return Err(ExecutionInterrupt::Error);
                                }

                                self.definitions[index].push_indent();
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(" = ");
                                self.definitions[index].push(&right.value);
                                self.definitions[index].push(";\n");
                                self.definitions[index].dec_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("} else {\n");
                                self.definitions[index].inc_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push(&tmp_var);
                                self.definitions[index].push(" = 0;\n");
                                self.definitions[index].dec_indent();

                                self.definitions[index].push_indent();
                                self.definitions[index].push("}\n");

                                Ok(SkyeValue::new(Rc::from(tmp_var), SkyeType::U8, false))
                            }
                            ImplementsHow::ThirdParty => {
                                let search_tok = Token::dummy(Rc::from("__and__"));
                                if let Some(value) = self.get_method(&left, &search_tok, true) {
                                    self.call(&value, expr, left_expr, &vec![*right_expr.clone()], index, allow_unknown)
                                } else {
                                    ast_error!(
                                        self, left_expr, 
                                        format!(
                                            "This operator is not implemented for type {}",
                                            left.type_.stringify_native()
                                        ).as_ref()
                                    );
                
                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            ImplementsHow::No => {
                                ast_error!(
                                    self, left_expr, 
                                    format!(
                                        "Type {} cannot use this operator",
                                        left.type_.stringify_native()
                                    ).as_ref()
                                );
                
                                Err(ExecutionInterrupt::Error)
                            }
                        }
                    }
                    TokenType::BitwiseXor => {
                        self.binary_operator(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "^", "__xor__", Operator::Xor, 
                            index, allow_unknown
                        )
                    }
                    TokenType::BitwiseOr => {
                        if left.type_.is_type() || matches!(left.type_, SkyeType::Void) {
                            let right = self.evaluate(&right_expr, index, allow_unknown)?;

                            if right.type_.is_type() || matches!(right.type_, SkyeType::Void) {
                                Ok(SkyeValue::special(SkyeType::Group(Box::new(left.type_), Box::new(right.type_))))
                            } else {
                                ast_error!(
                                    self, right_expr, 
                                    format!(
                                        "Left operand type ({}) does not match right operand type ({})",
                                        left.type_.stringify_native(), right.type_.stringify_native()
                                    ).as_ref()
                                );
                                
                                Err(ExecutionInterrupt::Error)
                            }
                        } else {
                            self.binary_operator(
                                left, left_type, &left_expr, &right_expr, 
                                expr, "|", "__bitor__", Operator::BitOr, 
                                index, allow_unknown
                            )
                        }
                    }
                    TokenType::BitwiseAnd => {
                        self.binary_operator(
                            left, left_type, &left_expr, &right_expr, 
                            expr, "&", "__bitand__", Operator::BitAnd, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Greater => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, ">", "__gt__", Operator::Gt, 
                            index, allow_unknown
                        )
                    }
                    TokenType::GreaterEqual => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, ">=", "__ge__", Operator::Ge, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Less => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, "<", "__lt__", Operator::Lt, 
                            index, allow_unknown
                        )
                    }
                    TokenType::LessEqual => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, "<=", "__le__", Operator::Le, 
                            index, allow_unknown
                        )
                    }
                    TokenType::EqualEqual => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, "==", "__eq__", Operator::Eq, 
                            index, allow_unknown
                        )
                    }
                    TokenType::BangEqual => {
                        self.binary_operator_no_check(
                            left, SkyeType::U8, &left_expr, &right_expr, 
                            expr, "!=", "__ne__", Operator::Ne, 
                            index, allow_unknown
                        )
                    }
                    TokenType::Bang => {
                        let left_ok = matches!(left.type_, SkyeType::Type(_)) | matches!(left.type_, SkyeType::Void) | matches!(left.type_, SkyeType::Unknown(_));
                        if left_ok {
                            if !left.type_.check_completeness() {
                                ast_error!(self, left_expr, "Cannot use incomplete type directly");
                                ast_note!(left_expr, "Define this type or reference it through a pointer");
                                return Err(ExecutionInterrupt::Error);
                            }

                            let right = self.evaluate(&right_expr, index, allow_unknown)?;
                            
                            if matches!(right.type_, SkyeType::Type(_)) | matches!(right.type_, SkyeType::Void) | matches!(right.type_, SkyeType::Unknown(_)) {
                                // result operator

                                if !right.type_.check_completeness() {
                                    ast_error!(self, right_expr, "Cannot use incomplete type directly");
                                    ast_note!(left_expr, "Define this type or reference it through a pointer");
                                    return Err(ExecutionInterrupt::Error);
                                }

                                let mut custom_token = op.clone();
                                custom_token.set_lexeme("core_DOT_Result");

                                self.evaluate(
                                    &Expression::Subscript(
                                        Box::new(Expression::Variable(custom_token)), 
                                        op.clone(), 
                                        vec![
                                            *left_expr.clone(),
                                            *right_expr.clone(),
                                        ]
                                    ), 
                                    index, allow_unknown
                                )
                            } else {
                                ast_error!(
                                    self, right_expr, 
                                    format!(
                                        "Invalid operand for result operator (expecting type but got {})",
                                        right.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                            }
                        } else {
                            ast_error!(
                                self, left_expr, 
                                format!(
                                    "Invalid operand for result operator (expecting type but got {})",
                                    left.type_.stringify_native()
                                ).as_ref()
                            );

                            Err(ExecutionInterrupt::Error)
                        }
                    }
                    _ => unreachable!()
                }
            }
            Expression::Variable(name) => {
                if let Some(var_info) = self.environment.borrow().get(&name) {
                    Ok(SkyeValue::new(name.lexeme.clone(), var_info.type_, var_info.is_const))
                } else if allow_unknown {
                    Ok(SkyeValue::special(SkyeType::Unknown(Rc::clone(&name.lexeme))))
                } else {
                    token_error!(self, &name, "Cannot reference undefined symbol");
                    Err(ExecutionInterrupt::Error)
                }
            }
            Expression::Assign(target_expr, op, value_expr) => {
                let target = self.evaluate(&target_expr, index, allow_unknown)?;
                let target_type = target.type_.clone();

                if target.is_const {
                    ast_error!(self, target_expr, "Assignment target is const");
                }

                match op.type_ {
                    TokenType::Equal => {
                        let value = self.evaluate(&value_expr, index, allow_unknown)?;

                        if target_type.equals(&value.type_, EqualsLevel::Strict) {
                            Ok(SkyeValue::new(Rc::from(format!("{} = {}", target.value, value.value)), value.type_, true))
                        } else {
                            ast_error!(
                                self, value_expr, 
                                format!(
                                    "Value type ({}) does not match target type ({})",
                                    value.type_.stringify_native(), target_type.stringify_native()
                                ).as_ref()
                            );

                            Err(ExecutionInterrupt::Error)
                        }
                    }
                    TokenType::PlusEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "+=", "__setadd__", Operator::SetAdd, 
                            index, allow_unknown
                        )
                    }
                    TokenType::MinusEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "-=", "__setsub__", Operator::SetSub, 
                            index, allow_unknown
                        )
                    }
                    TokenType::StarEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "*=", "__setmul__", Operator::SetMul, 
                            index, allow_unknown
                        )
                    }
                    TokenType::SlashEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "/=", "__setdiv__", Operator::SetDiv, 
                            index, allow_unknown
                        )
                    }
                    TokenType::ModEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "%=", "__setmod__", Operator::SetMod, 
                            index, allow_unknown
                        )
                    }
                    TokenType::ShiftLeftEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "<<=", "__setshl__", Operator::SetShl, 
                            index, allow_unknown
                        )
                    }
                    TokenType::ShiftRightEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, ">>=", "__setshr__", Operator::SetShr, 
                            index, allow_unknown
                        )
                    }
                    TokenType::AndEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "&=", "__setand__", Operator::SetAnd, 
                            index, allow_unknown
                        )
                    }
                    TokenType::XorEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "^=", "__setxor__", Operator::SetXor, 
                            index, allow_unknown
                        )
                    }
                    TokenType::OrEquals => {
                        self.binary_operator(
                            target, target_type, &target_expr, &value_expr, 
                            expr, "|=", "__setor__", Operator::SetOr, 
                            index, allow_unknown
                        )
                    },
                    _ => unreachable!()
                }
            }
            Expression::Call(callee_expr, _, arguments) => {
                let callee = self.evaluate(&callee_expr, index, allow_unknown)?;
                self.call(&callee, expr, callee_expr, arguments, index, allow_unknown)
            }
            Expression::FnPtr(kw, return_type_expr, params) => {
                let return_type = self.get_return_type(return_type_expr, index, allow_unknown)?;
                let (params_string, params_output) = self.get_params(params, None, false, index, allow_unknown)?;

                let return_stringified = return_type.stringify();
                let inner_type = SkyeType::Function(params_output, Box::new(return_type), false);
                
                let (mangled, type_) = self.generate_fn_signature(kw, &inner_type, &return_stringified, &params_string);

                Ok(SkyeValue::new(mangled.into(), type_, true))
            }
            Expression::Ternary(_, cond_expr, then_branch_expr, else_branch_expr) => {
                let cond = self.evaluate(&cond_expr, index, allow_unknown)?;

                match cond.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::AnyInt => (),
                    _ => {
                        ast_error!(
                            self, cond_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type for ternary operator condition (got {})",
                                cond.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                let then_branch = self.evaluate(&then_branch_expr, index, allow_unknown)?;
                let else_branch = self.evaluate(&else_branch_expr, index, allow_unknown)?;

                if !then_branch.type_.equals(&else_branch.type_, EqualsLevel::Typewise) {
                    ast_error!(
                        self, else_branch_expr, 
                        format!(
                            "Ternary operator else branch type ({}) does not match then branch type ({})",
                            else_branch.type_.stringify_native(), then_branch.type_.stringify_native()
                        ).as_ref()
                    );
                }

                Ok(SkyeValue::new(Rc::from(format!("{} ? {} : {}", cond.value, then_branch.value, else_branch.value)), then_branch.type_, true))
            }
            Expression::CompoundLiteral(identifier_expr, _, fields) => {
                let identifier_type = self.evaluate(&identifier_expr, index, allow_unknown)?;

                match &identifier_type.type_ {
                    SkyeType::Type(inner_type) => {
                        if !inner_type.check_completeness() {
                            ast_error!(self, identifier_expr, "Cannot use incomplete type directly");
                            ast_note!(identifier_expr, "Define this type or reference it through a pointer");
                            return Err(ExecutionInterrupt::Error);
                        }

                        match &**inner_type {
                            SkyeType::Struct(name, def_fields, _) => {
                                if let Some(defined_fields) = def_fields {
                                    if fields.len() != defined_fields.len() {
                                        ast_error!(self, expr, format!(
                                            "Expecting {} fields but got {}", 
                                            defined_fields.len(), fields.len()
                                        ).as_str());
                                        return Err(ExecutionInterrupt::Error);
                                    }
    
                                    let mut fields_output = String::new();
                                    for (i, field) in fields.iter().enumerate() {
                                        if let Some((field_type, _)) = defined_fields.get(&field.name.lexeme) {
                                            let field_evaluated = self.evaluate(&field.expr, index, allow_unknown)?;
                                            
                                            if !field_type.equals(&field_evaluated.type_, EqualsLevel::Strict) {
                                                ast_error!(
                                                    self, field.expr, 
                                                    format!(
                                                        "Invalid type for this field (expecting {} but got {})",
                                                        field_type.stringify_native(), field_evaluated.type_.stringify_native()
                                                    ).as_ref()
                                                );
                                            }

                                            fields_output.push('.');
                                            fields_output.push_str(&field.name.lexeme);
                                            fields_output.push_str(" = ");

                                            let search_tok = Token::dummy(Rc::from("__copy__"));
                                            if let Some(value) = self.get_method(&field_evaluated, &search_tok, true) {
                                                let copy_constructor = self.call(&value, expr, &field.expr, &Vec::new(), index, allow_unknown)?;
                                                fields_output.push_str(&copy_constructor.value);

                                                ast_info!(field.expr, "Skye inserted a copy constructor call for this expression"); // +I-copies
                                            } else {
                                                fields_output.push_str(&field_evaluated.value);
                                            }
    
                                            if i != fields.len() - 1 {
                                                fields_output.push_str(", ");
                                            }
                                        } else {
                                            token_error!(self, field.name, "Unknown struct field");
                                        }
                                    }
    
                                    Ok(SkyeValue::new(Rc::from(format!("({}) {{ {} }}", name, fields_output)), *inner_type.clone(), true))
                                } else {
                                    ast_error!(self, identifier_expr, "Cannot initialize struct that is declared but has no definition");
                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            SkyeType::Bitfield(name, def_fields) => {
                                if let Some(defined_fields) = def_fields {
                                    if fields.len() != defined_fields.len() {
                                        ast_error!(self, expr, format!(
                                            "Expecting {} fields but got {}", 
                                            defined_fields.len(), fields.len()
                                        ).as_str());
                                        return Err(ExecutionInterrupt::Error);
                                    }
    
                                    let mut fields_output = String::new();
                                    for (i, field) in fields.iter().enumerate() {
                                        if let Some(field_type) = defined_fields.get(&field.name.lexeme) {
                                            let field_evaluated = self.evaluate(&field.expr, index, allow_unknown)?;
                                            
                                            if !field_type.equals(&field_evaluated.type_, EqualsLevel::Strict) {
                                                ast_error!(
                                                    self, field.expr, 
                                                    format!(
                                                        "Invalid type for this field (expecting {} but got {})",
                                                        field_type.stringify_native(), field_evaluated.type_.stringify_native()
                                                    ).as_ref()
                                                );
                                            }

                                            // copy costructor here is not needed because bitfields always have numeric types
    
                                            fields_output.push('.');
                                            fields_output.push_str(&field.name.lexeme);
                                            fields_output.push_str(" = ");
                                            fields_output.push_str(&field_evaluated.value);
    
                                            if i != fields.len() - 1 {
                                                fields_output.push_str(", ");
                                            }
                                        } else {
                                            token_error!(self, field.name, "Unknown bitfield field");
                                        }
                                    }
    
                                    Ok(SkyeValue::new(Rc::from(format!("({}) {{ {} }}", name, fields_output)), *inner_type.clone(), true))
                                } else {
                                    ast_error!(self, identifier_expr, "Cannot initialize bitfield that is declared but has no definition");
                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            SkyeType::Union(name, def_fields) => {
                                if let Some(defined_fields) = def_fields {
                                    if fields.len() != 1 {
                                        ast_error!(self, expr, "Can only assign one field of a union");
                                        return Err(ExecutionInterrupt::Error);
                                    }
    
                                    let mut buf = String::new();
                                    if let Some(field_type) = defined_fields.get(&fields[0].name.lexeme) {
                                        let field_evaluated = self.evaluate(&fields[0].expr, index, allow_unknown)?;
                                        
                                        if !field_type.equals(&field_evaluated.type_, EqualsLevel::Strict) {
                                            ast_error!(
                                                self, fields[0].expr, 
                                                format!(
                                                    "Invalid type for this field (expecting {} but got {})",
                                                    field_type.stringify_native(), field_evaluated.type_.stringify_native()
                                                ).as_ref()
                                            );
                                        }

                                        buf.push('.');
                                        buf.push_str(&fields[0].name.lexeme);
                                        buf.push_str(" = ");

                                        let search_tok = Token::dummy(Rc::from("__copy__"));
                                        if let Some(value) = self.get_method(&field_evaluated, &search_tok, true) {
                                            let copy_constructor = self.call(&value, expr, &fields[0].expr, &Vec::new(), index, allow_unknown)?;
                                            buf.push_str(&copy_constructor.value);

                                            ast_info!(fields[0].expr, "Skye inserted a copy constructor call for this expression"); // +I-copies
                                        } else {
                                            buf.push_str(&field_evaluated.value);
                                        }            
                                    } else {
                                        token_error!(self, fields[0].name, "Unknown union field");
                                    }
    
                                    Ok(SkyeValue::new(Rc::from(format!("({}) {{ {} }}", name, buf)), *inner_type.clone(), true))
                                } else {
                                    ast_error!(self, identifier_expr, "Cannot initialize union that is declared but has no definition");
                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            _ => {
                                ast_error!(
                                    self, identifier_expr, 
                                    format!(
                                        "Expecting struct, struct template, union, or bitfield type as compound literal identifier (got {})",
                                        inner_type.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                            }
                        }
                    }
                    SkyeType::Template(name, definition, generics, generics_names, curr_name, read_env) => {
                        if let Statement::Struct(struct_name, defined_fields, ..) = &definition {
                            if fields.len() != defined_fields.len() {
                                ast_error!(self, expr, format!(
                                    "Expecting {} fields but got {}", 
                                    defined_fields.len(), fields.len()
                                ).as_str());
                                return Err(ExecutionInterrupt::Error);
                            }
                            
                            let mut generics_to_find = HashMap::new();
                            let mut generics_map = HashMap::new();
                            for generic in generics {
                                generics_to_find.insert(Rc::clone(&generic.name.lexeme), None);
                                generics_map.insert(Rc::clone(&generic.name.lexeme), generic.clone());
                            }

                            let mut fields_map = HashMap::new();
                            for field in defined_fields {
                                if fields_map.contains_key(&field.name.lexeme) {
                                    token_error!(self, field.name, "Cannot define the same struct field multiple times");
                                } else {
                                    fields_map.insert(Rc::clone(&field.name.lexeme), field.expr.clone());
                                }
                            }

                            let tmp_env = Rc::new(RefCell::new(
                                Environment::with_enclosing(Rc::clone(&read_env))
                            ));

                            let mut fields_output = String::new();
                            for (i, field) in fields.iter().enumerate() {
                                if let Some(def_field_expr) = fields_map.get(&field.name.lexeme) {
                                    let previous = Rc::clone(&self.environment);
                                    self.environment = Rc::clone(&tmp_env);
            
                                    let previous_name = self.curr_name.clone();
                                    self.curr_name = curr_name.clone();

                                    let def_evaluated = {
                                        if let Ok(evaluated) = self.evaluate(&def_field_expr, index, true) {
                                            evaluated
                                        } else {
                                            self.curr_name   = previous_name;
                                            self.environment = previous;
                                            return Err(ExecutionInterrupt::Error);
                                        }
                                    };

                                    self.curr_name   = previous_name;
                                    self.environment = previous;
                                            
                                    let literal_evaluated = self.evaluate(&field.expr, index, false)?;

                                    let def_type = {
                                        if matches!(def_evaluated.type_, SkyeType::Unknown(_)) {
                                            SkyeType::Type(Box::new(def_evaluated.type_))
                                        } else {
                                            def_evaluated.type_
                                        }
                                    };

                                    if !def_type.check_completeness() {
                                        ast_error!(self, def_field_expr, "Cannot use incomplete type directly");
                                        ast_note!(def_field_expr, "Define this type or reference it through a pointer");
                                        ast_note!(expr, "This error is a result of template generation originating from this compound literal");
                                        return Err(ExecutionInterrupt::Error);
                                    }

                                    if let SkyeType::Type(inner_type) = &def_type {
                                        if inner_type.equals(&literal_evaluated.type_, EqualsLevel::Permissive) {
                                            for (generic_name, generic_type) in inner_type.infer_type_from_similar(&literal_evaluated.type_) {
                                                if generics_to_find.get(&generic_name).unwrap().is_none() {
                                                    let wrapped = SkyeType::Type(Box::new(generic_type));
                                                    let mapped_generic = generics_map.get(&generic_name).unwrap();
    
                                                    if let Some(bounds) = &mapped_generic.bounds {
                                                        if !bounds.is_respected_by(&wrapped) {
                                                            ast_error!(
                                                                self, field.expr, 
                                                                format!(
                                                                    "Generic bound is not respected by this type (expecting {} but got {})",
                                                                    bounds.stringify_native(), wrapped.stringify_native()
                                                                ).as_ref()
                                                            );

                                                            token_note!(mapped_generic.name, "Generic defined here");
                                                        }
                                                    }
    
                                                    generics_to_find.insert(generic_name, Some(wrapped));
                                                }
                                            }
                                        } else {
                                            ast_error!(
                                                self, field.expr, 
                                                format!(
                                                    "Field type does not match definition field type (expecting {} but got {})",
                                                    inner_type.stringify_native(), literal_evaluated.type_.stringify_native()
                                                ).as_ref()
                                            );
                                        }
                                    } else {
                                        ast_error!(
                                            self, field.expr, 
                                            format!(
                                                "Expecting type as field type (got {})",
                                                def_type.stringify_native()
                                            ).as_ref()
                                        );
                                    }
    
                                    fields_output.push('.');
                                    fields_output.push_str(&field.name.lexeme);
                                    fields_output.push_str(" = ");
                                    fields_output.push_str(&literal_evaluated.value);
    
                                    if i != fields.len() - 1 {
                                        fields_output.push_str(", ");
                                    }
                                } else {
                                    token_error!(self, field.name, "Unknown struct field");
                                }
                            }

                            for (name, generic_type) in generics_to_find {
                                let mapped = generics_map.get(&name).unwrap();

                                let type_ = {
                                    if let Some(type_) = generic_type {
                                        Some(type_)
                                    } else {
                                        mapped.default.clone()
                                    }
                                };

                                if let Some(inner_type) = type_ {
                                    let mut env = tmp_env.borrow_mut();
                                    env.define(
                                        Rc::clone(&name),
                                        SkyeVariable::new(
                                            inner_type, true, 
                                            Some(Box::new(mapped.name.clone()))
                                        )
                                    );
                                } else {
                                    ast_error!(self, identifier_expr, "Skye cannot infer the generic types for this struct literal");
                                    ast_note!(identifier_expr, "This expression is a template and requires generic typing");
                                    ast_note!(identifier_expr, "Manually specify the generic types");
                                    return Err(ExecutionInterrupt::Error);
                                }
                            }

                            let final_name = self.get_generics(&name, &generics_names, &self.environment)?;
                            let search_tok = Token::new(Rc::from(""), TokenType::Identifier, Rc::clone(&final_name), 0, 0);
                            
                            let mut env = self.globals.borrow_mut();
                            if let Some(var) = env.get(&search_tok) {
                                env = tmp_env.borrow_mut();
    
                                for generic in generics {
                                    env.undef(Rc::clone(&generic.name.lexeme));
                                }
                                
                                if let SkyeType::Type(inner_type) = var.type_ {
                                    return Ok(SkyeValue::new(Rc::from(format!("({}) {{ {} }}", final_name, fields_output)), *inner_type, true));
                                } else if let Some(orig_tok) = var.tok {
                                    token_error!(self, struct_name, "This struct's generic type name resolves to an invalid type");
                                    token_note!(orig_tok, "This definition is invalid. Change the name of this symbol");
                                } else {
                                    token_error!(self, struct_name, "This struct's generic type name resolves to an invalid type. An invalid symbol definition is present in the code");
                                }
                            }
    
                            drop(env);

                            let previous = Rc::clone(&self.environment);
                            self.environment = Rc::clone(&tmp_env);
    
                            let previous_name = self.curr_name.clone();
                            self.curr_name = curr_name.clone();
                            
                            let type_ = self.execute(&definition, 0)?.expect("wrong type was generic-subscripted");
    
                            self.curr_name   = previous_name;
                            self.environment = previous;
    
                            env = tmp_env.borrow_mut();
                            for generic in generics {
                                env.undef(Rc::clone(&generic.name.lexeme));
                            }
    
                            env.define(
                                Rc::clone(&final_name), 
                                SkyeVariable::new(
                                    type_.clone(), true, None
                                )
                            );

                            if let SkyeType::Type(inner_type) = type_ {
                                return Ok(SkyeValue::new(Rc::from(format!("({}) {{ {} }}", final_name, fields_output)), *inner_type, true));
                            } else {
                                panic!("struct template generation resulted in not a type");
                            }
                        } else {
                            ast_error!(
                                self, identifier_expr, 
                                format!(
                                    "Expecting struct, struct template, union, or bitfield type as compound literal identifier (got {})",
                                    identifier_type.type_.stringify_native()
                                ).as_ref()
                            );

                            Err(ExecutionInterrupt::Error)
                        }
                    }
                    _ => {
                        ast_error!(
                            self, identifier_expr, 
                            format!(
                                "Expecting struct, struct template, union, or bitfield type as compound literal identifier (got {})",
                                identifier_type.type_.stringify_native()
                            ).as_ref()
                        );

                        Err(ExecutionInterrupt::Error)
                    }
                    
                }
            }
            Expression::Subscript(subscripted_expr, paren, arguments) => {
                let subscripted = self.evaluate(&subscripted_expr, index, allow_unknown)?;

                match subscripted.type_ {
                    SkyeType::Pointer(inner_type, is_const) => {
                        if arguments.len() != 1 {
                            token_error!(self, paren, "Expecting one subscript argument for pointer offset");
                            return Err(ExecutionInterrupt::Error);
                        }

                        let arg = self.evaluate(&arguments[0], index, allow_unknown)?;

                        match arg.type_ {
                            SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                            SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                            SkyeType::Usz | SkyeType::AnyInt => {
                                Ok(SkyeValue::new(Rc::from(format!("{}[{}]", subscripted.value, arg.value)), *inner_type, is_const))
                            }
                            _ => {
                                ast_error!(
                                    self, &arguments[0], 
                                    format!(
                                        "Expecting integer for subscripting operation (got {})",
                                        arg.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                           }
                        }
                    }
                    SkyeType::RawString => {
                        if arguments.len() != 1 {
                            token_error!(self, paren, "Expecting one subscript argument for string offset");
                            return Err(ExecutionInterrupt::Error);
                        }

                        let arg = self.evaluate(&arguments[0], index, allow_unknown)?;

                        match arg.type_ {
                            SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                            SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                            SkyeType::Usz | SkyeType::AnyInt => {
                                Ok(SkyeValue::new(Rc::from(format!("{}[{}]", subscripted.value, arg.value)), SkyeType::Char, true))
                            }
                            _ => {
                                 ast_error!(
                                    self, &arguments[0], 
                                    format!(
                                        "Expecting integer for subscripting operation (got {})",
                                        arg.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                           }
                        }
                    }
                    SkyeType::Template(name, definition, generics, generics_names, curr_name, read_env) => {    
                        if arguments.len() != generics.len() {
                            let mut needed_cnt = 0;
                            for generic in &generics {
                                if generic.default.is_none() {
                                    needed_cnt += 1;
                                }
                            }

                            if arguments.len() < needed_cnt || arguments.len() > generics.len() {
                                ast_error!(
                                    self, expr, 
                                    format!(
                                        "Expecting at least {} arguments and {} at most for template generation but got {}", 
                                        needed_cnt, generics.len(), arguments.len()
                                    ).as_str()
                                );
            
                                return Err(ExecutionInterrupt::Error);
                            }
                        }

                        let tmp_env = Rc::new(RefCell::new(
                            Environment::with_enclosing(Rc::clone(&read_env))
                        ));

                        let offs = {
                            if generics.len() > 1 && generics.first().unwrap().default.is_some() && generics.last().unwrap().default.is_none() {
                                generics.len() - arguments.len()
                            } else {
                                0
                            }
                        };

                        for (i, generic) in generics.iter().enumerate() {
                            let evaluated = {
                                if i >= offs && i - offs < arguments.len() {
                                    self.evaluate(&arguments[i - offs], index, allow_unknown)?.type_
                                } else {
                                    generic.default.as_ref().unwrap().clone()
                                }
                            };

                            match &evaluated {
                                SkyeType::Type(_) | SkyeType::Void | SkyeType::Unknown(_) => (),
                                _ => {
                                    ast_error!(
                                        self, arguments[i - offs], 
                                        format!(
                                            "Expecting type as generic type (got {})",
                                            evaluated.stringify_native()
                                        ).as_ref()
                                    );

                                    continue;
                                }
                            }

                            if !evaluated.check_completeness() {
                                ast_error!(self, arguments[i - offs], "Cannot use incomplete type directly");
                                ast_note!(arguments[i - offs], "Define this type or reference it through a pointer");
                                return Err(ExecutionInterrupt::Error);
                            }

                            if let Some(bounds) = &generic.bounds {
                                if !bounds.is_respected_by(&evaluated) {
                                    ast_error!(
                                        self, arguments[i - offs], 
                                        format!(
                                            "Generic bound is not respected by this type (expecting {} but got {})",
                                            bounds.stringify_native(), evaluated.stringify_native()
                                        ).as_ref()
                                    );

                                    token_note!(generic.name, "Generic defined here");
                                }
                            }
                                
                            let mut env = tmp_env.borrow_mut();
                            env.define(
                                Rc::clone(&generic.name.lexeme),
                                SkyeVariable::new(
                                    evaluated, true, 
                                    Some(Box::new(generic.name.clone()))
                                )
                            );
                        }

                        let final_name = self.get_generics(&name, &generics_names, &tmp_env)?;
                        let search_tok = Token::new(Rc::from(""), TokenType::Identifier, Rc::clone(&final_name), 0, 0);

                        let mut env = self.globals.borrow_mut();
                        if let Some(var) = env.get(&search_tok) {
                            if let SkyeType::Function(.., has_body) = var.type_ {
                                if has_body {
                                    env = tmp_env.borrow_mut();

                                    for generic in generics {
                                        env.undef(Rc::clone(&generic.name.lexeme));
                                    }

                                    if let Some(self_info) = subscripted.self_info {
                                        return Ok(SkyeValue::with_self_info(final_name, var.type_, var.is_const, self_info));
                                    } else {
                                        return Ok(SkyeValue::new(final_name, var.type_, var.is_const));
                                    }
                                }
                            } else {
                                env = tmp_env.borrow_mut();

                                for generic in generics {
                                    env.undef(Rc::clone(&generic.name.lexeme));
                                }

                                if let Some(self_info) = subscripted.self_info {
                                    return Ok(SkyeValue::with_self_info(final_name, var.type_, var.is_const, self_info));
                                } else {
                                    return Ok(SkyeValue::new(final_name, var.type_, var.is_const));
                                }
                            }
                        }

                        drop(env);

                        let previous = Rc::clone(&self.environment);
                        self.environment = Rc::clone(&tmp_env);

                        let previous_name = self.curr_name.clone();
                        self.curr_name = curr_name;

                        let type_ = self.execute(&definition, 0)?.expect("wrong type was generic-subscripted");

                        self.curr_name   = previous_name;
                        self.environment = previous;

                        env = tmp_env.borrow_mut();
                        for generic in generics {
                            env.undef(Rc::clone(&generic.name.lexeme));
                        }

                        env.define(
                            Rc::clone(&final_name), 
                            SkyeVariable::new(
                                type_.clone(), true, None
                            )
                        );

                        if let Some(self_info) = subscripted.self_info {
                            Ok(SkyeValue::with_self_info(final_name, type_, true, self_info))
                        } else {
                            Ok(SkyeValue::new(final_name, type_, true))
                        }
                    }
                    _ => {
                        match subscripted.type_.implements_op(Operator::Subscript) {
                            ImplementsHow::Native => unreachable!(),
                            ImplementsHow::ThirdParty => {
                                let search_tok = Token::dummy(Rc::from("__subscript__"));
                                if let Some(value) = self.get_method(&subscripted, &search_tok, true) {
                                    let call_value = self.call(&value, expr, subscripted_expr, &arguments, index, allow_unknown)?;
                                    
                                    if let SkyeType::Pointer(inner_type, is_const) = call_value.type_ {
                                        Ok(SkyeValue::new(Rc::from(format!("*{}", call_value.value).as_ref()), *inner_type, is_const))
                                    } else {
                                        ast_error!(
                                            self, subscripted_expr, 
                                            format!(
                                                "Expecting pointer as return type of __subscript__ (got {})",
                                                call_value.type_.stringify_native()
                                            ).as_ref()
                                        );

                                        Err(ExecutionInterrupt::Error)
                                    }
                                } else {
                                    ast_error!(
                                        self, subscripted_expr, 
                                        format!(
                                            "Subscripting operation is not implemented for type {}",
                                            subscripted.type_.stringify_native()
                                        ).as_ref()
                                    );

                                    Err(ExecutionInterrupt::Error)
                                }
                            }
                            ImplementsHow::No => {
                                ast_error!(
                                    self, subscripted_expr, 
                                    format!(
                                        "Type {} cannot be subscripted",
                                        subscripted.type_.stringify_native()
                                    ).as_ref()
                                );

                                Err(ExecutionInterrupt::Error)
                                
                            }
                        }
                    }
                }
            }
            Expression::Get(object_expr, name) => {
                let object = self.evaluate(&object_expr, index, allow_unknown)?;

                match object.type_.get(&object.value, name, object.is_const) {
                    GetResult::Ok(value, type_, is_const) => {
                        return Ok(SkyeValue::new(value, type_, is_const))
                    }
                    GetResult::InvalidType => {
                        ast_error!(
                            self, object_expr, 
                            format!(
                                "Can only get properties from structs and sum type enums (got {})",
                                object.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                    GetResult::Undefined => ast_error!(self, object_expr, "Cannot get properties from undefined struct or enum"),
                    GetResult::FieldNotFound => {
                        if let Some(value) = self.get_method(&object, name, false) {
                            return Ok(value);
                        } else {
                            token_error!(self, name, "Undefined property");
                        }
                    }
                }

                Err(ExecutionInterrupt::Error)
            }
            Expression::StaticGet(object_expr, name) => {
                let object = self.evaluate(&object_expr, index, allow_unknown)?;

                match object.type_.static_get(name) {
                    GetResult::Ok(value, ..) => {
                        let mut search_tok = Token::dummy(Rc::clone(&value));
                        let env = self.globals.borrow();

                        if let Some(var) = env.get(&search_tok) {
                            return Ok(SkyeValue::new(value, var.type_, var.is_const))
                        } else if let SkyeType::Type(inner_type) = &object.type_ {
                            if let SkyeType::Enum(enum_name, ..) = &**inner_type {
                                search_tok.set_lexeme(format!("{}_DOT_{}", enum_name, name.lexeme).as_ref());

                                if let Some(var) = env.get(&search_tok) {
                                    return Ok(SkyeValue::new(search_tok.lexeme, var.type_, var.is_const))
                                } else {
                                    token_error!(self, name, "Undefined property");
                                }
                            } else {
                                token_error!(self, name, "Undefined property");
                            }
                        } else {
                            token_error!(self, name, "Undefined property");
                        }
                    }
                    GetResult::InvalidType => {
                        ast_error!(
                            self, object_expr, 
                            format!(
                                "Can only statically access namespaces, structs, enums and instances (got {})",
                                object.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                    _ => unreachable!()
                }

                Err(ExecutionInterrupt::Error)
            }
        }
    }

    fn handle_deferred(&mut self, index: usize) {
        let deferred = self.deferred.borrow();
        if let Some(statements) = deferred.last() {  
            let cloned = statements.clone();   
            drop(deferred);                   

            for statement in cloned.iter().rev() {
                let _ = self.execute(&statement, index);
            }
        } 
    }

    fn handle_destructors(&mut self, index: usize, global: bool, stmt: &Statement, msg: &str) -> Result<Option<SkyeType>, ExecutionInterrupt> {
        if !global {
            let vars = self.environment.borrow().iter_local();

            for (name, var) in vars {
                if matches!(var.type_, SkyeType::Struct(..) | SkyeType::Enum(..)) {
                    let search_tok = Token::dummy(Rc::from("__destruct__"));
                    let var_value = SkyeValue::new(Rc::clone(&name), var.type_.clone(), var.is_const);

                    if let Some(value) = self.get_method(&var_value, &search_tok, true) {
                        let fake_expr = Expression::Variable(search_tok);
                        let call = self.call(&value, &fake_expr, &fake_expr, &Vec::new(), index, false)?;

                        ast_info!(stmt, format!("Skye inserted a destructor call for \"{}\" {} this statement", name, msg).as_ref()); // +I-destructors

                        self.definitions[index].push_indent();
                        self.definitions[index].push(&call.value);
                        self.definitions[index].push(";\n");
                    }
                }
            }
        }

        Ok(None)
    }

    fn execute_block(&mut self, statements: &Vec<Statement>, environment: Rc<RefCell<Environment>>, index: usize, global: bool) {
        let previous = Rc::clone(&self.environment);
        self.environment = environment;

        self.deferred.borrow_mut().push(Vec::new());

        let mut destructors_called = false;
        for (i, statement) in statements.iter().enumerate() {
            if let Err(interrupt) = self.execute(statement, index) {
                match interrupt {
                    ExecutionInterrupt::Error => (),
                    ExecutionInterrupt::Interrupt(output) => {
                        self.handle_deferred(index);
                        let _ = self.handle_destructors(index, global, statement, "before");
                        destructors_called = true;

                        self.definitions[index].push_indent();
                        self.definitions[index].push(&output);

                        if i != statements.len() - 1 {
                            ast_warning!(statements[i + 1], "Unreachable code");
                            break;
                        }
                    }
                    ExecutionInterrupt::Return(output) => {
                        let deferred = self.deferred.borrow().clone();

                        for statements in deferred.iter().rev() {    
                            for statement in statements.iter().rev() {
                                let _ = self.execute(&statement, index);
                            }
                        }

                        let _ = self.handle_destructors(index, global, statement, "before");
                        destructors_called = true;

                        self.definitions[index].push_indent();
                        self.definitions[index].push(&output);

                        if i != statements.len() - 1 {
                            ast_warning!(statements[i + 1], "Unreachable code");
                            break;
                        }
                    }
                }
            }
        }

        if statements.len() != 0 && !destructors_called {
            self.handle_deferred(index);
            let _ = self.handle_destructors(index, global, statements.last().unwrap(), "after");
        }

        self.deferred.borrow_mut().pop();
        self.environment = previous;
    }

    pub fn execute(&mut self, stmt: &Statement, index: usize) -> Result<Option<SkyeType>, ExecutionInterrupt> {
        match stmt {
            Statement::Empty => (),
            Statement::Expression(expr) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    ast_error!(self, expr, "Only declarations are allowed at top level");
                    ast_note!(expr, "Place this expression inside a function");
                }

                let value = self.evaluate(expr, index, false)?;

                if let SkyeType::Enum(.., base_name) = value.type_ {
                    if base_name.as_ref() == "core_DOT_Result" {
                        ast_warning!(expr, "Error is being ignored implictly");
                        ast_note!(expr, "Handle this error or discard it using the \"let _ = x\" syntax");
                    }
                }

                if value.value.as_ref() != "" { // can happen with try operator returning void
                    self.definitions[index].push_indent();
                    self.definitions[index].push(&value.value);
                    self.definitions[index].push(";\n");
                }
            }
            Statement::VarDecl(name, initializer, type_spec_expr, is_const, qualifiers) => {                
                let value = {
                    if let Some(init) = initializer {
                        Some(self.evaluate(init, index, false)?)
                    } else {
                        None
                    }
                };

                let type_spec = {
                    if let Some(type_) = type_spec_expr {
                        let type_spec_evaluated = self.evaluate(type_, index, false)?;

                        match type_spec_evaluated.type_ {
                            SkyeType::Type(inner_type) => {
                                if inner_type.check_completeness() {
                                    Some(*inner_type)
                                } else {
                                    ast_error!(self, type_, "Cannot use incomplete type directly");
                                    ast_note!(type_, "Define this type or reference it through a pointer");
                                    Some(SkyeType::Void)
                                }
                            }
                            SkyeType::Group(..) => {
                                ast_error!(self, type_, "Cannot use type group for variable declaration");
                                Some(SkyeType::Void)
                            }
                            _ => {
                                ast_error!(
                                    self, type_, 
                                    format!(
                                        "Invalid expression as type specifier (expecting type but got {})",
                                        type_spec_evaluated.type_.stringify_native()
                                    ).as_ref()
                                );

                                Some(SkyeType::Void)
                            }
                        }
                    } else {
                        None
                    }
                };

                if value.is_none() && type_spec.is_none() {
                    token_error!(self, name, "Variable declaration without initializer needs a type specifier");
                    token_note!(name, "Add a type specifier after the variable name");
                    return Err(ExecutionInterrupt::Error);
                }

                if value.is_some() && type_spec.is_some() && !type_spec.as_ref().unwrap().equals(&value.as_ref().unwrap().type_, EqualsLevel::Strict) {
                    ast_error!(
                        self, initializer.as_ref().unwrap(), 
                        format!(
                            "Initializer type ({}) does not match declared type ({})",
                            value.as_ref().unwrap().type_.stringify_native(),
                            type_spec.as_ref().unwrap().stringify_native()
                        ).as_ref()
                    );

                    ast_note!(initializer.as_ref().unwrap(), "Is this expression correct?");
                    ast_note!(type_spec_expr.as_ref().unwrap(), "If the initializer is correct, consider changing or removing the type specifier");
                }

                let type_ = {
                    if let Some(type_spec_) = type_spec {
                        type_spec_
                    } else {
                        value.as_ref().unwrap().type_.finalize()
                    }
                };

                let type_stringified = type_.stringify();
                if type_stringified.len() == 0 {
                    if type_spec_expr.is_some() {
                        ast_error!(
                            self, type_spec_expr.as_ref().unwrap(), 
                            format!(
                                "Invalid expression as type specifier (expecting type but got {})",
                                type_.stringify_native()
                            ).as_ref()
                        );
                    } else {
                        ast_error!(
                            self, initializer.as_ref().unwrap(), 
                            format!(
                                "The type of this expression ({}) cannot be assigned to a variable",
                                type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                if matches!(type_, SkyeType::Void) {
                    token_error!(self, name, "Cannot declare a variable as with type \"void\"");
                    ast_note!(initializer.as_ref().unwrap(), "This expression returns void");
                }

                let is_global = matches!(self.curr_function, CurrentFn::None);
                let is_discard = name.lexeme.as_ref() == "_";

                if is_discard {
                    if let Some(init) = initializer {
                        if is_global {
                            ast_error!(self, init, "Cannot discard a value in the global scope");
                            ast_note!(init, "Move the statement inside a function");
                        }
                    } else {
                        token_error!(self, name, "Cannot use this name for variable declaration");
                        token_note!(name, "Rename this variable");
                        return Err(ExecutionInterrupt::Error);
                    }

                    self.definitions[index].push_indent();
                    self.definitions[index].push(&value.as_ref().unwrap().value);
                    self.definitions[index].push(";\n");
                } else {
                    let full_name = {
                        if is_global {
                            self.get_name(&name.lexeme)
                        } else {
                            Rc::clone(&name.lexeme)
                        }
                    };
    
                    let mut buf = String::new();
    
                    for qualifier in qualifiers {
                        buf.push_str(&qualifier.lexeme);
                        buf.push(' ');
                    }
    
                    buf.push_str(&type_stringified);
                    buf.push(' ');
                    buf.push_str(&full_name);
    
                    if value.is_some() {
                        buf.push_str(" = ");
                        buf.push_str(&value.as_ref().unwrap().value);
                    }
    
                    buf.push_str(";\n");

                    if is_global {
                        if *is_const {
                            token_error!(self, name, "Global constants are not allowed");
                            token_note!(name, "If you want to create a compile-time constant, use a macro");
                        } else if let Some(init) = initializer {
                            ast_error!(self, init, "Cannot assign a value to a global variable directly");
                            ast_note!(init, "Remove the initializer and assign this value through a function");
                        }
    
                        self.declarations.push(CodeOutput::new());
                        self.declarations.last_mut().unwrap().push_indent();
                        self.declarations.last_mut().unwrap().push(&buf);
                    } else {
                        self.definitions[index].push_indent();
                        self.definitions[index].push(&buf);
                    }

                    let mut env = self.environment.borrow_mut();

                    if let Some(var) = env.get_in_scope(&Token::dummy(Rc::clone(&full_name))) {
                        token_error!(self, name, "Cannot declare variable with same name as existing symbol defined in the same scope");
                                
                        if let Some(token) = &var.tok {
                            token_note!(*token, "Previously defined here");
                        }
                    }
                    
                    env.define(
                        Rc::clone(&full_name), SkyeVariable::new(
                            type_, *is_const,
                            Some(Box::new(name.clone()))
                        )
                    );
                }
            }
            Statement::Block(kw, statements) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this block inside a function");
                }

                self.definitions[index].push_indent();

                if statements.len() == 0 {
                    self.definitions[index].push("{}\n");
                } else {
                    self.definitions[index].push("{\n");
                    self.definitions[index].inc_indent();

                    self.execute_block(
                        statements, 
                        Rc::new(RefCell::new(
                            Environment::with_enclosing(
                                Rc::clone(&self.environment)
                            )
                        )),
                        index, false
                    );

                    self.definitions[index].dec_indent();
                    self.definitions[index].push_indent();
                    self.definitions[index].push("}\n");
                }
            }
            Statement::Function(name, params, return_type_expr, body, qualifiers, generics, bind) => {
                let mut full_name = self.get_generics(&self.get_name(&name.lexeme), generics, &self.environment)?;

                let env = self.globals.borrow();
                let search_tok = Token::dummy(Rc::clone(&full_name));
                let existing = env.get(&search_tok);

                let has_decl = {
                    if let Some(var) = &existing {
                        if let SkyeType::Function(.., has_body) = var.type_ {
                            if has_body && body.is_some() {
                                token_error!(self, name, "Cannot redeclare functions");
                            
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }
                                
                                false
                            } else {
                                true
                            }
                        } else {
                            token_error!(self, name, "Cannot declare function with same name as existing symbol in same scope");
                            
                            if let Some(token) = &var.tok {
                                token_note!(*token, "Previously defined here");
                            }
                            
                            false
                        }
                    } else {
                        false
                    }
                };

                drop(env);

                let return_type = self.get_return_type(return_type_expr, index, false)?;

                if has_decl {
                    if let SkyeType::Function(_, existing_return_type, _) = &existing.as_ref().unwrap().type_ {
                        if !existing_return_type.equals(&return_type, EqualsLevel::Typewise) {
                            ast_error!(
                                self, return_type_expr, 
                                format!(
                                    "Function return type ({}) does not match declaration return type ({})",
                                    return_type.stringify_native(), existing_return_type.stringify_native()
                                ).as_ref()
                            );
                        }
                    }
                }

                let return_stringified = return_type.stringify();
                let (params_string, params_output) = self.get_params(params, existing, has_decl, index, false)?;
                let type_ = SkyeType::Function(params_output.clone(), Box::new(return_type.clone()), body.is_some());
                self.generate_fn_signature(name, &type_, &return_stringified, &params_string);

                let has_body = body.is_some();

                // main function handling
                if has_body && full_name.as_ref() == "main" {
                    if *bind {
                        token_error!(self, name, "Cannot bind \"main\" function");
                    }

                    let returns_void        = return_stringified == "void";
                    let returns_i32         = return_stringified == "i32";
                    let returns_i32_result  = return_stringified == "core_DOT_Result_GENOF_void_GENAND_i32_GENEND_";
                    let returns_void_result = return_stringified == "core_DOT_Result_GENOF_void_GENAND_void_GENEND_";
                    
                    let has_stdargs = {
                        params_output.len() == 2 && 
                        params_output[0].type_.equals(&SkyeType::AnyInt, EqualsLevel::Typewise) && 
                        params_output[1].type_.equals(&SkyeType::Pointer(Box::new(SkyeType::Pointer(Box::new(SkyeType::Char), false)), false), EqualsLevel::Typewise)
                    };
                    
                    let has_args = {
                        params_output.len() == 1 &&
                        {
                            if let SkyeType::Struct(.., base_name) = &params_output[0].type_ {
                                base_name.as_ref() == "core_DOT_Array"
                            } else {
                                false
                            }
                        }
                    };

                    let no_args = params_output.len() == 0;

                    if (returns_void | returns_i32 | returns_i32_result | returns_void_result) && (no_args || has_args || has_stdargs) {
                        full_name = Rc::from("_SKYE_MAIN");

                        let real_main_idx = self.definitions.len();
                        self.definitions.push(CodeOutput::new());

                        if returns_void {
                            if has_stdargs {
                                self.definitions[real_main_idx].push(VOID_MAIN_PLUS_STD_ARGS);
                            } else if has_args {
                                self.definitions[real_main_idx].push(VOID_MAIN_PLUS_ARGS);
                            } else {
                                self.definitions[real_main_idx].push(VOID_MAIN);
                            }
                        } else if returns_i32 {
                            if has_stdargs {
                                self.definitions[real_main_idx].push(I32_MAIN_PLUS_STD_ARGS);
                            } else if has_args {
                                self.definitions[real_main_idx].push(I32_MAIN_PLUS_ARGS);
                            } else {
                                self.definitions[real_main_idx].push(I32_MAIN);
                            }
                        } else if returns_i32_result {
                            if has_stdargs {
                                self.definitions[real_main_idx].push(RESULT_I32_MAIN_PLUS_STD_ARGS);
                            } else if has_args {
                                self.definitions[real_main_idx].push(RESULT_I32_MAIN_PLUS_ARGS);
                            } else {
                                self.definitions[real_main_idx].push(RESULT_I32_MAIN);
                            }
                        } else if returns_void_result {
                            if has_stdargs {
                                self.definitions[real_main_idx].push(RESULT_VOID_MAIN_PLUS_STD_ARGS);
                            } else if has_args {
                                self.definitions[real_main_idx].push(RESULT_VOID_MAIN_PLUS_ARGS);
                            } else {
                                self.definitions[real_main_idx].push(RESULT_VOID_MAIN);
                            }
                        }
                    } else {
                        token_error!(self, name, "Invalid function signature for \"main\" function");
                    }
                }

                let mut buf = String::new();

                if !*bind {
                    for qualifier in qualifiers {
                        buf.push_str(&qualifier.lexeme);
                        buf.push(' ');
                    }

                    buf.push_str(&return_stringified);
                    buf.push(' ');
                    buf.push_str(&full_name);
                    buf.push('(');
                    buf.push_str(&params_string);
                    buf.push(')');

                    if (!has_decl) || (!has_body) {
                        self.declarations.push(CodeOutput::new());
                        self.declarations.last_mut().unwrap().push(&buf);
                        self.declarations.last_mut().unwrap().push(";\n");
                    }
                }
                
                let mut fn_environment = None;
                if has_body {
                    fn_environment = Some(Environment::with_enclosing(Rc::clone(&self.environment)));

                    for i in 0 .. params.len() {
                        fn_environment.as_mut().unwrap().define(
                            Rc::clone(&params[i].name.as_ref().unwrap().lexeme), 
                            SkyeVariable::new(
                                params_output[i].type_.clone(), 
                                params_output[i].is_const,
                                Some(Box::new(params[i].name.as_ref().unwrap().clone()))
                            )
                        );
                    }
                }

                let mut env = self.globals.borrow_mut();
                env.define(
                    Rc::clone(&full_name), SkyeVariable::new(
                        type_.clone(), true,
                        Some(Box::new(name.clone()))
                    )
                );
                drop(env);

                if has_body {
                    let enclosing_level = self.curr_function.clone();
                    self.curr_function = CurrentFn::Some(return_type, return_type_expr.clone());

                    let enclosing_deferred = Rc::clone(&self.deferred);
                    self.deferred = Rc::new(RefCell::new(Vec::new()));
                    
                    let new_index = self.definitions.len();
                    self.definitions.push(CodeOutput::new());
                    self.definitions[new_index].push(&buf);
                    self.definitions[new_index].push(" {\n");
                    self.definitions[new_index].inc_indent();

                    self.execute_block(
                        body.as_ref().unwrap(),
                        Rc::new(RefCell::new(fn_environment.unwrap())),
                        new_index, false
                    );

                    self.curr_function = enclosing_level;
                    self.deferred = enclosing_deferred;

                    self.definitions[new_index].dec_indent();
                    self.definitions[new_index].push_indent();
                    self.definitions[new_index].push("}\n\n");
                } 

                return Ok(Some(type_));
            }
            Statement::If(kw, cond_expr, then_branch, else_branch) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this if statement inside a function");
                }

                let cond = self.evaluate(cond_expr, index, false)?;

                match cond.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::AnyInt => (),
                    _ => {
                        ast_error!(
                            self, cond_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type for if condition (got {})",
                                cond.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                let not_grouping = !matches!(cond_expr, Expression::Grouping(_));
                let not_block    = !matches!(**then_branch, Statement::Block(..));

                self.definitions[index].push_indent();
                self.definitions[index].push("if ");

                if not_grouping {
                    self.definitions[index].push("(");
                }

                self.definitions[index].push(&cond.value);

                if not_grouping {
                    self.definitions[index].push(")");
                }

                self.definitions[index].push("\n");

                if not_block {
                    self.definitions[index].push_indent();
                    self.definitions[index].push("{\n");
                    self.definitions[index].inc_indent();

                    self.execute_block(
                        &vec![*then_branch.clone()], 
                        Rc::new(RefCell::new(Environment::with_enclosing(
                            Rc::clone(&self.environment)
                        ))), 
                        index, false
                    );
                } else {
                    let _ = self.execute(&then_branch, index);
                }

                if not_block {
                    self.definitions[index].dec_indent();
                    self.definitions[index].push_indent();
                    self.definitions[index].push("}\n");
                }

                if let Some(else_branch_statement) = else_branch {
                    let not_block = !matches!(**else_branch_statement, Statement::Block(..));

                    self.definitions[index].push_indent();
                    self.definitions[index].push("else\n");

                    if not_block {
                        self.definitions[index].push_indent();
                        self.definitions[index].push("{\n");
                        self.definitions[index].inc_indent();

                        self.execute_block(
                            &vec![*else_branch_statement.clone()], 
                            Rc::new(RefCell::new(Environment::with_enclosing(
                                Rc::clone(&self.environment)
                            ))), 
                            index, false
                        );
                    } else {
                        let _ = self.execute(&else_branch_statement, index);
                    }

                    if not_block {
                        self.definitions[index].dec_indent();
                        self.definitions[index].push_indent();
                        self.definitions[index].push("}\n");
                    }
                }
            }
            Statement::While(kw, cond_expr, body) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this while loop inside a function");
                }

                let not_grouping = !matches!(cond_expr, Expression::Grouping(_));
                let not_block    = !matches!(**body, Statement::Block(..));

                self.definitions[index].push_indent();
                self.definitions[index].push("while (1) {\n");
                self.definitions[index].inc_indent();

                let cond = self.evaluate(cond_expr, index, false)?;

                match cond.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::AnyInt => (),
                    _ => {
                        ast_error!(
                            self, cond_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type for while condition (got {})",
                                cond.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("if (!");

                if not_grouping {
                    self.definitions[index].push("(");
                }

                self.definitions[index].push(&cond.value);

                if not_grouping {
                    self.definitions[index].push(")");
                }

                self.definitions[index].push(") break;\n");

                let break_label = self.get_temporary_var();

                let previous_loop = self.curr_loop.clone();
                self.curr_loop = Some(Rc::from(break_label.clone()));

                if not_block {
                    self.execute_block(
                        &vec![*body.clone()], 
                        Rc::new(RefCell::new(Environment::with_enclosing(
                            Rc::clone(&self.environment)
                        ))), 
                        index, false
                    );
                } else {
                    let _ = self.execute(&body, index);
                }
                
                self.curr_loop = previous_loop;

                self.definitions[index].dec_indent();
                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");

                self.definitions[index].push_indent();
                self.definitions[index].push(&break_label);
                self.definitions[index].push(":;\n");
            }
            Statement::For(kw, initializer, cond_expr, increment, body) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this for loop inside a function");
                }

                let not_block = !matches!(**body, Statement::Block(..));
                let not_grouping = !matches!(cond_expr, Expression::Grouping(_));

                if let Some(init) = initializer {
                    let _ = self.execute(&init, index);
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("for (;; ");

                if let Some(inc_expr) = increment {
                    let inc = self.evaluate(inc_expr, index, false)?;

                    self.definitions[index].push(&inc.value);
                }

                self.definitions[index].push(") {\n");
                self.definitions[index].inc_indent();

                let cond = self.evaluate(cond_expr, index, false)?;

                match cond.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::AnyInt => (),
                    _ => {
                        ast_error!(
                            self, cond_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type for for condition (got {})",
                                cond.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("if (!");

                if not_grouping {
                    self.definitions[index].push("(");
                }

                self.definitions[index].push(&cond.value);

                if not_grouping {
                    self.definitions[index].push(")");
                }

                self.definitions[index].push(") break;\n");

                let break_label = self.get_temporary_var();

                let previous_loop = self.curr_loop.clone();
                self.curr_loop = Some(Rc::from(break_label.clone()));

                if not_block {
                    self.execute_block(
                        &vec![*body.clone()], 
                        Rc::new(RefCell::new(Environment::with_enclosing(
                            Rc::clone(&self.environment)
                        ))), 
                        index, false
                    );
                } else {
                    let _ = self.execute(&body, index);
                }

                self.curr_loop = previous_loop;

                self.definitions[index].dec_indent();
                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");

                self.definitions[index].push_indent();
                self.definitions[index].push(&break_label);
                self.definitions[index].push(":;\n");
            }
            Statement::DoWhile(kw, cond_expr, body) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this do-while loop inside a function");
                }

                let not_grouping = !matches!(cond_expr, Expression::Grouping(_));
                let not_block    = !matches!(**body, Statement::Block(..));

                self.definitions[index].push_indent();
                self.definitions[index].push("while (1) {\n");
                self.definitions[index].inc_indent();

                let break_label = self.get_temporary_var();

                let previous_loop = self.curr_loop.clone();
                self.curr_loop = Some(Rc::from(break_label.clone()));

                if not_block {
                    self.execute_block(
                        &vec![*body.clone()], 
                        Rc::new(RefCell::new(Environment::with_enclosing(
                            Rc::clone(&self.environment)
                        ))), 
                        index, false
                    );
                } else {
                    let _ = self.execute(&body, index);
                }
                
                self.curr_loop = previous_loop;

                let cond = self.evaluate(cond_expr, index, false)?;

                match cond.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::AnyInt => (),
                    _ => {
                        ast_error!(
                            self, cond_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type for while condition (got {})",
                                cond.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("if (!");

                if not_grouping {
                    self.definitions[index].push("(");
                }

                self.definitions[index].push(&cond.value);

                if not_grouping {
                    self.definitions[index].push(")");
                }

                self.definitions[index].push(") break;\n");
                self.definitions[index].dec_indent();

                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");

                self.definitions[index].push_indent();
                self.definitions[index].push(&break_label);
                self.definitions[index].push(":;\n");
            }
            Statement::Return(kw, ret_expr) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Cannot return from top-level code");
                    token_note!(kw, "Remove this return statement");
                }

                let mut buf = String::new();

                if let Some(expr) = ret_expr {
                    let value = self.evaluate(expr, index, false)?;

                    if let CurrentFn::Some(type_, orig_ret_type) = &self.curr_function {
                        if matches!(type_, SkyeType::Void) {
                            ast_error!(self, expr, "Cannot return value in a function that returns void");
                            ast_note!(expr, "Remove this expression");
                            ast_note!(orig_ret_type, "Return type defined here");
                        } else if !type_.equals(&value.type_, EqualsLevel::Typewise) {
                            ast_error!(
                                self, expr, 
                                format!(
                                    "Returned value type ({}) does not match function return type ({})",
                                    value.type_.stringify_native(), type_.stringify_native()
                                ).as_ref()
                            );    

                            ast_note!(orig_ret_type, "Return type defined here");
                        }
                    } else {
                        unreachable!();
                    }

                    let final_value = {
                        let search_tok = Token::dummy(Rc::from("__copy__"));
                        if let Some(method_value) = self.get_method(&value, &search_tok, true) {
                            let copy_constructor = self.call(&method_value, expr, &expr, &Vec::new(), index, false)?;
                            
                            ast_info!(expr, "Skye inserted a copy constructor call for this expression"); // +I-copies
                            copy_constructor
                        } else {
                            value
                        }
                    };

                    // return value is saved in a temporary variable so deferred statements get executed after evaluation
                    let tmp_var_name = self.get_temporary_var();

                    self.definitions[index].push_indent();
                    self.definitions[index].push(&final_value.type_.stringify());
                    self.definitions[index].push(" ");
                    self.definitions[index].push(&tmp_var_name);
                    self.definitions[index].push(" = ");
                    self.definitions[index].push(&final_value.value);
                    self.definitions[index].push(";\n");

                    buf.push_str("return ");
                    buf.push_str(&tmp_var_name);
                    buf.push_str(";\n");
                } else {
                    if let CurrentFn::Some(type_, expr) = &self.curr_function {
                        if !matches!(type_, SkyeType::Void) {
                            token_error!(self, kw, "Cannot return no value in this function");
                            token_note!(kw, "Add a return value");
                            ast_note!(expr, "Return type defined here");
                        }
                    } else {
                        unreachable!();
                    }

                    buf.push_str("return;\n");
                }

                return Err(ExecutionInterrupt::Return(Rc::from(buf.as_ref())))
            }
            Statement::Struct(name, fields, has_body, binding, generics, bind_typedefed) => {
                let base_name = self.get_name(&name.lexeme);
                let full_name = self.get_generics(&base_name, generics, &self.environment)?;

                let env = self.globals.borrow();
                let existing = env.get(
                    &Token::dummy(Rc::clone(&full_name))
                );

                let has_decl = {
                    if let Some(var) = &existing {
                        if let SkyeType::Type(inner_type) = &var.type_ {
                            if let SkyeType::Struct(_, existing_fields, _) = &**inner_type {
                                if *has_body && existing_fields.is_some() {
                                    token_error!(self, name, "Cannot redefine structs");
                                
                                    if let Some(token) = &var.tok {
                                        token_note!(*token, "Previously defined here");
                                    }

                                    false                            
                                } else {
                                    true
                                }
                            } else {
                                token_error!(self, name, "Cannot declare struct with same name as existing symbol in same scope");
                                
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }

                                false
                            }
                        } else {
                            token_error!(self, name, "Cannot declare struct with same name as existing symbol in same scope");
                            
                            if let Some(token) = &var.tok {
                                token_note!(*token, "Previously defined here");
                            }

                            false
                        }
                    } else {
                        false
                    }
                };

                drop(env);

                let mut buf = String::from("typedef ");

                if let Some(bound_name) = binding {
                    if !*bind_typedefed {
                        buf.push_str("struct ");
                    }
                    
                    buf.push_str(&bound_name.lexeme);
                } else {
                    let base_struct_name = self.get_generics(&name.lexeme, generics, &self.environment)?;
                    let full_struct_name = self.get_name(&Rc::from(format!("SKYE_STRUCT_{}", base_struct_name)));
                    buf.push_str("struct ");
                    buf.push_str(&full_struct_name);
                }
                
                buf.push(' ');

                if (!has_decl) || (!*has_body) {
                    self.declarations.push(CodeOutput::new());
                    self.declarations.last_mut().unwrap().push(&buf);
                    self.declarations.last_mut().unwrap().push(&full_name);
                    self.declarations.last_mut().unwrap().push(";\n");
                }

                let mut def_buf = CodeOutput::new();
                
                if *has_body && binding.is_none() {
                    def_buf.push(&buf);
                }

                let type_ = {
                    if *has_body {
                        if binding.is_none() {
                            def_buf.push("{\n");
                            def_buf.inc_indent();
                        }
    
                        let mut output_fields = HashMap::new();
                        for field in fields {
                            let field_type = {
                                let tmp = self.evaluate(&field.expr, index, false)?.type_;

                                match tmp {
                                    SkyeType::Type(inner_type) => {
                                        if inner_type.check_completeness() {
                                            *inner_type
                                        } else {
                                            ast_error!(self, field.expr, "Cannot use incomplete type directly");
                                            ast_note!(field.expr, "Define this type or reference it through a pointer");
                                            SkyeType::Void
                                        }
                                    }
                                    SkyeType::Unknown(_) => tmp,
                                    _ => {
                                        ast_error!(
                                            self, field.expr, 
                                            format!(
                                                "Expecting type as field type (got {})",
                                                tmp.stringify_native()
                                            ).as_ref()
                                        );

                                        SkyeType::Void
                                    }
                                }
                            };
    
                            if output_fields.contains_key(&field.name.lexeme) {
                                token_error!(self, field.name, "Cannot define the same struct field multiple times");
                            } else {
                                let field_type_stringified = field_type.stringify();
                                output_fields.insert(Rc::clone(&field.name.lexeme), (field_type, field.is_const));
                                
                                if binding.is_none() {
                                    def_buf.push_indent();
                                    def_buf.push(&field_type_stringified);
                                    def_buf.push(" ");
                                    def_buf.push(&field.name.lexeme);
                                    def_buf.push(";\n");
                                }
                            }
                        }
                        
                        if binding.is_none() {
                            def_buf.dec_indent();
                            def_buf.push("} ");
                            def_buf.push(&full_name);
                            def_buf.push(";\n\n");
                        }
                        
                        self.struct_definitions.insert(Rc::clone(&full_name), def_buf);
                        self.struct_defs_order.push(Rc::clone(&full_name));

                        SkyeType::Struct(Rc::clone(&full_name), Some(output_fields), base_name)
                    } else {
                        SkyeType::Struct(Rc::clone(&full_name), None, base_name)
                    }
                };

                let output_type = SkyeType::Type(Box::new(type_));

                let mut env = self.globals.borrow_mut();

                env.define(
                    Rc::clone(&full_name), 
                    SkyeVariable::new(
                        output_type.clone(), true,
                        Some(Box::new(name.clone()))
                    )
                );

                return Ok(Some(output_type));
            }
            Statement::Impl(struct_expr, statements) => {
                let struct_name = self.evaluate(struct_expr, index, false)?;

                match &struct_name.type_ {
                    SkyeType::Type(inner_type) => {
                        match inner_type.as_ref() {
                            SkyeType::Struct(..) | 
                            SkyeType::Enum(..) => { 
                                let mut env = self.globals.borrow_mut();
                                env.define(
                                    Rc::from("Self"), 
                                    SkyeVariable::new(
                                        struct_name.type_.clone(), 
                                        true, None
                                    )
                                );
                                drop(env);
    
                                let previous_name = self.curr_name.clone();
                                self.curr_name = struct_name.value.to_string();
                                self.execute_block(statements, Rc::clone(&self.globals), index, true);
                                self.curr_name = previous_name;
    
                                env = self.globals.borrow_mut();
                                env.undef(Rc::from("Self"));
                            }
                            _ => {
                                ast_error!(
                                    self, struct_expr, 
                                    format!(
                                        "Can only implement structs and enums or their templates (got {})",
                                        struct_name.type_.stringify_native()
                                    ).as_ref()
                                );

                                return Err(ExecutionInterrupt::Error);
                            }
                        }
                    }
                    SkyeType::Template(_, definition, ..) => {
                        match definition {
                            Statement::Struct(..) |
                            Statement::Enum(..) => {
                                let mut env = self.globals.borrow_mut();
                                env.define(
                                    Rc::from("Self"), 
                                    SkyeVariable::new(
                                        struct_name.type_.clone(), 
                                        true, None
                                    )
                                );
                                drop(env);
    
                                let previous_name = self.curr_name.clone();
                                self.curr_name = struct_name.value.to_string();
                                self.execute_block(statements, Rc::clone(&self.globals), index, true);
                                self.curr_name = previous_name;
    
                                env = self.globals.borrow_mut();
                                env.undef(Rc::from("Self"));
                            }
                            _ => {
                                ast_error!(
                                    self, struct_expr, 
                                    format!(
                                        "Can only implement structs and enums or their templates (got {})",
                                        struct_name.type_.stringify_native()
                                    ).as_ref()
                                );

                                return Err(ExecutionInterrupt::Error);
                            }
                        }
                    }
                    _ => {
                        ast_error!(
                            self, struct_expr, 
                            format!(
                                "Can only implement structs and enums or their templates (got {})",
                                struct_name.type_.stringify_native()
                            ).as_ref()
                        );

                        return Err(ExecutionInterrupt::Error);
                    }
                }
            }
            Statement::Namespace(name, statements) => {
                if !matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, name, "Namespaces are only allowed in the global scope");
                }

                let full_name = self.get_name(&name.lexeme);

                let mut env = self.globals.borrow_mut();
                if let Some(var) = env.get(name) {
                    if let SkyeType::Namespace(_) = var.type_ {
                        ()
                    } else {
                        token_error!(self, name, "Cannot declare namespace with same name as existing symbol in same scope");
                    
                        if let Some(token) = &var.tok {
                            token_note!(*token, "Previously defined here");
                        }

                        return Err(ExecutionInterrupt::Error);
                    }
                } else {
                    env.define(
                        Rc::clone(&full_name),
                        SkyeVariable::new(
                            SkyeType::Namespace(Rc::clone(&full_name)),
                            true,
                            Some(Box::new(name.clone()))
                        )
                    );
                }
                
                drop(env);

                if statements.len() == 0 {
                    token_error!(self, name, "Cannot create an empty namespace");
                } else {
                    let previous_name = self.curr_name.clone();
                    self.curr_name = full_name.to_string();
                    self.execute_block(statements, Rc::clone(&self.environment), index, true);
                    self.curr_name = previous_name;
                }
            }
            Statement::Use(use_expr, identifier) => {
                let use_value = self.evaluate(use_expr, index, false)?;

                if identifier.lexeme.as_ref() != "_" {
                    if use_value.value.len() != 0 {
                        let mut buf = String::new();

                        buf.push_str("#define ");
                        buf.push_str(&identifier.lexeme);
                        buf.push(' ');
                        buf.push_str(&use_value.value);
                        buf.push('\n');

                        if matches!(self.curr_function, CurrentFn::None) {
                            self.declarations.push(CodeOutput::new());
                            self.declarations.last_mut().unwrap().push(&buf);
                        } else {
                            self.definitions[index].push_indent();
                            self.definitions[index].push(&buf);

                            self.deferred.borrow_mut().last_mut().unwrap().insert(0, Statement::Undef(Rc::clone(&identifier.lexeme)));
                        }
                    }
                    
                    let mut env = self.environment.borrow_mut();
                    env.define(
                        Rc::clone(&identifier.lexeme), SkyeVariable::new(
                            use_value.type_, use_value.is_const, 
                            Some(Box::new(identifier.clone()))
                        )
                    );
                }
            }
            Statement::Undef(name) => {
                self.definitions[index].push_indent();
                self.definitions[index].push("#undef ");
                self.definitions[index].push(&name);
                self.definitions[index].push("\n");

                let mut env = self.environment.borrow_mut();
                env.undef(Rc::clone(name));
            }
            Statement::Enum(name, type_expr, variants, is_simple, has_body, binding, generics, bind_typedefed) => {
                let base_name = self.get_name(&name.lexeme);
                let full_name = self.get_generics(&base_name, generics, &self.environment)?;

                let type_ = {
                    let enum_type = self.evaluate(type_expr, index, false)?.type_;

                    if let SkyeType::Type(inner_type) = &enum_type {
                        match **inner_type {
                            SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                            SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                            SkyeType::Usz => *inner_type.clone(),
                            _ => {
                                ast_error!(
                                    self, type_expr, 
                                    format!(
                                        "Expecting primitive arithmetic type as enum type (got {})",
                                        enum_type.stringify_native()
                                    ).as_ref()
                                );

                                SkyeType::I32
                            }
                        }
                    } else {
                        ast_error!(
                            self, type_expr, 
                            format!(
                                "Expecting type as enum type (got {})",
                                enum_type.stringify_native()
                            ).as_ref()
                        );

                        SkyeType::I32
                    }
                };

                let simple_enum_name = {
                    if *is_simple {
                        Rc::clone(&name.lexeme)
                    } else {
                        Rc::from(format!("{}_DOT_Kind", name.lexeme))
                    }
                };

                let simple_enum_full_name = self.get_name(&simple_enum_name);
                
                let output_type = {
                    if *has_body {
                        let simple_enum_type = SkyeType::Enum(
                            Rc::clone(&simple_enum_full_name), None,
                            Rc::clone(&simple_enum_full_name)
                        );

                        let env = self.globals.borrow();
                        let search_tok = Token::dummy(Rc::clone(&simple_enum_full_name));
                        if let Some(var) = env.get(&search_tok) {
                            drop(env);
                            if generics.len() == 0 {
                                token_error!(self, name, "Cannot redefine enums");
                            
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }
                            }
                        } else {
                            self.declarations.push(CodeOutput::new());
                            self.declarations.last_mut().unwrap().push("typedef ");
    
                            if let Some(bound_name) = binding {
                                if !*bind_typedefed {
                                    self.declarations.last_mut().unwrap().push("enum ");
                                }

                                self.declarations.last_mut().unwrap().push(&bound_name.lexeme);
                                self.declarations.last_mut().unwrap().push(": ");
                                self.declarations.last_mut().unwrap().push(&type_.stringify());
                                self.declarations.last_mut().unwrap().push(" ");
                                self.declarations.last_mut().unwrap().push(&full_name);
                                self.declarations.last_mut().unwrap().push(";\n");
                            } else {
                                self.declarations.last_mut().unwrap().push("enum ");
                                
                                let full_struct_name = self.get_name(&Rc::from(format!("SKYE_ENUM_{}", simple_enum_name)));
                                self.declarations.last_mut().unwrap().push(&full_struct_name);
                                self.declarations.last_mut().unwrap().push(": ");
                                self.declarations.last_mut().unwrap().push(&type_.stringify());
                                self.declarations.last_mut().unwrap().push(" {\n");
                                self.declarations.last_mut().unwrap().inc_indent();
        
                                for (i, variant) in variants.iter().enumerate() {
                                    self.declarations.last_mut().unwrap().push_indent();
                                    self.declarations.last_mut().unwrap().push(&simple_enum_full_name);
                                    self.declarations.last_mut().unwrap().push("_DOT_");
                                    self.declarations.last_mut().unwrap().push(&variant.name.lexeme);
        
                                    if i != variants.len() - 1 {
                                        self.declarations.last_mut().unwrap().push(",\n");
                                    }
                                }
        
                                self.declarations.last_mut().unwrap().dec_indent();
                                self.declarations.last_mut().unwrap().push("\n} ");
                                self.declarations.last_mut().unwrap().push(&simple_enum_full_name);
                                self.declarations.last_mut().unwrap().push(";\n");
                            }

                            drop(env);
                            let mut env = self.globals.borrow_mut();
                            env.define(
                                Rc::clone(&simple_enum_full_name), 
                                SkyeVariable::new(
                                    SkyeType::Type(Box::new(simple_enum_type.clone())), 
                                    true, Some(Box::new(name.clone()))
                                )
                            );
                        }

                        let write_output = binding.is_none() && !*is_simple;
                        let base_struct_name = self.get_generics(&name.lexeme, generics, &self.environment)?;
                        let full_struct_name = self.get_name(&Rc::from(format!("SKYE_STRUCT_{}", base_struct_name)));

                        let mut def_buf = CodeOutput::new();

                        if write_output {
                            let mut buf = String::from("typedef struct ");
                            buf.push_str(&full_struct_name);
                            buf.push(' ');

                            self.declarations.push(CodeOutput::new());
                            self.declarations.last_mut().unwrap().push(&buf);
                            self.declarations.last_mut().unwrap().push(&full_name);
                            self.declarations.last_mut().unwrap().push(";\n");
                            
                            def_buf.push(&buf);
                            def_buf.push("{\n");
                            def_buf.inc_indent();

                            def_buf.push_indent();
                            def_buf.push("union {\n");
                            def_buf.inc_indent();
                        }   
    
                        let mut output_fields = HashMap::new();
                        let mut initializers = CodeOutput::new();
                        let mut evaluated_variants = Vec::new();
                        for variant in variants {
                            let variant_type = {
                                let type_ = self.evaluate(&variant.expr, index, false)?.type_;
                                match type_ {
                                    SkyeType::Void | SkyeType::Unknown(_) => type_,
                                    SkyeType::Type(inner_type) => {
                                        if inner_type.check_completeness() {
                                            *inner_type
                                        } else {
                                            ast_error!(self, variant.expr, "Cannot use incomplete type directly");
                                            ast_note!(variant.expr, "Define this type or reference it through a pointer");
                                            SkyeType::Void
                                        }
                                    }
                                    _ => {
                                        ast_error!(
                                            self, variant.expr, 
                                            format!(
                                                "Expecting type as enum variant type (got {})",
                                                type_.stringify_native()
                                            ).as_ref()
                                        );

                                        SkyeType::Void
                                    }
                                }
                            };

                            evaluated_variants.push(SkyeEnumVariant::new(
                                variant.name.clone(), 
                                variant_type.clone())
                            );

                            let mut env = self.globals.borrow_mut();
                            if binding.is_some() {
                                env.define(
                                    Rc::clone(&variant.name.lexeme), 
                                    SkyeVariable::new(
                                        type_.clone(), true,
                                        Some(Box::new(variant.name.clone()))
                                    )
                                );
                            } else {
                                env.define(
                                    Rc::from(format!("{}_DOT_{}", simple_enum_full_name, variant.name.lexeme)), 
                                    SkyeVariable::new(
                                        type_.clone(), true,
                                        Some(Box::new(variant.name.clone()))
                                    )
                                );
                            }
                            
                            drop(env);
    
                            let is_void = matches!(variant_type, SkyeType::Void);
                            let is_not_void = !is_void;
                            
                            let lowercase_variant = variant.name.lexeme.to_lowercase();
    
                            if write_output {
                                let mut buf = String::new();
                                buf.push_str(&full_name);
                                buf.push(' ');
                                buf.push_str(&full_name);
                                buf.push_str("_DOT_");

                                if is_void {
                                    buf.push_str("SKYE_ENUM_INIT_");
                                }

                                buf.push_str(&variant.name.lexeme);
                                buf.push_str("(");

                                if is_not_void {
                                    buf.push_str(&variant_type.stringify());
                                    buf.push_str(" value");
                                } 

                                buf.push(')');

                                self.declarations.push(CodeOutput::new());
                                self.declarations.last_mut().unwrap().push(&buf);
                                self.declarations.last_mut().unwrap().push(";\n");

                                initializers.push_indent();
                                initializers.push(&buf);
                                initializers.push(" {\n");
                                initializers.inc_indent();
    
                                initializers.push_indent();
                                initializers.push(&full_name);
                                initializers.push(" tmp;\n");
    
                                initializers.push_indent();
                                initializers.push("tmp.kind = ");
                                initializers.push(&simple_enum_full_name);
                                initializers.push("_DOT_");
                                initializers.push(&variant.name.lexeme);
                                initializers.push(";\n");
    
                                if is_not_void {
                                    initializers.push_indent();
                                    initializers.push("tmp.");
                                    initializers.push(&lowercase_variant);
                                    initializers.push(" = value;\n");
                                }
    
                                initializers.push_indent();
                                initializers.push("return tmp;\n");
                                initializers.dec_indent();
    
                                initializers.push_indent();
                                initializers.push("}\n\n");
                            }
                            
                            if is_void {
                                if write_output {
                                    self.declarations.push(CodeOutput::new());
                                    self.declarations.last_mut().unwrap().push("#define ");
                                    self.declarations.last_mut().unwrap().push(&full_name);
                                    self.declarations.last_mut().unwrap().push("_DOT_");
                                    self.declarations.last_mut().unwrap().push(&variant.name.lexeme);
                                    self.declarations.last_mut().unwrap().push(" ");
                                    self.declarations.last_mut().unwrap().push(&full_name);
                                    self.declarations.last_mut().unwrap().push("_DOT_SKYE_ENUM_INIT_");
                                    self.declarations.last_mut().unwrap().push(&variant.name.lexeme);
                                    self.declarations.last_mut().unwrap().push("()\n");
                                }
                            } else {
                                if write_output {
                                    def_buf.push_indent();
                                    def_buf.push(&variant_type.stringify());
                                    def_buf.push(" ");
                                    def_buf.push(&lowercase_variant);
                                    def_buf.push(";\n");
                                }
                                
                                output_fields.insert(Rc::from(lowercase_variant), variant_type);
                            }
                        }
    
                        if write_output {
                            output_fields.insert(Rc::from("kind"), simple_enum_type.clone());

                            def_buf.dec_indent();
                            def_buf.push_indent();
                            def_buf.push("};\n\n");
    
                            def_buf.push_indent();
                            def_buf.push(&simple_enum_full_name);
                            def_buf.push(" kind;\n");
                            def_buf.dec_indent();
    
                            def_buf.push_indent();
                            def_buf.push("} ");
                            def_buf.push(&full_name);
                            def_buf.push(";\n\n");
    
                            def_buf.push(&initializers.code);

                            let struct_output_type = SkyeType::Enum(
                                Rc::clone(&full_name), Some(output_fields),
                                Rc::clone(&base_name)
                            );
    
                            for variant in evaluated_variants {
                                let mut env = self.globals.borrow_mut();
    
                                if matches!(variant.type_, SkyeType::Void) {
                                    env.define(
                                        Rc::from(format!("{}_DOT_{}", full_name, variant.name.lexeme)), 
                                        SkyeVariable::new(
                                            struct_output_type.clone(),
                                            true,
                                            Some(Box::new(variant.name.clone()))
                                        )
                                    );
    
                                    let type_ = SkyeType::Function(
                                        Vec::new(), Box::new(struct_output_type.clone()), 
                                        true
                                    );
    
                                    env.define(
                                        Rc::from(format!("{}_DOT_SKYE_ENUM_INIT_{}", full_name, variant.name.lexeme)), 
                                        SkyeVariable::new(
                                            type_.clone(), true,
                                            Some(Box::new(variant.name.clone()))
                                        )
                                    );
    
                                    drop(env);

                                    self.generate_fn_signature(
                                        &variant.name, &type_, 
                                        &struct_output_type.stringify(), 
                                        &String::new()
                                    );
                                } else {
                                    let type_ = SkyeType::Function(
                                        vec![SkyeFunctionParam::new(variant.type_.clone(), true)], 
                                        Box::new(struct_output_type.clone()), 
                                        true
                                    );
    
                                    env.define(
                                        Rc::from(format!("{}_DOT_{}", full_name, variant.name.lexeme)), 
                                        SkyeVariable::new(
                                            type_.clone(), true,
                                            Some(Box::new(variant.name.clone()))
                                        )
                                    );
    
                                    drop(env);

                                    self.generate_fn_signature(
                                        &variant.name, &type_, 
                                        &struct_output_type.stringify(), 
                                        &variant.type_.stringify()
                                    );
                                }
                            }

                            self.struct_definitions.insert(Rc::clone(&full_name), def_buf);
                            self.struct_defs_order.push(Rc::clone(&full_name));

                            Some(struct_output_type)
                        } else {
                            Some(simple_enum_type)
                        }
                    } else {
                        if binding.is_none() {
                            let full_struct_name = self.get_name(&Rc::from(format!("SKYE_STRUCT_{}", simple_enum_name)));
                            self.declarations.push(CodeOutput::new());
                            self.declarations.last_mut().unwrap().push("typedef struct ");
                            self.declarations.last_mut().unwrap().push(&full_struct_name);
                            self.declarations.last_mut().unwrap().push(" ");
                            self.declarations.last_mut().unwrap().push(&full_name);
                            self.declarations.last_mut().unwrap().push(";\n");
                        } 
    
                        Some(SkyeType::Enum(Rc::clone(&full_name), None, base_name))
                    }
                };

                let mut env = self.globals.borrow_mut();
                let existing = env.get(&Token::dummy(Rc::clone(&full_name)));

                if let Some(var) = &existing {
                    if let SkyeType::Type(inner_type) = &var.type_ {
                        if let SkyeType::Enum(_, existing_fields, _) = &**inner_type {
                            if *has_body && existing_fields.is_some() {
                                token_error!(self, name, "Cannot redefine enums");
                            
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }                            
                            } 
                        } else {
                            token_error!(self, name, "Cannot declare enum with same name as existing symbol in same scope");
                            
                            if let Some(token) = &var.tok {
                                token_note!(*token, "Previously defined here");
                            }
                        }
                    } else {
                        token_error!(self, name, "Cannot declare enum with same name as existing symbol in same scope");
                        
                        if let Some(token) = &var.tok {
                            token_note!(*token, "Previously defined here");
                        }
                    }
                } 

                if let Some(out) = output_type {
                    let final_type = SkyeType::Type(Box::new(out));

                    env.define(
                        Rc::clone(&full_name), 
                        SkyeVariable::new(
                            final_type.clone(), true,
                            Some(Box::new(name.clone()))
                        )
                    );

                    return Ok(Some(final_type));
                }
            }
            Statement::Defer(kw, statement) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Remove this defer statement");
                }

                match &**statement {
                    Statement::Return(kw, _) | Statement::Break(kw) | 
                    Statement::Continue(kw) | Statement::Defer(kw, _) => {
                        token_error!(self, kw, "Cannot use this statement inside a defer statement");
                    }
                    _ => ()
                }

                self.deferred.borrow_mut().last_mut().unwrap().push(*statement.clone());
            }
            Statement::Switch(kw, switch_expr, cases) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Remove this switch statement");
                }

                let is_not_grouping = !matches!(switch_expr, Expression::Grouping(_));
                let switch = self.evaluate(switch_expr, index, false)?;
                match &switch.type_ {
                    SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                    SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                    SkyeType::Usz | SkyeType::F32 | SkyeType::F64 | SkyeType::AnyInt |
                    SkyeType::AnyFloat | SkyeType::Char => (),
                    SkyeType::Enum(_, variants, _) => {
                        if variants.is_some() {
                            ast_error!(
                                self, switch_expr, 
                                format!(
                                    "Expecting expression of primitive arithmetic type or simple enum for switch condition (got {})",
                                    switch.type_.stringify_native()
                                ).as_ref()
                            );
                        }
                    }
                    _ => {
                        ast_error!(
                            self, switch_expr, 
                            format!(
                                "Expecting expression of primitive arithmetic type or simple enum for switch condition (got {})",
                                switch.type_.stringify_native()
                            ).as_ref()
                        );
                    }
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("switch ");

                if is_not_grouping {
                    self.definitions[index].push("(");
                }   

                self.definitions[index].push(&switch.value);

                if is_not_grouping {
                    self.definitions[index].push(")");
                }

                self.definitions[index].push(" {\n");
                self.definitions[index].inc_indent();

                for case in cases {
                    if let Some(real_cases) = &case.cases {
                        for (i, real_case) in real_cases.iter().enumerate() {
                            self.definitions[index].push_indent();
                            self.definitions[index].push("case ");

                            let real_case_evaluated = self.evaluate(real_case, index, false)?;
                            match real_case_evaluated.type_ {
                                SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
                                SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
                                SkyeType::Usz | SkyeType::F32 | SkyeType::F64 | SkyeType::AnyInt |
                                SkyeType::AnyFloat | SkyeType::Char => (),
                                _ => {
                                    ast_error!(
                                        self, real_case, 
                                        format!(
                                            "Expecting expression of primitive arithmetic type for case expression (got {})",
                                            real_case_evaluated.type_.stringify_native()
                                        ).as_ref()
                                    );
                                }
                            }

                            self.definitions[index].push(&real_case_evaluated.value);
                            self.definitions[index].push(":");

                            if i != real_cases.len() - 1 {
                                self.definitions[index].push("\n");
                            } else {
                                self.definitions[index].push(" ");
                            }
                        }
                    } else {
                        self.definitions[index].push_indent();
                        self.definitions[index].push("default: ");
                    }

                    self.definitions[index].push("{\n");
                    self.definitions[index].inc_indent();

                    self.execute_block(
                        &case.code, 
                        Rc::new(RefCell::new(
                            Environment::with_enclosing(
                                Rc::clone(&self.environment)
                            )
                        )),
                        index, false
                    );

                    self.definitions[index].dec_indent();
                    self.definitions[index].push_indent();
                    self.definitions[index].push("} break;\n");
                }

                self.definitions[index].dec_indent();
                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");
            }
            Statement::Template(name, definition, generics, generics_names) => {
                let full_name = self.get_name(&name.lexeme);

                let mut generics_evaluated = Vec::new();
                for generic in generics {
                    let bounds_type = {
                        if let Some(bounds) = &generic.bounds {
                            let evaluated = self.evaluate(bounds, index, false)?;
    
                            if evaluated.type_.is_type() || matches!(evaluated.type_, SkyeType::Void) {
                                Some(evaluated.type_)
                            } else {
                                ast_error!(
                                    self, bounds, 
                                    format!(
                                        "Expecting type or group as generic bound (got {})",
                                        evaluated.type_.stringify_native()
                                    ).as_ref()
                                );

                                None
                            }
                        } else {
                            None
                        }
                    };

                    let default_type = {
                        if let Some(default) = &generic.default {
                            let evaluated = self.evaluate(default, index, false)?;
    
                            if matches!(evaluated.type_, SkyeType::Type(_)) || matches!(evaluated.type_, SkyeType::Void) {
                                if evaluated.type_.check_completeness() {
                                    Some(evaluated.type_)
                                } else {
                                    ast_error!(self, default, "Cannot use incomplete type directly");
                                    ast_note!(default, "Define this type or reference it through a pointer");
                                    None
                                }
                            } else {
                                ast_error!(
                                    self, default, 
                                    format!(
                                        "Expecting type as default generic (got {})",
                                        evaluated.type_.stringify_native()
                                    ).as_ref()
                                );

                                None
                            }
                        } else {
                            None
                        }
                    };
                    
                    generics_evaluated.push(SkyeGeneric::new(generic.name.clone(), bounds_type, default_type));
                }
                
                let mut env = self.globals.borrow_mut();
                let cloned_globals = Rc::new(RefCell::new(env.clone()));
                env.define(
                    Rc::clone(&full_name), 
                    SkyeVariable::new(
                        SkyeType::Template(
                            full_name, *definition.clone(),
                            generics_evaluated, generics_names.clone(),
                            self.curr_name.clone(), cloned_globals
                        ), 
                        true, 
                        Some(Box::new(name.clone()))
                    )
                );
            }
            Statement::Break(kw) => {
                if let Some(label) = &self.curr_loop {                    
                    return Err(ExecutionInterrupt::Interrupt(Rc::from(format!("goto {};\n", label))));
                } else {
                    token_error!(self, kw, "Can only use break inside loops");
                }
            }
            Statement::Continue(kw) => {
                if self.curr_loop.is_some() {
                    return Err(ExecutionInterrupt::Interrupt(Rc::from("continue;\n")));
                } else {
                    token_error!(self, kw, "Can only use continue inside loops");
                }
            }
            Statement::Import(path_tok, import_type) => {
                let mut path = PathBuf::from(path_tok.lexeme.as_ref());

                let skye_import = {
                    let fetched_extension = {
                        if let Some(extension) = path.extension() {
                            Some(OsString::from(extension))
                        } else {
                            None
                        }
                    };

                    if let Some(extension) = fetched_extension {
                        if *import_type == ImportType::Lib {
                            match env::var(SKYE_PATH_VAR) {
                                Ok(val) => path = PathBuf::from(val).join("lib").join(path),
                                Err(e) => {
                                    token_error!(self, path_tok, format!("An error occurred while trying to load the SKYE_PATH variable. Error: {}", e.to_string()).as_ref());
                                    token_note!(path_tok, "Is the environment variable set?");
                                    return Err(ExecutionInterrupt::Error);
                                }
                            }
                        } else if path.is_relative() && self.source_path.is_some() && *import_type != ImportType::Ang {
                            path = PathBuf::from((**self.source_path.as_ref().unwrap()).clone()).join(path);
                        } else {
                            path = path_tok.lexeme.split('/').collect();
                        }

                        extension == "skye"
                    } else if path.is_relative() {
                        match env::var(SKYE_PATH_VAR) {
                            Ok(val) => {
                                path = PathBuf::from(val).join("lib").join(path).with_extension("skye");
                                true
                            }
                            Err(e) => {
                                token_error!(self, path_tok, format!("An error occurred while trying to load the SKYE_PATH variable. Error: {}", e.to_string()).as_ref());
                                token_note!(path_tok, "Is the environment variable set?");
                                return Err(ExecutionInterrupt::Error);
                            }
                        }
                    } else {
                        token_error!(self, path_tok, "A file extension is required on absolute path imports for Skye to know what kind of import to perform");
                        token_note!(path_tok, "Add the file extension (\".skye\", \".c\", \".h\", ...)");
                        return Err(ExecutionInterrupt::Error);
                    }
                };

                if skye_import {
                    match parse_file(path.as_os_str()) {
                        Ok(statements) => self.compile_internal(statements, index),
                        Err(e) => {
                            token_error!(self, path_tok, format!("Could not import this file. Error: {}", e.to_string()).as_ref());
                        }
                    }
                } else {
                    let mut buf = String::from("#include ");
                    
                    let is_ang = *import_type == ImportType::Ang;
                    if is_ang {
                        buf.push('<');
                    } else {
                        buf.push('"');
                    }

                    buf.push_str(&path.to_str().expect("Error converting to string"));

                    if is_ang {
                        buf.push('>');
                    } else {
                        buf.push('"');
                    }

                    buf.push('\n');

                    if matches!(self.curr_function, CurrentFn::None) {
                        self.includes.push(&buf);
                    } else {
                        self.definitions[index].push(&buf);
                    }
                }
            }
            Statement::Union(name, fields, has_body, binding, bind_typedefed) => {
                let full_name = self.get_name(&name.lexeme);

                let env = self.globals.borrow();
                let existing = env.get(&Token::dummy(Rc::clone(&full_name)));

                let has_decl = {
                    if let Some(var) = &existing {
                        if let SkyeType::Type(inner_type) = &var.type_ {
                            if let SkyeType::Union(_, existing_fields) = &**inner_type {
                                if *has_body && existing_fields.is_some() {
                                    token_error!(self, name, "Cannot redefine unions");
                                
                                    if let Some(token) = &var.tok {
                                        token_note!(*token, "Previously defined here");
                                    }

                                    false                            
                                } else {
                                    true
                                }
                            } else {
                                token_error!(self, name, "Cannot declare union with same name as existing symbol in same scope");
                                
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }

                                false
                            }
                        } else {
                            token_error!(self, name, "Cannot declare union with same name as existing symbol in same scope");
                            
                            if let Some(token) = &var.tok {
                                token_note!(*token, "Previously defined here");
                            }

                            false
                        }
                    } else {
                        false
                    }
                };

                drop(env);

                let mut buf = String::from("typedef ");

                if let Some(bound_name) = binding {
                    if !*bind_typedefed {
                        buf.push_str("union ");
                    }

                    buf.push_str(&bound_name.lexeme);
                } else {
                    buf.push_str("union ");
                    let full_union_name = self.get_name(&Rc::from(format!("SKYE_UNION_{}", name.lexeme)));
                    buf.push_str(&full_union_name);
                }
                
                buf.push(' ');

                if (!has_decl) || (!*has_body) {
                    self.declarations.push(CodeOutput::new());
                    self.declarations.last_mut().unwrap().push(&buf);
                    self.declarations.last_mut().unwrap().push(&full_name);
                    self.declarations.last_mut().unwrap().push(";\n");
                }

                let mut def_buf = CodeOutput::new();
                
                if *has_body && binding.is_none() {
                    def_buf.push(&buf);
                }

                let type_ = {
                    if *has_body {
                        if binding.is_none() {
                            def_buf.push("{\n");
                            def_buf.inc_indent();
                        }
    
                        let mut output_fields = HashMap::new();
                        for field in fields {
                            let field_type = {
                                let inner_field_type = self.evaluate(&field.expr, index, false)?.type_;
                                
                                if let SkyeType::Type(inner_type) = inner_field_type {
                                    if inner_type.check_completeness() {
                                        *inner_type
                                    } else {
                                        ast_error!(self, field.expr, "Cannot use incomplete type directly");
                                        ast_note!(field.expr, "Define this type or reference it through a pointer");
                                        SkyeType::Void
                                    }          
                                    
                                } else {
                                    ast_error!(
                                        self, field.expr, 
                                        format!(
                                            "Expecting type as field type (got {})",
                                            inner_field_type.stringify_native()
                                        ).as_ref()
                                    );

                                    SkyeType::Void
                                }
                            };
    
                            if output_fields.contains_key(&field.name.lexeme) {
                                token_error!(self, field.name, "Cannot define the same union field multiple times");
                            } else {
                                let field_type_stringified = field_type.stringify();
                                output_fields.insert(Rc::clone(&field.name.lexeme), field_type);
                                
                                if binding.is_none() {
                                    def_buf.push_indent();
                                    def_buf.push(&field_type_stringified);
                                    def_buf.push(" ");
                                    def_buf.push(&field.name.lexeme);
                                    def_buf.push(";\n");
                                }
                            }
                        }
                        
                        if binding.is_none() {
                            def_buf.dec_indent();
                            def_buf.push("} ");
                            def_buf.push(&full_name);
                            def_buf.push(";\n\n");
                        }

                        self.struct_definitions.insert(Rc::clone(&full_name), def_buf);
                        self.struct_defs_order.push(Rc::clone(&full_name));

                        SkyeType::Union(Rc::clone(&full_name), Some(output_fields))
                    } else {
                        SkyeType::Union(Rc::clone(&full_name), None)
                    }
                };

                let output_type = SkyeType::Type(Box::new(type_));

                let mut env = self.globals.borrow_mut();

                env.define(
                    Rc::clone(&full_name), 
                    SkyeVariable::new(
                        output_type.clone(), true,
                        Some(Box::new(name.clone()))
                    )
                );
            }
            Statement::Bitfield(name, fields, has_body, binding, bind_typedefed) => {
                let full_name = self.get_name(&name.lexeme);

                let env = self.globals.borrow();
                let existing = env.get(&Token::dummy(Rc::clone(&full_name)));

                let has_decl = {
                    if let Some(var) = &existing {
                        if let SkyeType::Type(inner_type) = &var.type_ {
                            if let SkyeType::Bitfield(_, existing_fields) = &**inner_type {
                                if *has_body && existing_fields.is_some() {
                                    token_error!(self, name, "Cannot redefine bitfields");
                                
                                    if let Some(token) = &var.tok {
                                        token_note!(*token, "Previously defined here");
                                    }

                                    false                            
                                } else {
                                    true
                                }
                            } else {
                                token_error!(self, name, "Cannot declare union with same name as existing symbol in same scope");
                                
                                if let Some(token) = &var.tok {
                                    token_note!(*token, "Previously defined here");
                                }

                                false
                            }
                        } else {
                            token_error!(self, name, "Cannot declare union with same name as existing symbol in same scope");
                            
                            if let Some(token) = &var.tok {
                                token_note!(*token, "Previously defined here");
                            }

                            false
                        }
                    } else {
                        false
                    }
                };

                drop(env);

                let mut buf = String::from("typedef ");

                if let Some(bound_name) = binding {
                    if !*bind_typedefed {
                        buf.push_str("struct ");
                    }
                    
                    buf.push_str(&bound_name.lexeme);
                } else {
                    buf.push_str("struct ");
                    let full_struct_name = self.get_name(&Rc::from(format!("SKYE_STRUCT_{}", name.lexeme)));
                    buf.push_str(&full_struct_name);
                }
                
                buf.push(' ');

                if (!has_decl) || (!*has_body) {
                    self.declarations.push(CodeOutput::new());
                    self.declarations.last_mut().unwrap().push(&buf);
                    self.declarations.last_mut().unwrap().push(&full_name);
                    self.declarations.last_mut().unwrap().push(";\n");
                }
                
                let mut def_buf = CodeOutput::new();
                
                if *has_body && binding.is_none() {
                    def_buf.push(&buf);
                }

                let type_ = {
                    if *has_body {
                        if binding.is_none() {
                            def_buf.push("{\n");
                            def_buf.inc_indent();
                        }
    
                        let mut output_fields = HashMap::new();
                        for field in fields {
                            let field_type = {
                                match field.bits {
                                    0  ..= 8  => SkyeType::U8,
                                    9  ..= 16 => SkyeType::U16,
                                    17 ..= 32 => SkyeType::U32,
                                    33 ..= 64 => SkyeType::U64,
                                    _ => unreachable!() // parser ensures this is unreachable
                                }
                            };
    
                            if output_fields.contains_key(&field.name.lexeme) {
                                token_error!(self, field.name, "Cannot define the same bitfield field multiple times");
                            } else {
                                let field_type_stringified = field_type.stringify();
                                output_fields.insert(Rc::clone(&field.name.lexeme), field_type);
                                
                                if binding.is_none() {
                                    def_buf.push_indent();
                                    def_buf.push(&field_type_stringified);
                                    def_buf.push(" ");
                                    def_buf.push(&field.name.lexeme);
                                    def_buf.push(format!(": {}", field.bits).as_ref());
                                    def_buf.push(";\n");
                                }
                            }
                        }
                        
                        if binding.is_none() {
                            def_buf.dec_indent();
                            def_buf.push("} ");
                            def_buf.push(&full_name);
                            def_buf.push(";\n\n");
                        }

                        self.struct_definitions.insert(Rc::clone(&full_name), def_buf);
                        self.struct_defs_order.push(Rc::clone(&full_name));

                        SkyeType::Bitfield(Rc::clone(&full_name), Some(output_fields))
                    } else {
                        SkyeType::Bitfield(Rc::clone(&full_name), None)
                    }
                };

                let output_type = SkyeType::Type(Box::new(type_));

                let mut env = self.globals.borrow_mut();

                env.define(
                    Rc::clone(&full_name), 
                    SkyeVariable::new(
                        output_type.clone(), true,
                        Some(Box::new(name.clone()))
                    )
                );
            }
            Statement::Macro(name, params, return_expr, return_type) => {
                if self.curr_name != "" {
                    token_warning!(name, "Macros do not support namespaces. This macro will be saved in the global namespace"); // +Wmacro-namespace
                }

                let mut env = self.globals.borrow_mut();
                env.define(
                    Rc::clone(&name.lexeme), 
                    SkyeVariable::new(
                        SkyeType::Type(Box::new(
                            SkyeType::Macro(
                                Rc::clone(&name.lexeme),
                                params.clone(), 
                                return_expr.clone(), 
                                return_type.clone()
                            )
                        )), 
                        true,
                        Some(Box::new(name.clone()))
                    )
                );
            }
            Statement::Foreach(kw, var_name, iterator_expr, body) => {
                if matches!(self.curr_function, CurrentFn::None) {
                    token_error!(self, kw, "Only declarations are allowed at top level");
                    token_note!(kw, "Place this for loop inside a function");
                }

                let iterator_raw = self.evaluate(iterator_expr, index, false)?;

                let tmp_iter_var_name = self.get_temporary_var();

                if !matches!(iterator_raw.type_, SkyeType::Struct(..) | SkyeType::Enum(..)) {
                    ast_error!(
                        self, iterator_expr, 
                        format!(
                            "This type ({}) is not iterable",
                            iterator_raw.type_.stringify_native()
                        ).as_ref()
                    );

                    return Err(ExecutionInterrupt::Error);
                }

                self.definitions[index].push_indent();
                self.definitions[index].push("{\n");
                self.definitions[index].inc_indent();

                self.definitions[index].push_indent();
                self.definitions[index].push(&iterator_raw.type_.stringify());
                self.definitions[index].push(" ");
                self.definitions[index].push(&tmp_iter_var_name);
                self.definitions[index].push(" = ");
                self.definitions[index].push(&iterator_raw.value);
                self.definitions[index].push(";\n");

                let iterator = SkyeValue::new(Rc::from(tmp_iter_var_name.as_ref()), iterator_raw.type_.clone(), iterator_raw.is_const);

                let mut search_tok = Token::dummy(Rc::from("next"));
                let method = {
                    if let Some(method) = self.get_method(&iterator, &search_tok, false) {
                        method
                    } else {
                        search_tok.set_lexeme("iter");
    
                        if let Some(method) = self.get_method(&iterator, &search_tok, false) {
                            let iterator_call = self.call(
                                &method, iterator_expr, iterator_expr, 
                                &Vec::new(), index, false
                            )?;

                            let iterator_type_stringified = iterator_call.type_.stringify();
                            if iterator_type_stringified.len() == 0 || !matches!(iterator.type_, SkyeType::Struct(..) | SkyeType::Enum(..)) {
                                ast_error!(
                                    self, iterator_expr, 
                                    format!(
                                        "The implementation of iter for this type ({}) returns an invalid type (expecting struct or enum type but got {})",
                                        iterator.type_.stringify_native(), iterator_call.type_.stringify_native()
                                    ).as_ref()
                                );

                                return Err(ExecutionInterrupt::Error);
                            }

                            let tmp_var_name = self.get_temporary_var();
                            let iterator_val = SkyeValue::new(Rc::from(tmp_var_name.as_ref()), iterator_call.type_, false);

                            search_tok.set_lexeme("next");
                            if let Some(final_method) = self.get_method(&iterator_val, &search_tok, false) {
                                self.definitions[index].push_indent();
                                self.definitions[index].push(&iterator_type_stringified);
                                self.definitions[index].push(" ");
                                self.definitions[index].push(&tmp_var_name);
                                self.definitions[index].push(" = ");
                                self.definitions[index].push(&iterator_call.value);
                                self.definitions[index].push(";\n");

                                final_method
                            } else {
                                ast_error!(
                                    self, iterator_expr, 
                                    format!(
                                        "The iterator object (of type {}) returned by iter has no next method",
                                        iterator_val.type_.stringify_native()
                                    ).as_ref()
                                );

                                return Err(ExecutionInterrupt::Error);
                            }
                        } else {
                            ast_error!(
                                self, iterator_expr, 
                                format!(
                                    "Type {} is not iterable",
                                    iterator_raw.type_.stringify_native()
                                ).as_ref()
                            );

                            return Err(ExecutionInterrupt::Error);
                        }
                    }
                };

                let next_call = self.call(
                    &method, iterator_expr, iterator_expr, 
                    &Vec::new(), index, false
                )?;

                let item_type = {
                    if let SkyeType::Enum(_, variants, name) = &next_call.type_ {
                        if name.as_ref() != "core_DOT_Option" {
                            ast_error!(
                                self, iterator_expr, 
                                format!(
                                    "The implementation of next for this iterator returns an invalid type (expecting core::Option but got {})",
                                    next_call.type_.stringify_native()
                                ).as_ref()
                            );

                            return Err(ExecutionInterrupt::Error);
                        }

                        variants.as_ref().unwrap().get("some").unwrap().clone()
                    } else {
                        ast_error!(
                            self, iterator_expr, 
                            format!(
                                "The implementation of next for this iterator returns an invalid type (expecting core::Option but got {})",
                                next_call.type_.stringify_native()
                            ).as_ref()
                        );

                        return Err(ExecutionInterrupt::Error);
                    }
                };

                let item_type_stringified = item_type.stringify();
                if item_type_stringified.len() == 0 {
                    ast_error!(
                        self, iterator_expr, 
                        format!(
                            "The implementation of next for this iterator returns an invalid type (expecting core::Option but got {})",
                            next_call.type_.stringify_native()
                        ).as_ref()
                    );

                    return Err(ExecutionInterrupt::Error);
                }

                let previous = Rc::clone(&self.environment);
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(&self.environment))));

                let mut env = self.environment.borrow_mut();
                env.define(
                    Rc::clone(&var_name.lexeme),
                    SkyeVariable::new(
                        item_type,
                        true,
                        Some(Box::new(var_name.clone()))
                    )
                );
                drop(env);

                let tmp_item_var_name = self.get_temporary_var();
                
                self.definitions[index].push_indent();
                self.definitions[index].push(&next_call.type_.stringify());
                self.definitions[index].push(" ");
                self.definitions[index].push(&tmp_item_var_name);
                self.definitions[index].push(";\n");

                self.definitions[index].push_indent();
                self.definitions[index].push("while ((");
                self.definitions[index].push(&tmp_item_var_name);
                self.definitions[index].push(" = ");
                self.definitions[index].push(&next_call.value);
                self.definitions[index].push(").kind == core_DOT_Option_DOT_Kind_DOT_Some) {\n");
                self.definitions[index].inc_indent();

                self.definitions[index].push_indent();
                self.definitions[index].push(&item_type_stringified);
                self.definitions[index].push(" ");
                self.definitions[index].push(&var_name.lexeme);
                self.definitions[index].push(" = ");
                self.definitions[index].push(&tmp_item_var_name);
                self.definitions[index].push(".some;\n");

                let break_label = self.get_temporary_var();

                let previous_loop = self.curr_loop.clone();
                self.curr_loop = Some(Rc::from(break_label.clone()));

                if matches!(**body, Statement::Block(..)) {
                    self.execute_block(
                        &vec![*body.clone()], 
                        Rc::new(RefCell::new(Environment::with_enclosing(
                            Rc::clone(&self.environment)
                        ))), 
                        index, false
                    );
                } else {
                    let _ = self.execute(&body, index);
                }
                
                self.curr_loop = previous_loop;
                self.environment = previous;

                self.definitions[index].dec_indent();
                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");

                self.definitions[index].dec_indent();
                self.definitions[index].push_indent();
                self.definitions[index].push("}\n");
                
                self.definitions[index].push_indent();
                self.definitions[index].push(&break_label);
                self.definitions[index].push(":;\n");
            }
        }

        Ok(None)
    }

    fn compile_internal(&mut self, statements: Vec<Statement>, index: usize) {
        for statement in statements {
            let _ = self.execute(&statement, index);
        }
    }

    pub fn compile(&mut self, statements: Vec<Statement>) {
        self.compile_internal(statements, 0);
    }

    pub fn get_output(&self) -> Option<String> {
        if self.had_error {
            None
        } else {
            let mut output = String::from("// Hello from Skye!! ^_^\n\n");

            if self.includes.code.len() != 0 {
                output.push_str(&self.includes.code);
                output.push('\n');
            }

            if self.strings_code.code.len() != 0 {
                output.push_str(&self.strings_code.code);
                output.push('\n');
            }
            
            if self.declarations.len() != 0 {
                for declaration in &self.declarations {
                    if !declaration.code.contains("_UNKNOWN_") {
                        output.push_str(&declaration.code);
                    }
                }

                output.push('\n');
            }

            for definition in &self.struct_defs_order {
                if !definition.contains("_UNKNOWN_") {
                    output.push_str(&self.struct_definitions.get(definition).unwrap().code);
                }
            }
            
            for definition in &self.definitions {
                output.push_str(&definition.code);
            }

            Some(output)
        }
    }
}