use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::{Expression, Statement}, environment::Environment, tokens::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct SkyeFunctionParam {
    pub type_: SkyeType,
    pub is_const: bool
}

impl SkyeFunctionParam {
    pub fn new(type_: SkyeType, is_const: bool) -> Self {
        SkyeFunctionParam { type_, is_const }
    }
}

#[derive(Debug, Clone)]
pub enum GetResult {
    Ok(Rc<str>, SkyeType, bool), // value type is_const
    InvalidType,
    FieldNotFound,
    Undefined
}

#[derive(Debug, Clone, PartialEq)]
pub struct SkyeEnumVariant {
    pub name: Token,
    pub type_: SkyeType
}

impl SkyeEnumVariant {
    pub fn new(name: Token, type_: SkyeType) -> Self {
        SkyeEnumVariant { name, type_ }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SkyeGeneric {
    pub name: Token,
    pub bounds: Option<SkyeType>,
    pub default: Option<SkyeType>
}

impl SkyeGeneric {
    pub fn new(name: Token, bounds: Option<SkyeType>, default: Option<SkyeType>) -> Self {
        SkyeGeneric { name, bounds, default }
    }
}

pub enum Operator {
    Inc, Dec,
    Pos, Neg,
    Not, Inv,
    Ref, ConstRef,
    Deref, ConstDeref,
    Add, Sub, Div, Mul, Mod,
    Shl, Shr,
    Or, And,
    BitOr, BitAnd, Xor,
    Gt, Ge, Lt, Le, Eq, Ne,
    SetAdd, SetSub, SetMul, SetDiv, SetMod,
    SetShl, SetShr,
    SetAnd, SetXor, SetOr,
    Subscript,
    AsPtr
}

pub enum ImplementsHow {
    Native(Vec<SkyeType>),
    ThirdParty,
    No
}

pub enum CastableHow {
    Yes,
    No,
    ConstnessLoss
}

#[derive(Clone, Copy)]
pub enum EqualsLevel {
    ConstStrict,
    Strict,
    Typewise,
    Permissive
}

const ALL_INTS: &[SkyeType] = &[
    SkyeType::U8, SkyeType::U16, SkyeType::U32, SkyeType::U64, SkyeType::Usz,
    SkyeType::I8, SkyeType::I16, SkyeType::I32, SkyeType::I64, SkyeType::AnyInt
];

#[derive(Clone, PartialEq)]
pub enum SkyeType {
    U8, U16, U32, U64, Usz,
    I8, I16, I32, I64, AnyInt,
    F32, F64, AnyFloat,
    Char,

    Void,
    Unknown(Rc<str>), // used for type inference

    Pointer(Box<SkyeType>, bool), // type is_const
    Type(Box<SkyeType>),
    Group(Box<SkyeType>, Box<SkyeType>), // left right
    Function(Vec<SkyeFunctionParam>, Box<SkyeType>, bool), // params return_type has_body
    Struct(Rc<str>, Option<HashMap<Rc<str>, (SkyeType, bool)>>, Rc<str>), // name fields base_name
    Namespace(Rc<str>), // name
    Enum(Rc<str>, Option<HashMap<Rc<str>, SkyeType>>, Rc<str>), // name variants base_name
    Template(Rc<str>, Statement, Vec<SkyeGeneric>, Vec<Token>, String, Rc<RefCell<Environment>>), // name definition generics generics_names curr_name environment
    Union(Rc<str>, Option<HashMap<Rc<str>, SkyeType>>), // name fields
    Bitfield(Rc<str>, Option<HashMap<Rc<str>, SkyeType>>), // name fields
    Macro(Rc<str>, Option<Vec<Token>>, Option<Expression>, Option<Expression>), // name params return_expr return_type
}

impl std::fmt::Debug for SkyeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "U8"),
            Self::U16 => write!(f, "U16"),
            Self::U32 => write!(f, "U32"),
            Self::U64 => write!(f, "U64"),
            Self::Usz => write!(f, "Usz"),
            Self::I8 => write!(f, "I8"),
            Self::I16 => write!(f, "I16"),
            Self::I32 => write!(f, "I32"),
            Self::I64 => write!(f, "I64"),
            Self::AnyInt => write!(f, "AnyInt"),
            Self::F32 => write!(f, "F32"),
            Self::F64 => write!(f, "F64"),
            Self::AnyFloat => write!(f, "AnyFloat"),
            Self::Char => write!(f, "Char"),
            Self::Void => write!(f, "Void"),
            Self::Unknown(arg0) => f.debug_tuple("Unknown").field(arg0).finish(),
            Self::Pointer(arg0, arg1) => f.debug_tuple("Pointer").field(arg0).field(arg1).finish(),
            Self::Type(arg0) => f.debug_tuple("Type").field(arg0).finish(),
            Self::Group(arg0, arg1) => f.debug_tuple("Group").field(arg0).field(arg1).finish(),
            Self::Function(arg0, arg1, arg2) => f.debug_tuple("Function").field(arg0).field(arg1).field(arg2).finish(),
            Self::Struct(arg0, arg1, arg2) => f.debug_tuple("Struct").field(arg0).field(arg1).field(arg2).finish(),
            Self::Namespace(arg0) => f.debug_tuple("Namespace").field(arg0).finish(),
            Self::Enum(arg0, arg1, arg2) => f.debug_tuple("Enum").field(arg0).field(arg1).field(arg2).finish(),
            Self::Template(arg0, arg1, arg2, arg3, arg4, _) => f.debug_tuple("Template").field(arg0).field(arg1).field(arg2).field(arg3).field(arg4).finish(),
            Self::Union(arg0, arg1) => f.debug_tuple("Union").field(arg0).field(arg1).finish(),
            Self::Bitfield(arg0, arg1) => f.debug_tuple("Bitfield").field(arg0).field(arg1).finish(),
            Self::Macro(arg0, arg1, arg2, arg3) => f.debug_tuple("Macro").field(arg0).field(arg1).field(arg2).field(arg3).finish(),
        }
    }
}

impl SkyeType {
    pub fn stringify(&self) -> String {
        match self {
            SkyeType::U8  => String::from("u8"),
            SkyeType::I8  => String::from("i8"),
            SkyeType::U16 => String::from("u16"),
            SkyeType::I16 => String::from("i16"),
            SkyeType::U32 => String::from("u32"),
            SkyeType::U64 => String::from("u64"),
            SkyeType::I64 => String::from("i64"),
            SkyeType::F64 => String::from("f64"),
            SkyeType::Usz => String::from("usz"),
            SkyeType::I32 | SkyeType::AnyInt   => String::from("i32"),
            SkyeType::F32 | SkyeType::AnyFloat => String::from("f32"),
            
            SkyeType::Char => String::from("char"),

            SkyeType::Void       => String::from("void"),
            SkyeType::Unknown(_) => String::from("void*"),

            SkyeType::Group(..) | 
            SkyeType::Template(..) | 
            SkyeType::Macro(..) => String::new(),
            
            SkyeType::Type(inner) => inner.stringify(),
            SkyeType::Function(..) => self.mangle(),

            SkyeType::Pointer(inner, _) => {
                String::from(format!("{}*", inner.stringify()))
            }

            SkyeType::Struct(name, ..) | 
            SkyeType::Namespace(name) |
            SkyeType::Enum(name, ..) | 
            SkyeType::Union(name, _) | 
            SkyeType::Bitfield(name, _) => name.to_string(),
        }
    }

    pub fn stringify_native(&self) -> String {
        match self {
            SkyeType::U8  => String::from("u8"),
            SkyeType::I8  => String::from("i8"),
            SkyeType::U16 => String::from("u16"),
            SkyeType::I16 => String::from("i16"),
            SkyeType::U32 => String::from("u32"),
            SkyeType::U64 => String::from("u64"),
            SkyeType::I64 => String::from("i64"),
            SkyeType::F64 => String::from("f64"),
            SkyeType::Usz => String::from("usz"),
            SkyeType::I32 => String::from("i32"),
            SkyeType::F32 => String::from("f32"),
            SkyeType::AnyInt   => String::from("AnyInt"),
            SkyeType::AnyFloat => String::from("AnyFloat"),
            
            SkyeType::Char => String::from("char"),
            SkyeType::Void => String::from("void"),

            SkyeType::Unknown(name) => format!("unknown \"{}\"", name),
            SkyeType::Group(left, right) => format!("{} | {}", left.stringify_native(), right.stringify_native()),
            SkyeType::Template(name, ..) => format!("template \"{}\"", name.replace("_DOT_", "::")),
            SkyeType::Namespace(name) => format!("namespace \"{}\"", name.replace("_DOT_", "::")),
            SkyeType::Macro(name, ..) => format!("macro {}", name),
            
            SkyeType::Type(inner) => format!("type \"{}\"", inner.stringify_native()),
            SkyeType::Function(args, return_type, _) => {
                let mut buf = String::from("fn (");
                for (i, arg) in args.iter().enumerate() {
                    if arg.is_const {
                        buf.push_str("const ");
                    }

                    buf.push_str(&arg.type_.stringify_native());

                    if i != args.len() - 1 {
                        buf.push_str(", ");
                    }
                }

                buf.push_str(") ");
                buf.push_str(&return_type.stringify_native());
                buf
            }

            SkyeType::Pointer(inner, is_const) => {
                if *is_const {
                    String::from(format!("*const {}", inner.stringify_native()))
                } else {
                    String::from(format!("*{}", inner.stringify_native()))
                }
            }

            SkyeType::Struct(name, ..) | 
            SkyeType::Enum(name, ..) => {
                // not ideal, but it's just error reporting ¯\_(ツ)_/¯
                name.to_string()
                    .replace("_DOT_", "::")
                    .replace("_FNPTR_", "fn (")
                    .replace("_PARAM_AND_", ", ")
                    .replace("_PARAM_END_", ") ")
                    .replace("_FNPTR_END_", "")
                    .replace("_GENOF_", "[")
                    .replace("_GENAND_", ", ")
                    .replace("_GENEND_", "]")
                    .replace("_UNKNOWN_", "{any}")
            }
            
            SkyeType::Union(name, _) | 
            SkyeType::Bitfield(name, _) => name.to_string().replace("_DOT_", "::"),
        }
    }

    pub fn mangle(&self) -> String {
        match self {
            SkyeType::U8  => String::from("u8"),
            SkyeType::I8  => String::from("i8"),
            SkyeType::U16 => String::from("u16"),
            SkyeType::I16 => String::from("i16"),
            SkyeType::U32 => String::from("u32"),
            SkyeType::U64 => String::from("u64"),
            SkyeType::I64 => String::from("i64"),
            SkyeType::F64 => String::from("f64"),
            SkyeType::Usz => String::from("usz"),
            SkyeType::I32 | SkyeType::AnyInt   => String::from("i32"),
            SkyeType::F32 | SkyeType::AnyFloat => String::from("f32"),
            
            SkyeType::Char       => String::from("char"),
            SkyeType::Void       => String::from("void"),
            SkyeType::Unknown(_) => String::from("_UNKNOWN_"),

            SkyeType::Group(..) | 
            SkyeType::Namespace(_) | 
            SkyeType::Template(..) | 
            SkyeType::Macro(..) => String::new(),
            
            SkyeType::Type(inner) => inner.mangle(),
            SkyeType::Struct(name, ..) | 
            SkyeType::Enum(name, ..) |
            SkyeType::Union(name, _) | 
            SkyeType::Bitfield(name, _) => name.to_string(),

            SkyeType::Pointer(inner, _) => {
                let inner_mangled = inner.mangle();
                if inner_mangled.len() == 0 {
                    return inner_mangled;
                }

                String::from(format!("_PTROF_{}_PTREND_", inner_mangled))
            },

            SkyeType::Function(params, return_type, _) => {
                let ret_type_mangled = return_type.mangle();
                if ret_type_mangled.len() == 0 {
                    return ret_type_mangled;
                }

                let mut output_params = String::new();
                for i in 0 .. params.len() {
                    let param = params[i].type_.mangle();
                    if param.len() == 0 {
                        return param;
                    }

                    output_params.push_str(&param);

                    if i != params.len() - 1 {
                        output_params.push_str("_PARAM_AND_");
                    }
                }

                String::from(format!("_FNPTR_{}_PARAM_END_{}_FNPTR_END_", output_params, ret_type_mangled))
            }
        }
    }

    pub fn equals(&self, other: &SkyeType, level: EqualsLevel) -> bool {
        match self {
            SkyeType::U8  => matches!(other, SkyeType::U8)  || matches!(other, SkyeType::AnyInt),
            SkyeType::I8  => matches!(other, SkyeType::I8)  || matches!(other, SkyeType::AnyInt),
            SkyeType::U16 => matches!(other, SkyeType::U16) || matches!(other, SkyeType::AnyInt),
            SkyeType::I16 => matches!(other, SkyeType::I16) || matches!(other, SkyeType::AnyInt),
            SkyeType::U32 => matches!(other, SkyeType::U32) || matches!(other, SkyeType::AnyInt),
            SkyeType::I32 => matches!(other, SkyeType::I32) || matches!(other, SkyeType::AnyInt),
            SkyeType::U64 => matches!(other, SkyeType::U64) || matches!(other, SkyeType::AnyInt),
            SkyeType::I64 => matches!(other, SkyeType::I64) || matches!(other, SkyeType::AnyInt),
            SkyeType::Usz => matches!(other, SkyeType::Usz) || matches!(other, SkyeType::AnyInt),
            SkyeType::F32 => matches!(other, SkyeType::F32) || matches!(other, SkyeType::AnyFloat),
            SkyeType::F64 => matches!(other, SkyeType::F64) || matches!(other, SkyeType::AnyFloat),
            SkyeType::AnyInt   => matches!(other, SkyeType::AnyInt)   || other.equals(self, level),
            SkyeType::AnyFloat => matches!(other, SkyeType::AnyFloat) || other.equals(self, level),

            SkyeType::Char => matches!(other, SkyeType::Char),
            SkyeType::Void => matches!(other, SkyeType::Void),

            SkyeType::Group(..) | 
            SkyeType::Namespace(_) | 
            SkyeType::Template(..) | 
            SkyeType::Macro(..) => false,

            SkyeType::Unknown(_) => true,

            SkyeType::Type(self_inner) => {
                if let SkyeType::Type(other_inner) = other {
                    self_inner.equals(other_inner, level)
                } else {
                    false
                }
            }
            SkyeType::Pointer(self_inner, self_is_const) => {
                match level {
                    EqualsLevel::Typewise => {
                        if let SkyeType::Pointer(other_inner, _) = other {
                            self_inner.equals(other_inner, level)
                        } else {
                            false
                        }
                    }
                    EqualsLevel::ConstStrict => {
                        if let SkyeType::Pointer(other_inner, other_is_const) = other {
                            !(self_is_const ^ other_is_const) && self_inner.equals(other_inner, level)
                        } else {
                            false
                        }
                    }
                    _ => {
                        if let SkyeType::Pointer(other_inner, other_is_const) = other {
                            if *self_is_const {
                                self_inner.equals(other_inner, level)
                            } else {
                                (!*other_is_const) && self_inner.equals(other_inner, level)
                            }
                        } else {
                            false
                        }
                    }
                }
            }
            SkyeType::Function(self_params, self_return_type, _) => {
                if let SkyeType::Function(other_params, other_return_type, _) = other {
                    if self_params.len() != other_params.len() || !self_return_type.equals(other_return_type, level) {
                        false
                    } else {
                        for i in 0..self_params.len() {
                            if matches!(level, EqualsLevel::ConstStrict) && (self_params[i].is_const ^ other_params[i].is_const) {
                                return false;
                            }

                            if !self_params[i].type_.equals(&other_params[i].type_, level) {
                                return false;
                            }
                        }

                        true
                    }
                } else {
                    false
                }
            }
            SkyeType::Struct(self_name, _, self_base_name) => {
                if matches!(level, EqualsLevel::Permissive) {
                    if let SkyeType::Struct(.., other_base_name) = other {
                        self_base_name == other_base_name
                    } else {
                        false
                    }
                } else {
                    if let SkyeType::Struct(other_name, ..) = other {
                        self_name == other_name
                    } else {
                        false
                    }
                }
            }
            SkyeType::Enum(self_name, _, self_base_name) => {
                if matches!(level, EqualsLevel::Permissive) {
                    if let SkyeType::Enum(.., other_base_name) = other {
                        self_base_name == other_base_name
                    } else {
                        false
                    }
                } else {
                    if let SkyeType::Enum(other_name, ..) = other {
                        self_name == other_name
                    } else {
                        false
                    }
                }
            }
            SkyeType::Union(self_name, _) => {
                if let SkyeType::Union(other_name, _) = other {
                    self_name == other_name
                } else {
                    false
                }
            }
            SkyeType::Bitfield(self_name, _) => {
                if let SkyeType::Bitfield(other_name, _) = other {
                    self_name == other_name
                } else {
                    false
                }
            }
        }
    }

    // checks if `other` respects `self`, where `self` is a generic type bound
    pub fn is_respected_by(&self, other: &SkyeType) -> bool {
        match self {
            SkyeType::Group(left, right) => {
                left.is_respected_by(other) || right.is_respected_by(other)
            }
            _ => self.equals(other, EqualsLevel::Typewise)
        }
    }

    pub fn is_type(&self) -> bool {
        matches!(self, SkyeType::Type(_) | SkyeType::Group(..))
    }

    pub fn finalize(&self) -> SkyeType {
        match self {
            SkyeType::AnyInt   => SkyeType::I32,
            SkyeType::AnyFloat => SkyeType::F32,
            _ => self.clone()
        }
    }

    fn get_internal(&self, from: &Rc<str>, name: &Token, is_source_const: bool, d: usize) -> GetResult {
        match self {
            SkyeType::Pointer(inner_type, is_pointer_const) => {
                let inner_res = inner_type.get_internal(from, name, *is_pointer_const, d + 1);
                if let GetResult::Ok(inner_str, type_, is_const) = inner_res {
                    if d == 0 {
                        if let SkyeType::Pointer(..) = **inner_type {
                            GetResult::Ok(Rc::from(format!("({})->{}", inner_str, name.lexeme)), type_, *is_pointer_const || is_const)
                        } else {
                            GetResult::Ok(Rc::from(format!("{}->{}", inner_str, name.lexeme)), type_, *is_pointer_const || is_const)
                        }
                    } else {
                        GetResult::Ok(Rc::from(format!("*{}", inner_str)), type_, *is_pointer_const || is_const)
                    }
                } else {
                    inner_res
                }
            }
            SkyeType::Struct(_, fields, _) => {
                if let Some(defined_fields) = fields {
                    if let Some((field, is_const)) = defined_fields.get(&name.lexeme) {
                        if d == 0 {
                            GetResult::Ok(Rc::from(format!("{}.{}", from, name.lexeme)), field.clone(), is_source_const || *is_const)
                        } else {
                            GetResult::Ok(Rc::clone(from), field.clone(), is_source_const || *is_const)
                        }
                    } else {
                        GetResult::FieldNotFound
                    }
                } else {
                    GetResult::Undefined
                }
            }
            SkyeType::Enum(_, fields, _) => {
                if let Some(defined_fields) = fields {
                    if let Some(field) = defined_fields.get(&name.lexeme) {
                        if d == 0 {
                            GetResult::Ok(Rc::from(format!("{}.{}", from, name.lexeme)), field.clone(), true)
                        } else {
                            GetResult::Ok(Rc::clone(from), field.clone(), true)
                        }
                    } else {
                        GetResult::FieldNotFound
                    }
                } else {
                    GetResult::InvalidType
                }
            }
            SkyeType::Union(_, fields) | SkyeType::Bitfield(_, fields) => {
                if let Some(defined_fields) = fields {
                    if let Some(field) = defined_fields.get(&name.lexeme) {
                        if d == 0 {
                            GetResult::Ok(Rc::from(format!("{}.{}", from, name.lexeme)), field.clone(), is_source_const)
                        } else {
                            GetResult::Ok(Rc::clone(from), field.clone(), is_source_const)
                        }
                    } else {
                        GetResult::FieldNotFound
                    }
                } else {
                    GetResult::InvalidType
                }
            }
            _ => GetResult::InvalidType
        }
    }

    pub fn get(&self, from: &Rc<str>, name: &Token, is_source_const: bool) -> GetResult {
        self.get_internal(from, name, is_source_const, 0)
    }

    fn static_get_internal(&self, name: &Token, d: usize) -> GetResult {
        match self {
            SkyeType::Pointer(inner_type, _) => inner_type.static_get_internal(name, d + 1),
            SkyeType::Type(inner_type) => {
                if d == 0 {
                    inner_type.static_get_internal(name, d + 1)
                } else {
                    GetResult::InvalidType
                }
            }
            SkyeType::Namespace(namespace_name) | 
            SkyeType::Struct(.., namespace_name) |
            SkyeType::Enum(.., namespace_name) | 
            SkyeType::Template(namespace_name, ..) => {
                GetResult::Ok(Rc::from(format!("{}_DOT_{}", namespace_name, name.lexeme)), SkyeType::Void, false)
            }
            _ => GetResult::InvalidType
        }
    }

    pub fn static_get(&self, name: &Token) -> GetResult {
        self.static_get_internal(name, 0)
    }

    pub fn get_method(&self, name: &Token, strict: bool) -> GetResult {
        match self {
            SkyeType::Pointer(inner_type, _) => {
                if strict {
                    GetResult::InvalidType
                } else {
                    inner_type.get_method(name, strict)
                }
            }
            SkyeType::Struct(.., obj_name) |
            SkyeType::Enum(.., obj_name) | 
            SkyeType::Template(obj_name, ..) => {
                GetResult::Ok(Rc::from(format!("{}_DOT_{}", obj_name, name.lexeme)), SkyeType::Void, false)
            }
            _ => GetResult::InvalidType
        }
    }

    fn get_self_internal(&self, from: &Rc<str>, is_source_const: bool, d: usize) -> Option<(Rc<str>, SkyeType)> {
        match self {
            SkyeType::Pointer(inner_type, is_const) => {
                if d == 0 {
                    inner_type.get_self_internal(from, *is_const, d + 1)
                } else {
                    let (inner_val, inner_type) = inner_type.get_self_internal(from, *is_const, d + 1)?;
                    Some((Rc::from(format!("*{}", inner_val)), inner_type))
                }
            }
            SkyeType::Struct(..) | SkyeType::Enum(..) => {
                if d == 0 {
                    Some((Rc::from(format!("&{}", from)), SkyeType::Pointer(Box::new(self.clone()), is_source_const)))
                } else {
                    Some((Rc::clone(from), SkyeType::Pointer(Box::new(self.clone()), is_source_const)))
                }
            }
            _ => None
        }
    }

    pub fn get_self(&self, from: &Rc<str>, is_source_const: bool) -> Option<(Rc<str>, SkyeType)> {
        self.get_self_internal(from, is_source_const, 0)
    }

    fn infer_type_from_similar_internal(&self, other: &SkyeType, data: Rc<RefCell<HashMap<Rc<str>, SkyeType>>>) -> Option<()> {
        if !self.equals(other, EqualsLevel::Permissive) {
            return None;
        }

        match self {
            SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
            SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 |
            SkyeType::Usz | SkyeType::F32 | SkyeType::F64 | SkyeType::AnyInt |
            SkyeType::AnyFloat | SkyeType::Char | SkyeType::Void | 
            SkyeType::Group(..) | SkyeType::Namespace(_) | SkyeType::Template(..) | 
            SkyeType::Macro(..) => (),

            SkyeType::Unknown(name) => {
                if let SkyeType::Pointer(inner_type, _) = other {
                    data.borrow_mut().insert(Rc::clone(name), SkyeType::Pointer(inner_type.clone(), false));
                } else {
                    data.borrow_mut().insert(Rc::clone(name), other.clone());
                }
            }
           
            SkyeType::Pointer(self_inner_type, _) | 
            SkyeType::Type(self_inner_type) => {
                match other {
                    SkyeType::Pointer(other_inner_type, _) | 
                    SkyeType::Type(other_inner_type) => {
                        self_inner_type.infer_type_from_similar_internal(other_inner_type, data)?;
                    }
                    _ => unreachable!()
                }
            }

            SkyeType::Function(self_params, self_return, _) => {
                if let SkyeType::Function(other_params, other_return, _) = other {
                    for i in 0 .. self_params.len() {
                        self_params[i].type_.infer_type_from_similar_internal(&other_params[i].type_, Rc::clone(&data))?;
                    }

                    self_return.infer_type_from_similar_internal(&other_return, data)?;
                } else {
                    unreachable!()
                }
            }

            SkyeType::Struct(_, self_fields, _) => {
                if let SkyeType::Struct(_, other_fields, _) = other {
                    if let Some(real_self_fields) = self_fields {
                        if let Some(real_other_fields) = other_fields {
                            for (key, (value, _)) in real_self_fields {
                                if let Some((field, _)) = real_other_fields.get(key) {
                                    value.infer_type_from_similar_internal(field, Rc::clone(&data))?;
                                }
                            }
                        }
                    }
                } else {
                    unreachable!()
                }
            } 
            SkyeType::Union(_, self_fields) | 
            SkyeType::Bitfield(_, self_fields) => {
                match other {
                    SkyeType::Union(_, other_fields) | 
                    SkyeType::Bitfield(_, other_fields) => {
                        if let Some(real_self_fields) = self_fields {
                            if let Some(real_other_fields) = other_fields {
                                for (key, value) in real_self_fields {
                                    if let Some(field) = real_other_fields.get(key) {
                                        value.infer_type_from_similar_internal(field, Rc::clone(&data))?;
                                    }
                                }
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            SkyeType::Enum(_, self_fields, _) => {
                match other {
                    SkyeType::Enum(_, other_fields, _) => {
                        if let Some(real_self_fields) = self_fields {
                            if let Some(real_other_fields) = other_fields {
                                if real_self_fields.len() >= real_other_fields.len() {
                                    for (key, value) in real_self_fields {
                                        if let Some(field) = real_other_fields.get(key) {
                                            value.infer_type_from_similar_internal(field, Rc::clone(&data))?;
                                        } else {
                                            // if variant is not there in enum and they are equal, it means that the variant type is void
                                            value.infer_type_from_similar_internal(&SkyeType::Void, Rc::clone(&data))?;
                                        }
                                    }
                                } else {
                                    for (key, value) in real_other_fields {
                                        if let Some(field) = real_self_fields.get(key) {
                                            value.infer_type_from_similar_internal(field, Rc::clone(&data))?;
                                        } else {
                                            value.infer_type_from_similar_internal(&SkyeType::Void, Rc::clone(&data))?;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
        }

        Some(())
    }

    pub fn infer_type_from_similar(&self, other: &SkyeType) -> Option<HashMap<Rc<str>, SkyeType>> {
        let data = Rc::new(RefCell::new(HashMap::new()));
        self.infer_type_from_similar_internal(other, Rc::clone(&data))?;
        let result = data.borrow().clone();
        Some(result)
    }

    pub fn implements_op(&self, op: Operator) -> ImplementsHow {
        match self {
            SkyeType::Unknown(_) => ImplementsHow::Native(Vec::new()),
            SkyeType::Pointer(..) => {
                match op {
                    Operator::Add | Operator::Sub | Operator::Div | Operator::Mul | Operator::Mod |
                    Operator::Eq  | Operator::Ne => ImplementsHow::Native(ALL_INTS.into()),
                    _ => ImplementsHow::Native(Vec::new())
                }
            }

            // at this stage, the compiler can't know whether the operator is implemented or not, 
            // so it assumes it is, that way it can try to find the relative function
            SkyeType::Template(..) | SkyeType::Struct(..) => ImplementsHow::ThirdParty,

            SkyeType::Enum(_, variants, _) => {
                if variants.is_none() {
                    if matches!(op, Operator::Eq | Operator::Ne) {
                        ImplementsHow::Native(ALL_INTS.into())
                    } else {
                        ImplementsHow::No
                    }
                } else {
                    ImplementsHow::ThirdParty
                }
            }

            SkyeType::Void | SkyeType::Type(_) | SkyeType::Group(..) | 
            SkyeType::Namespace(_) | SkyeType::Macro(..) => {
                ImplementsHow::No
            }

            SkyeType::Union(..) | SkyeType::Function(..) | SkyeType::Bitfield(..) => {
                if matches!(op, Operator::Ref | Operator::ConstRef) {
                    ImplementsHow::Native(Vec::new())
                } else {
                    ImplementsHow::No
                }
            }

            SkyeType::U8  | SkyeType::I8  | SkyeType::U16 | SkyeType::I16 |
            SkyeType::U32 | SkyeType::I32 | SkyeType::U64 | SkyeType::I64 | 
            SkyeType::Usz | SkyeType::AnyInt => {
                match op {
                    Operator::Subscript | Operator::Deref | Operator::ConstDeref | Operator::AsPtr => ImplementsHow::No,
                    _ => ImplementsHow::Native(vec![SkyeType::Char])
                }
            }

            SkyeType::F32 | SkyeType::F64 | SkyeType::AnyFloat => {
                match op {
                    Operator::Subscript | Operator::Deref | Operator::ConstDeref | Operator::AsPtr => ImplementsHow::No,
                    _ => ImplementsHow::Native(Vec::new())
                }
            }

            SkyeType::Char => {
                match op {
                    Operator::Subscript | Operator::Deref | Operator::ConstDeref | Operator::AsPtr => ImplementsHow::No,
                    _ => ImplementsHow::Native(vec![SkyeType::AnyInt, SkyeType::U8, SkyeType::I8])
                }
            }
        }
    }

    pub fn check_completeness(&self) -> bool {
        match self {
            SkyeType::Type(inner) => inner.check_completeness(),

            SkyeType::U8  | SkyeType::U16 | SkyeType::U32 | SkyeType::U64 | SkyeType::Usz |
            SkyeType::I8  | SkyeType::I16 | SkyeType::I32 | SkyeType::I64 | SkyeType::AnyInt |
            SkyeType::F32 | SkyeType::F64 | SkyeType::AnyFloat |
            SkyeType::Char | SkyeType::Void | SkyeType::Unknown(_) | 
            SkyeType::Pointer(..) | SkyeType::Function(..) | SkyeType::Enum(..) => true,

            SkyeType::Group(..) | SkyeType::Namespace(_) | SkyeType::Template(..) |
            SkyeType::Macro(..) => false,
            
            SkyeType::Struct(_, fields, _) => fields.is_some(),
            SkyeType::Union(_, fields) |
            SkyeType::Bitfield(_, fields) => fields.is_some()
        }
    }

    pub fn is_castable_to(&self, cast_to: &SkyeType) -> CastableHow {
        match self {
            SkyeType::Void | SkyeType::Type(_) | SkyeType::Group(..) | SkyeType::Function(..) |
            SkyeType::Struct(..) | SkyeType::Namespace(_) | SkyeType::Template(..) |
            SkyeType::Union(..) | SkyeType::Bitfield(..) | SkyeType::Macro(..) => CastableHow::No,
            SkyeType::Unknown(_) => CastableHow::Yes,

            SkyeType::U8 | SkyeType::U16 | SkyeType::U32 | SkyeType::U64 |
            SkyeType::I8 | SkyeType::I16 | SkyeType::I32 | SkyeType::I64 | 
            SkyeType::AnyInt | SkyeType::AnyFloat | SkyeType::F32 | SkyeType::F64 | 
            SkyeType::Char => {
                if matches!(
                    cast_to, 
                    SkyeType::F32 | 
                    SkyeType::F64 | 
                    SkyeType::AnyFloat | 
                    SkyeType::Char
                ) || ALL_INTS.contains(cast_to) {
                    CastableHow::Yes
                } else {
                    CastableHow::No
                }
            }
            SkyeType::Usz => {
                if matches!(
                    cast_to, 
                    SkyeType::F32 | 
                    SkyeType::F64 | 
                    SkyeType::AnyFloat | 
                    SkyeType::Char |
                    SkyeType::Pointer(..)
                ) || ALL_INTS.contains(cast_to) {
                    CastableHow::Yes
                } else {
                    CastableHow::No
                }
            }

            SkyeType::Pointer(_, self_is_const) => {
                if matches!(cast_to, SkyeType::Usz) {
                    CastableHow::Yes
                } else if let SkyeType::Pointer(_, cast_to_const) = cast_to {
                    if *cast_to_const || !*self_is_const {
                        CastableHow::Yes
                    } else {
                        CastableHow::ConstnessLoss
                    }
                } else {
                    CastableHow::No
                }
            }
            SkyeType::Enum(_, variants, _) => {
                if variants.is_none() && ALL_INTS.contains(cast_to) {
                    CastableHow::Yes
                } else {
                    CastableHow::No
                }
            }
        }
    }
}