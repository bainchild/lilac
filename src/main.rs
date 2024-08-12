// goal: make everything a number
// and if not a number, a function

use clap::Parser;
use clio::*;
use lang_c::ast::{
    BlockItem, Constant, Declaration, DeclarationSpecifier, Declarator, DeclaratorKind, Expression,
    ExternalDeclaration, FunctionDefinition, ParameterDeclaration, Statement, TS18661FloatType,
    TranslationUnit, TypeSpecifier,
};
use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, RefCell},
    ops::Deref,
    sync::Mutex,
};
#[derive(Parser)]
#[clap(name = "lilac")]
struct Args {
    #[clap(value_parser, default_value = "-")]
    input: Input,
    // #[clap(long, short, value_parser, default_value = "a.out")]
    // output: Output,
}

trait Instantiatable {}
trait Variable<T> {
    fn get(&self) -> T;
    fn set(&mut self, v: T);
}
#[derive(Clone, Debug)]
struct Array {}
impl Instantiatable for Array {}
#[derive(Clone, Debug)]
struct Enum {}
impl Instantiatable for Enum {}
#[derive(Clone, Debug)]
struct Struct {}
impl Instantiatable for Struct {}
#[derive(Clone, Debug)]
struct Function {
    arguments: Vec<(String, Type)>,
    return_type: Box<Type>,
}
// trait Operatable {
//     fn subscript(&self, k: &str) -> IValue; // a[b], deref then access (takes array + integer/enum variant, gives array subtype)
//     fn indirection(&self) -> IValue; // *a, deref (takes prvalue, gives ivalue)  deref void errors
//     fn address_of(&self) -> PRValue; // &a, ref (takes any, gives prvalue)
//     fn member(&self, k: &str) -> IValue; // a.b, access (takes ivalue+ivalue, gives any)
//     fn member_indirect(&self, k: &str) -> IValue; // a->b, deref then access (takes prvalue+ivalue, gives any)
//     fn member_pointer(&self, k: &str) -> IValue; // a.*b, deref member, then access (takes ivalue+prvalue, gives any)
//     fn member_indirect_pointer(&self, k: &str) -> IValue; // a->*b, deref both, then access (takes prvalue+prvalue, gives any)
// }
#[derive(Clone, Debug)]
enum Type {
    Void,
    Char(bool),
    Int(bool), // bool is signed/unsigned
    Short(bool),
    Long(bool),
    Float,
    Double,
    Bool,

    Alias((String, Box<Type>)),
    Array((String, Array)),
    Enum((String, Enum)),
    Struct((String, Struct)),
    Function((String, Function)),
}
// there HAS to be a better way...
macro_rules! get_type_name {
    ($var:expr) => {
        match $var {
            Type::Void => ("Void"),
            Type::Char(_) => ("Char"),
            Type::Int(_) => ("Int"),
            Type::Short(_) => ("Short"),
            Type::Long(_) => ("Long"),
            Type::Float => ("Float"),
            Type::Double => ("Double"),
            Type::Bool => ("Bool"),
            Type::Alias(t) => (t.0.as_str()),
            Type::Array(t) => (t.0.as_str()),
            Type::Enum(t) => (t.0.as_str()),
            Type::Struct(t) => (t.0.as_str()),
            Type::Function(t) => (t.0.as_str()),
            // _ => None,
        }
    };
}
#[derive(Clone)]
struct IValue<T: Instantiatable + Copy> {
    offset: Option<i32>,
    basetype: T,
    val: Cell<T>,
}
impl<T: Instantiatable + Copy> Variable<T> for IValue<T> {
    fn get(&self) -> T {
        return self.val.clone().into_inner();
    }

    fn set(&mut self, v: T) {
        self.val.set(v);
    }
}
struct PRValue<T: Instantiatable + Copy> {
    offset: i32,
    basetype: T,
    iv: IValue<T>,
}

struct State<'a> {
    output: &'a mut String,
    src: &'a Vec<BlockItem>,
    types: &'a mut Vec<Type>,
    variables: &'a mut Vec<&'a dyn Instantiatable>,
    strtab: &'a Mutex<Vec<(String, Type)>>,
    // variables: Vec<&'a dyn Instantiatable>,
    // parent: Option<Box<&'a State<'a>>>,
}
trait StateInto<T> {
    fn state_into(self, state: &State) -> T;
}
// tri-state area!!!!
trait TryStateInto<T> {
    type Error;
    fn try_state_into(self, state: &State) -> std::result::Result<T, Self::Error>;
}
impl<T, B> StateInto<Vec<B>> for Vec<T>
where
    T: StateInto<B> + Copy,
{
    fn state_into(self, state: &State) -> Vec<B> {
        self.iter().map(|x| x.state_into(state)).collect()
    }
}
impl<T, S> TryStateInto<T> for S
where
    S: StateInto<T>,
{
    type Error = ();
    fn try_state_into(self, state: &State) -> std::result::Result<T, Self::Error> {
        Ok(self.state_into(state))
    }
}
impl<T, B> StateInto<B> for lang_c::span::Node<T>
where
    T: StateInto<B>,
{
    fn state_into(self, state: &State) -> B {
        self.node.state_into(state)
    }
}

fn declaration_to_lua(decl: Declaration, state: &State) -> String {
    // println!("declaration specifiers {:?}", decl.specifiers);
    // println!("declaration declarators {:?}", decl.declarators);
    let binding = decl
        .specifiers
        .iter()
        .filter_map(|x| match x.node.clone() {
            DeclarationSpecifier::TypeSpecifier(ts2) => Some(ts2.node),
            _ => None,
        })
        .collect::<Vec<TypeSpecifier>>();
    let ts = binding
        .first()
        .expect("The declaration should have a type specifier.");
    let ty = ts.clone().try_state_into(state).unwrap();
    let binding2 = decl
        .declarators
        .iter()
        .filter_map(|x| match x.node.clone().declarator.node.kind.node {
            DeclaratorKind::Identifier(id) => Some(id.node.name),
            _ => None,
        })
        .collect::<Vec<String>>();
    let id = binding2
        .first()
        .expect("There should be an identifier with this declarator.");
    return "local ".to_string() + id.as_str() + type_into_comment(ty, false).as_str() + ";";
}
fn declarator_to_lua<'a>(declarator: Declarator, state: &State) -> String {
    let id = match declarator.kind.node {
        DeclaratorKind::Abstract => todo!(),
        DeclaratorKind::Identifier(id) => id.node.name,
        DeclaratorKind::Declarator(_) => todo!(),
    };
    let mut typedef = "nil".to_string();
    for decl in declarator.derived {
        match decl.node {
            lang_c::ast::DerivedDeclarator::Pointer(p) => typedef = "*".to_string() + id.as_str(),
            lang_c::ast::DerivedDeclarator::Function(f) => {
                typedef = "{type='function',parameters={".to_string()
                    + f.node
                        .parameters
                        .iter()
                        .map(|x| {
                            typetostring(
                                x.node
                                    .specifiers
                                    .iter()
                                    .filter_map(|y| match y.node.clone() {
                                        DeclarationSpecifier::TypeSpecifier(s) => Some(s.node),
                                        _ => None,
                                    })
                                    .collect::<Vec<TypeSpecifier>>()
                                    .first()
                                    .unwrap()
                                    .clone()
                                    .try_state_into(state)
                                    .unwrap(),
                            )
                        })
                        .map(|x| "'".to_string() + x.as_str() + "'")
                        .collect::<Vec<String>>()
                        .join(",")
                        .as_str()
                    + "},variadic="
                    + match f.node.ellipsis {
                        lang_c::ast::Ellipsis::Some => "true",
                        lang_c::ast::Ellipsis::None => "false",
                    }
                    + ",size=1}"
            }
            _ => {
                println!("unimpl {:?}", decl.node);
                todo!()
            }
        }
    }
    "_imports['".to_string() + id.as_str() + "'] = " + typedef.as_str() + ";"
}
fn constant_to_lua(constant: Constant) -> String {
    match constant {
        Constant::Integer(i) => {
            let prefix = match i.base {
                lang_c::ast::IntegerBase::Decimal => "",
                lang_c::ast::IntegerBase::Hexadecimal => "0x",
                lang_c::ast::IntegerBase::Binary => "0b",
                _ => todo!(),
            };
            (prefix.to_string() + i.number.borrow())
            // + "--[["
            // + (if i.suffix.unsigned { "U" } else { "" })
            // + {
            //     match i.suffix.size {
            //         lang_c::ast::IntegerSize::Int => "Int",
            //         lang_c::ast::IntegerSize::Long => "Long",
            //         lang_c::ast::IntegerSize::LongLong => "LongLong",
            //     }
            // }
            // + "]]")
        }
        Constant::Float(_) => todo!(),
        Constant::Character(_) => todo!(),
    }
}
fn expression_to_lua<'a>(expression: Expression, state: &'a State) -> String {
    match expression {
        Expression::Identifier(i) => i.node.name,
        Expression::Constant(cst) => constant_to_lua(cst.node),
        Expression::StringLiteral(l) => {
            let content = l.node.join("\n");
            let locked = state.strtab.lock().unwrap();
            let cloned = locked.clone();
            std::mem::drop(locked);
            let mut found = cloned
                .iter()
                .enumerate()
                .find(|x| x.1 .0 == content)
                .and_then(|x| Some(x.0));
            if found.is_none() {
                let mut strtab = state.strtab.lock().unwrap();
                // .expect("The strtab shouldn't be mutable borrowed at this point");
                let val = (
                    content,
                    Type::Array((
                        "stringliteral".to_string() + strtab.len().to_string().as_str(),
                        Array {},
                    )),
                );
                strtab.push(val);
                let len = strtab.len();
                std::mem::drop(strtab);
                found = Some(len)
            }
            "_strtab[".to_string() + found.unwrap().to_string().as_str() + "]"
        }
        Expression::GenericSelection(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::Call(c) => {
            (match c.node.callee.node {
                Expression::Identifier(i) => {
                    "_namespace['".to_string() + i.node.name.as_str() + "']"
                }
                _ => expression_to_lua(c.node.callee.node, state),
            }) + "("
                + (c.node
                    .arguments
                    .iter()
                    .map(|x| expression_to_lua(x.node.clone(), state))
                    .collect::<Vec<String>>()
                    .join(", ")
                    .as_str())
                + ")"
        }
        Expression::CompoundLiteral(_) => todo!(),
        Expression::SizeOfTy(_) => todo!(),
        Expression::SizeOfVal(_) => todo!(),
        Expression::AlignOf(_) => todo!(),
        Expression::UnaryOperator(_) => todo!(),
        Expression::Cast(_) => todo!(),
        Expression::BinaryOperator(b) => match b.node.clone().operator.node {
            lang_c::ast::BinaryOperator::Assign => {
                expression_to_lua(b.node.clone().lhs.node, state)
                    + " = "
                    + expression_to_lua(b.node.clone().rhs.node, state).as_str()
            }
            lang_c::ast::BinaryOperator::Index => {
                "_memory[".to_string()
                    + expression_to_lua(b.node.clone().lhs.node, state).as_str()
                    + "+("
                    + expression_to_lua(b.node.clone().rhs.node, state).as_str()
                    + "-1)*_pointersize]" // sizeof *char == sizeof pointer
            }
            _ => {
                "(".to_string()
                    + expression_to_lua(b.node.clone().lhs.node, state).as_str()
                    + " "
                    + match b.node.clone().operator.node {
                        lang_c::ast::BinaryOperator::LogicalAnd => "and",
                        lang_c::ast::BinaryOperator::LogicalOr => "or",
                        lang_c::ast::BinaryOperator::Less => "<",
                        lang_c::ast::BinaryOperator::Greater => ">",
                        lang_c::ast::BinaryOperator::LessOrEqual => "<=",
                        lang_c::ast::BinaryOperator::GreaterOrEqual => ">=",
                        lang_c::ast::BinaryOperator::Equals => "==",
                        _ => {
                            println!("unimpl {:?}", b.node.clone().operator.node);
                            todo!()
                        }
                    }
                    + " "
                    + expression_to_lua(b.node.clone().rhs.node, state).as_str()
                    + match b.node.clone().operator.node {
                        lang_c::ast::BinaryOperator::Index => "]",
                        _ => ")",
                    }
            }
        },
        Expression::Conditional(_) => todo!(),
        Expression::Comma(_) => todo!(),
        Expression::OffsetOf(_) => todo!(),
        Expression::VaArg(_) => todo!(),
        Expression::Statement(_) => todo!(),
    }
}
fn statement_to_lua<'a>(stmt: Statement, state: &'a State) -> String {
    match stmt {
        Statement::Labeled(_) => todo!(),
        Statement::Compound(vnbi) => {
            // println!("compound statement {:?}", vnbi);
            let mut st = State {
                output: &mut "".to_string(),
                src: &vnbi
                    .iter()
                    .map(|x| x.node.clone())
                    .collect::<Vec<BlockItem>>(),
                types: &mut state.types.clone(),
                variables: &mut Vec::new(),
                strtab: &state.strtab,
            };
            st.go(false).unwrap();
            for bl in st.src.iter() {
                match bl {
                    BlockItem::Declaration(dec) => st
                        .output
                        .push_str((declaration_to_lua(dec.node.clone(), &st) + "\n").as_str()),
                    BlockItem::StaticAssert(_) => todo!(),
                    BlockItem::Statement(s) => st.output.push_str(
                        (statement_to_lua(s.node.clone(), &st).to_string() + "\n").as_str(),
                    ),
                }
            }
            st.output.to_string()
        }
        Statement::Expression(exp) => {
            if exp.is_some() {
                expression_to_lua(exp.unwrap().node, state)
            } else {
                "".to_string()
            }
        }
        Statement::If(i) => {
            let mut out = "if ".to_string()
                + expression_to_lua(i.node.clone().condition.node, state).as_str()
                + " then"
                + ("\n".to_string()
                    + statement_to_lua(i.node.clone().then_statement.node, state)
                        .split_terminator("\n")
                        .map(|x| "   ".to_string() + x)
                        .collect::<Vec<String>>()
                        .join("\n")
                        .as_str())
                .as_str();
            if i.node.else_statement.is_some() {
                out = out
                    + "\nelse"
                    + ("\n".to_string()
                        + statement_to_lua(i.node.clone().else_statement.unwrap().node, state)
                            .split_terminator("\n")
                            .map(|x| "   ".to_string() + x)
                            .collect::<Vec<String>>()
                            .join("\n")
                            .as_str())
                    .as_str()
            }
            out + "\nend"
        }
        Statement::Switch(_) => todo!(),
        Statement::While(_) => todo!(),
        Statement::DoWhile(_) => todo!(),
        Statement::For(_) => todo!(),
        Statement::Goto(_) => todo!(),
        Statement::Continue => todo!(),
        Statement::Break => todo!(),
        Statement::Return(val) => {
            // println!("return statement {:?}", val);
            "return ".to_string()
                + val
                    .and_then(|x| Some(expression_to_lua(x.node, state)))
                    .unwrap_or("".to_string())
                    .as_str()
        }
        Statement::Asm(_) => todo!(),
    }
}
impl Into<Type> for TS18661FloatType {
    fn into(self) -> Type {
        // ummm
        match self.format {
            lang_c::ast::TS18661FloatFormat::BinaryInterchange => todo!(),
            lang_c::ast::TS18661FloatFormat::BinaryExtended => todo!(),
            lang_c::ast::TS18661FloatFormat::DecimalInterchange => todo!(),
            lang_c::ast::TS18661FloatFormat::DecimalExtended => todo!(),
        }
    }
}
impl TryStateInto<Type> for TypeSpecifier {
    type Error = ();
    fn try_state_into(self, state: &State) -> std::result::Result<Type, ()> {
        match self {
            TypeSpecifier::Void => Ok(Type::Void),
            TypeSpecifier::Char => Ok(Type::Char(false)),
            TypeSpecifier::Int => Ok(Type::Int(false)),
            TypeSpecifier::Short => Ok(Type::Short(false)),
            TypeSpecifier::Long => Ok(Type::Long(false)),
            TypeSpecifier::Float => Ok(Type::Float),
            TypeSpecifier::Double => Ok(Type::Double),
            TypeSpecifier::Signed => Ok(Type::Int(false)),
            TypeSpecifier::Unsigned => Ok(Type::Int(true)),
            TypeSpecifier::Bool => Ok(Type::Bool),
            TypeSpecifier::TS18661Float(a) => Ok(a.into()),
            TypeSpecifier::TypedefName(id) => {
                // println!("looking for alias source for {}", id.node.name);
                Ok(state
                    .types
                    .iter()
                    // .inspect(|x| {
                    //     println!("type name {:?} == {} ?", get_type_name!(x), id.node.name)
                    // })
                    .find(|x| get_type_name!(x) == id.node.name)
                    .expect("type alias source should be defined")
                    .to_owned()
                    .clone())
            }
            _ => {
                println!("failure to convert typespecifier to type: {:?}", self);
                Err(())
            }
        }
    }
}
impl StateInto<Type> for ParameterDeclaration {
    fn state_into(self, state: &State) -> Type {
        match self.specifiers.first().unwrap().node.clone() {
            DeclarationSpecifier::TypeSpecifier(t) => t.node.try_state_into(state).unwrap(),
            _ => todo!(),
        }
        // todo!()
    }
}
impl StateInto<Type> for FunctionDefinition {
    fn state_into(self, state: &State) -> Type {
        let ff = self
            .declarator
            .node
            .derived
            .iter()
            .find_map(|x| match x.node.clone() {
                lang_c::ast::DerivedDeclarator::Function(f) => Some(f),
                lang_c::ast::DerivedDeclarator::KRFunction(_) => todo!("k&r style functions"),
                _ => None,
            })
            .unwrap();
        Type::Function((
            {
                match self.declarator.node.kind.node {
                    DeclaratorKind::Identifier(i) => i.node.name,
                    _ => "".to_string(),
                }
            },
            Function {
                return_type: Box::new(
                    {
                        let mut temp: Option<Type> = None;
                        for t in self.specifiers.iter() {
                            match t.node.clone() {
                                DeclarationSpecifier::TypeSpecifier(ts) => match ts.node {
                                    TypeSpecifier::Complex => {}
                                    TypeSpecifier::Atomic(_) => {}
                                    TypeSpecifier::Struct(_) => {}
                                    TypeSpecifier::Enum(_) => {}
                                    TypeSpecifier::TypedefName(id) => {
                                        temp = Some(
                                            state
                                                .types
                                                .iter()
                                                .find(|x| get_type_name!(x) == id.node.name)
                                                .expect("type used to be defined")
                                                .to_owned()
                                                .clone(),
                                        );
                                    }
                                    TypeSpecifier::TypeOf(_) => {}
                                    _ => temp = Some(ts.node.try_state_into(state).unwrap()),
                                },
                                DeclarationSpecifier::TypeQualifier(_) => todo!(),
                                DeclarationSpecifier::StorageClass(_) => todo!(),
                                DeclarationSpecifier::Function(_) => todo!(),
                                DeclarationSpecifier::Alignment(_) => todo!(),
                                DeclarationSpecifier::Extension(_) => todo!(),
                            }
                        }
                        if temp.is_none() {
                            Err(())
                        } else {
                            Ok(temp.unwrap())
                        }
                    }
                    .unwrap(),
                ),
                arguments: {
                    ff.node
                        .parameters
                        .iter()
                        .map(|x| x.node.clone().state_into(state))
                        .zip(ff.node.parameters.iter().filter_map(|x| {
                            x.node
                                .declarator
                                .clone()
                                .and_then(|y| match y.node.kind.node {
                                    DeclaratorKind::Identifier(id) => Some(id.node.name),
                                    _ => None,
                                })
                                .or(Some("".to_string()))
                        }))
                        .map(|x| (x.1, x.0))
                        .collect::<Vec<(String, Type)>>()
                },
            },
        ))
    }
}
impl StateInto<Type> for Declaration {
    fn state_into(self, state: &State) -> Type {
        // println!("declarators {:?}", self.declarators);
        // println!("specifiers {:?}", self.specifiers);
        if self
            .specifiers
            .iter()
            .find(|x| match x.node.clone() {
                DeclarationSpecifier::StorageClass(s) => match s.node {
                    lang_c::ast::StorageClassSpecifier::Extern => true,
                    _ => false,
                },
                _ => false,
            })
            .is_some()
        {
            // extern
            // for ele in self.declarators.iter() {
            // }
            self.specifiers
                .iter()
                .filter_map(|x| match x.node.clone() {
                    DeclarationSpecifier::TypeSpecifier(ts) => Some(ts.node),
                    _ => None,
                })
                .take(1)
                .collect::<Vec<TypeSpecifier>>()
                .first()
                .unwrap()
                .clone()
                .try_state_into(state)
                .unwrap()
        } else {
            // local
            self.specifiers
                .iter()
                .filter_map(|x| match x.node.clone() {
                    DeclarationSpecifier::TypeSpecifier(ts) => Some(ts.node),
                    _ => None,
                })
                .take(1)
                .collect::<Vec<TypeSpecifier>>()
                .first()
                .unwrap()
                .clone()
                .try_state_into(state)
                .unwrap()
        }
    }
}
fn extdec_into_blockitem(ex: ExternalDeclaration) -> BlockItem {
    match ex {
        ExternalDeclaration::Declaration(d) => BlockItem::Declaration(d),
        ExternalDeclaration::StaticAssert(s) => BlockItem::StaticAssert(s),
        ExternalDeclaration::FunctionDefinition(f) => BlockItem::Statement(f.node.statement),
    }
}
impl TryStateInto<Type> for BlockItem {
    type Error = ();
    fn try_state_into(self, state: &State) -> std::result::Result<Type, ()> {
        match self {
            BlockItem::Declaration(d) => Ok(d.node.state_into(state)),
            _ => Err(()),
        }
    }
}
fn type_into_comment(x: Type, ret: bool) -> String {
    let snip = match x {
        Type::Void => "Void",
        Type::Char(unsigned) => {
            if unsigned {
                "UChar"
            } else {
                "Char"
            }
        }
        Type::Int(unsigned) => {
            if unsigned {
                "UInt"
            } else {
                "Int"
            }
        }
        Type::Short(unsigned) => {
            if unsigned {
                "UShort"
            } else {
                "Short"
            }
        }
        Type::Long(unsigned) => {
            if unsigned {
                "ULong"
            } else {
                "Long"
            }
        }
        Type::Float => "Float",
        Type::Double => "Double",
        Type::Bool => "Bool",
        _ => todo!(),
    };
    if (ret) {
        "--> ".to_string() + snip
    } else {
        "--[[".to_string() + snip + "]]"
    }
}
impl State<'_> {
    fn go(&mut self, root: bool) -> Result<()> {
        // language server protocol
        let it = self
            .src
            .iter()
            .filter_map(|x| x.clone().try_state_into(&self).ok())
            .map(|x| x.borrow().clone())
            .collect::<Vec<Type>>();
        self.types.extend(it);
        Ok(())
    }
}
fn typetostring(x: Type) -> String {
    match x {
        Type::Void => "void".to_string(),
        Type::Char(us) => if us { "unsigned char" } else { "char" }.to_string(),
        Type::Int(us) => if us { "unsigned int" } else { "int" }.to_string(),
        Type::Short(us) => if us {
            "unsigned short int"
        } else {
            "short int"
        }
        .to_string(),
        Type::Long(us) => if us { "unsigned long int" } else { "long int" }.to_string(),
        Type::Float => "float".to_string(),
        Type::Double => "double".to_string(),
        Type::Bool => "boolean".to_string(),
        _ => todo!(), // Type::Alias(_) => todo!(),
                      // Type::Array(_) => todo!(),
                      // Type::Enum(_) => todo!(),
                      // Type::Struct(_) => todo!(),
                      // Type::Function(_) => todo!(),
    }
}
fn tu_to_lua(tu: TranslationUnit) -> String {
    const BUILTINS: [(&str, Type); 9] = [
        ("__builtin_va_list", Type::Char(false)),
        ("__int8_t", Type::Char(true)),
        ("__uint8_t", Type::Char(false)),
        ("__int16_t", Type::Short(true)),
        ("__uint16_t", Type::Short(false)),
        ("__int32_t", Type::Int(true)),
        ("__uint32_t", Type::Int(false)),
        ("__int64_t", Type::Long(true)),
        ("__uint64_t", Type::Long(false)),
    ];
    let mut state = State {
        output: &mut "".to_string(),
        src: &mut tu
            .0
            .iter()
            .map(|x| extdec_into_blockitem(x.node.clone()))
            .collect::<Vec<BlockItem>>(),
        types: &mut Vec::new(),
        variables: &mut Vec::new(),
        strtab: &Mutex::new(Vec::new()), // parent: None,
    };
    for pair in BUILTINS.iter() {
        state
            .types
            .push(Type::Alias((pair.0.to_string(), Box::new(pair.1.clone()))))
    }
    state.go(true).unwrap();
    let ntl = state.types.len() - BUILTINS.len();
    let vln = state.variables.len();
    state.output.push_str(
        format!(
            "-- contains {} type{}, {} global variable{}\n",
            ntl,
            if ntl != 1 { "s" } else { "" },
            vln,
            if vln != 1 { "s" } else { "" }
        )
        .as_str(),
    );
    state
        .output
        .push_str("local _namespace,_exports,_imports,_memory,_strtab = {},{},{},{},nil;\n");
    state.output.push_str("-- _imports['sizeof'] = {type='function',parameters={'*char[]'},variadic=false,return_='size_t',size=1}\n");
    for extdecl in tu.0.iter() {
        match extdecl.node.clone() {
            ExternalDeclaration::Declaration(dec) => {
                for decl in dec.node.declarators {
                    state.output.push_str(
                        (declarator_to_lua(decl.node.declarator.node, &state) + "\n").as_str(),
                    );
                }
            }
            ExternalDeclaration::FunctionDefinition(f2) => {
                if let Type::Function((id, f)) = f2.node.clone().state_into(&state) {
                    state.output.push_str(
                        ("local function func_".to_string()
                            + id.as_str()
                            + "("
                            + f.arguments
                                .iter()
                                .map(|x| {
                                    (x.0.to_string()
                                        + type_into_comment(x.1.clone(), false).as_str())
                                })
                                .collect::<Vec<String>>()
                                .join(",")
                                .as_str()
                            + ") "
                            + type_into_comment(*f.return_type.clone(), true).as_str()
                            + ("\n".to_string()
                                + "local _pointersize=_namespace['sizeof']('*char');\n"
                                + statement_to_lua(f2.node.statement.node, &state).as_str())
                            .split_terminator("\n")
                            .map(|x| "   ".to_string() + x)
                            .collect::<Vec<String>>()
                            .join("\n")
                            .as_str()
                            + "\nend;\n"
                            + "_namespace['"
                            + id.as_str()
                            + "'] = func_"
                            + id.as_str()
                            + ";\n_exports['"
                            + id.as_str()
                            + "'] = {type='function',parameters={"
                            + f.arguments
                                .iter()
                                // .zip(f2.node.specifiers.iter())
                                .map(|x| "'".to_string() + typetostring(x.1.clone()).as_str() + "'")
                                .collect::<Vec<String>>()
                                .join(",")
                                .as_str()
                            + "},return_='"
                            + typetostring(*f.return_type).as_str()
                            + "',size=1};\n")
                            .as_str(),
                    )
                }
            }
            ExternalDeclaration::StaticAssert(_) => todo!(),
        }
    }
    state.output.push_str("_strtab = {\n");
    for str in state.strtab.lock().unwrap().iter() {
        state
            .output
            .push_str(("   ".to_string() + str.0.as_str() + ";\n").as_str())
    }
    state.output.push_str("};\n");
    state.output.push_str(
        "return {namespace=_namespace,exports=_exports,imports=_imports,memory=_memory,set_memory=function(mem)_memory=mem;end,strtab=_strtab};\n",
    );
    return state.output.to_string();
}
fn main() {
    let arg = Args::parse();
    let config = lang_c::driver::Config {
        cpp_command: "clang".to_string(),
        cpp_options: {
            let mut v = Vec::new();
            // v.push("-nostdlib".to_string());
            v.push("-E".to_string());
            // arg.define
            //     .iter()
            //     .map(|x| "-D".to_string() + &x)
            //     .for_each(|x| v.push(x));
            v
        },
        flavor: lang_c::driver::Flavor::ClangC11,
    };
    let ast = lang_c::driver::parse(&config, arg.input.path().path()).unwrap();
    let mut outp = tu_to_lua(ast.unit).clone();
    outp = format!(
        "{}",
        "-- file: ".to_string() + arg.input.path().to_string().as_str()
    )
    .to_string()
        + "\n"
        + outp.as_str();
    println!("{}", outp);
}
