use std::{
    backtrace,
    ffi::{OsStr, OsString},
    fs::File,
    io::Write,
    ops::Deref,
    os::fd::AsFd,
    process::Command,
    sync::Arc,
    thread::spawn,
    time::SystemTime,
};

use lang_c::{
    ast::{
        self, ArraySize, BinaryOperator, BlockItem, CastExpression, CompoundLiteral, Constant,
        DeclarationSpecifier, DeclaratorKind, DerivedDeclarator, Ellipsis, Expression, FloatBase,
        ForInitializer, FunctionSpecifier, GenericSelection, Identifier, Initializer, IntegerBase,
        Label, MemberOperator, PointerQualifier, SpecifierQualifier, Statement,
        StorageClassSpecifier, StructDeclaration, StructKind, StructType, TypeOf, TypeQualifier,
        TypeSpecifier,
    },
    driver::{parse, Config},
    span::Node,
};
// TODO: where are the enums???
// todo: the clones are numerable
fn indent(str: String, id: usize) -> String {
    let indentation = " ".repeat(id);
    let iter = str.split("\n");
    iter.map(|a| indentation.clone() + a + "\n")
        .collect::<Vec<String>>()
        .concat()
}
trait IntoLua {
    fn into_lua(&self) -> String;
}
impl IntoLua for String {
    fn into_lua(&self) -> String {
        self.to_string()
        // "\"".to_owned() + self + "\""
    }
}
impl<T: IntoLua> IntoLua for Node<T> {
    fn into_lua(&self) -> String {
        self.node.into_lua()
    }
}
// impl<T: IntoLua> IntoLua for Vec<T> {
//     fn into_lua(&self) -> String {
//         let mut str = String::new();
//         for nto in self.iter() {
//             str.push_str(nto.into_lua().as_str());
//         }
//         str
//     }
// }
fn get_practical_type(s: Node<TypeSpecifier>) -> String {
    // if s.into_lua().trim().is_empty() {
    //     return "AAAAAAAAAAAAAAAAAAAAAA".to_string();
    // }
    let vartype: String;
    // this feels stupid. like... there should be a better solution to this
    // but I haven't found it yet
    match s.node.clone() {
        TypeSpecifier::Void => vartype = "Void".to_owned(),
        TypeSpecifier::Char => vartype = "Char".to_owned(),
        TypeSpecifier::Short => vartype = "integer".to_owned(), //--[[short]]".to_owned(),
        TypeSpecifier::Int => vartype = "integer".to_owned(),
        TypeSpecifier::Long => vartype = "number".to_owned(), //--[[long]]".to_owned(),
        TypeSpecifier::Float => vartype = "number".to_owned(), //--[[float]]".to_owned(),
        TypeSpecifier::Double => vartype = "number".to_owned(), //--[[double]]".to_owned(),
        TypeSpecifier::Signed => vartype = "number".to_owned(), //--[[signed]]".to_owned(),
        TypeSpecifier::Unsigned => vartype = "integer".to_owned(), //--[[unsigned]]".to_owned(),
        TypeSpecifier::Bool => vartype = "boolean".to_owned(),
        TypeSpecifier::Complex => vartype = "number".to_owned(), //--[[complex]]".to_owned(),
        TypeSpecifier::TypedefName(i) => vartype = i.into_lua(),
        TypeSpecifier::Struct(st) => {
            vartype = {
                if st.node.declarations.is_some() {
                    return match st.node.kind.node {
                        StructKind::Union => {
                            let mut s = "".to_owned();
                            if st.node.identifier.is_some() {
                                s = s + "--[[";
                                s = s + &st.node.identifier.unwrap().into_lua();
                                s = s + "]]";
                            }
                            st.node
                                .declarations
                                .unwrap()
                                .iter()
                                .map(|x| x.into_lua())
                                .collect::<Vec<String>>()
                                .join("|");
                            s
                        }
                        StructKind::Struct => {
                            let mut s = "".to_owned();
                            if st.node.identifier.is_some() {
                                s = s + "--[[";
                                s = s + &st.node.identifier.unwrap().into_lua();
                                s = s + "]]";
                            }
                            s = s + "{";
                            st.node
                                .declarations
                                .unwrap()
                                .iter()
                                .map(|x| x.into_lua())
                                .collect::<Vec<String>>()
                                .join(",");
                            s = s + "}";
                            s
                        }
                    };
                }
                "unknown--[[empty struct]]".to_owned()
            }
        }
        .to_owned(),
        _ => vartype = "unknown".to_owned(),
    };
    vartype
}
impl IntoLua for ast::BlockItem {
    fn into_lua(&self) -> String {
        match self {
            BlockItem::Declaration(b) => b.into_lua(),
            BlockItem::StaticAssert(b) => b.into_lua(),
            BlockItem::Statement(b) => b.into_lua(),
        }
    }
}
impl IntoLua for ast::CallExpression {
    fn into_lua(&self) -> String {
        "_D.".to_string()
            + &self.callee.into_lua()
            + "("
            + self
                .arguments
                .iter()
                .map(|a| a.into_lua())
                .collect::<Vec<String>>()
                .join(", ")
                .as_str()
            + ")"
    }
}
impl IntoLua for ast::Constant {
    fn into_lua(&self) -> String {
        "____C.Cst(".to_owned()
            + &(match self {
                Constant::Integer(i) => match i.base {
                    IntegerBase::Decimal => i.number.to_string(),
                    IntegerBase::Binary => {
                        "tonumber(\"".to_owned() + i.number.clone().into_string().as_str() + "\",2)"
                    }
                    IntegerBase::Octal => {
                        "tonumber(\"".to_owned() + i.number.clone().into_string().as_str() + "\",8)"
                    }
                    IntegerBase::Hexadecimal => {
                        "0x".to_owned() + i.number.clone().into_string().as_str()
                    }
                },
                Constant::Float(f) => match f.base {
                    FloatBase::Decimal => f.number.to_string(),
                    FloatBase::Hexadecimal => {
                        "0x".to_owned() + f.number.clone().into_string().as_str()
                    }
                },
                Constant::Character(c) => c.into(),
            })
            + ")"
    }
}
impl IntoLua for ast::MemberOperator {
    fn into_lua(&self) -> String {
        match self {
            MemberOperator::Direct => ".",
            MemberOperator::Indirect => ".--[[->]]",
        }
        .to_string()
    }
}
impl IntoLua for ast::MemberExpression {
    fn into_lua(&self) -> String {
        if let MemberOperator::Indirect = self.operator.node {
            "____C.Deref(".to_owned()
                + &self.expression.into_lua()
                + ")."
                + &self.identifier.into_lua()
        } else {
            self.expression.into_lua() + &self.operator.into_lua() + &self.identifier.into_lua()
        }
        //"--[[member expression]]".to_string()
    }
}
impl IntoLua for ast::GenericSelection {
    fn into_lua(&self) -> String {
        "--[[generic selection]]".to_string()
    }
}
impl IntoLua for ast::CompoundLiteral {
    fn into_lua(&self) -> String {
        "--[[compound literal]]".to_string()
    }
}
impl IntoLua for ast::SizeOfTy {
    fn into_lua(&self) -> String {
        "____C.SizeOfType('".to_string() + &self.0.into_lua() + "')"
    }
}
impl IntoLua for ast::SizeOfVal {
    fn into_lua(&self) -> String {
        "____C.SizeOfValue(".to_string() + &self.0.into_lua() + ")"
    }
}
// impl IntoLua for ast::UnaryOperator {
//     fn into_lua(&self) -> String {
//     }
// }
impl IntoLua for ast::UnaryOperatorExpression {
    fn into_lua(&self) -> String {
        let operand = self.operand.into_lua();
        match self.operator.node {
            ast::UnaryOperator::PostIncrement => {
                "(function()local _=".to_owned()
                    + &operand
                    + ";____C.Set("
                    + &operand
                    + ","
                    + &operand
                    + " + 1);return _ end)()"
            }
            ast::UnaryOperator::PostDecrement => {
                "(function()local _=".to_owned()
                    + &operand
                    + ";____C.Set("
                    + &operand
                    + ","
                    + &operand
                    + " - 1);return _ end)()"
            }
            ast::UnaryOperator::PreIncrement => {
                "(function()____C.Set(".to_owned()
                    + &operand
                    + ","
                    + &operand
                    + " + 1);return "
                    + &operand
                    + ";end)()"
            }
            ast::UnaryOperator::PreDecrement => {
                "(function()____C.Set(".to_owned()
                    + &operand
                    + ","
                    + &operand
                    + " - 1);return "
                    + &operand
                    + ";end)()"
            }
            ast::UnaryOperator::Address => "____C.AddressOf(".to_owned() + &operand + ")",
            ast::UnaryOperator::Indirection => "____C.Ptr(".to_owned() + &operand + ")",
            ast::UnaryOperator::Plus => operand,
            ast::UnaryOperator::Minus => "-".to_owned() + &operand,
            ast::UnaryOperator::Complement => "(~ ".to_owned() + &operand + ")",
            ast::UnaryOperator::Negate => "(not ".to_owned() + &operand + ")",
        }
    }
}
impl IntoLua for ast::BinaryOperator {
    fn into_lua(&self) -> String {
        // TODO
        match self {
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::GreaterOrEqual => ">=",
            BinaryOperator::Equals => "==",
            BinaryOperator::NotEquals => "~=",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "~",
            BinaryOperator::LogicalAnd => "and",
            BinaryOperator::LogicalOr => "or",
            _ => "%--[[unimpl. binop ]]",
        }
        .to_string()
    }
}
impl IntoLua for ast::BinaryOperatorExpression {
    fn into_lua(&self) -> String {
        match self.operator.node {
            BinaryOperator::Index => self.lhs.into_lua() + "[" + &self.rhs.into_lua() + "]",
            // I think (think) I can change these to not be so space consuming
            BinaryOperator::Assign => {
                "____C.Set(".to_owned() + &self.lhs.into_lua() + "," + &self.rhs.into_lua() + ")"
            }
            BinaryOperator::AssignMultiply => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " * "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignDivide => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " / "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignModulo => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " % "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignPlus => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " + "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignMinus => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " - "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignShiftLeft => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " << "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignShiftRight => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " >> "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignBitwiseAnd => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " & "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignBitwiseOr => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " | "
                    + &self.rhs.into_lua()
                    + ")"
            }
            BinaryOperator::AssignBitwiseXor => {
                "____C.Set(".to_owned()
                    + &self.lhs.into_lua()
                    + ","
                    + &self.lhs.into_lua()
                    + " ~ "
                    + &self.rhs.into_lua()
                    + ")"
            }

            // BinaryOperator::Assign => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignMultiply => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " * "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignDivide => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " / "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignModulo => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " % "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignPlus => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " + "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignMinus => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " - "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignShiftLeft => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " << "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignShiftRight => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " >> "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignBitwiseAnd => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " & "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignBitwiseOr => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " | "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            // BinaryOperator::AssignBitwiseXor => {
            //     "(function() ".to_owned()
            //         + &self.lhs.into_lua()
            //         + "="
            //         + &self.lhs.into_lua()
            //         + " ~ "
            //         + &self.rhs.into_lua()
            //         + " end)()"
            // }
            _ => {
                // "(".to_string()
                /*  + &*/
                self.lhs.into_lua()
                    + " " // )
                    + &self.operator.into_lua()
                    + " " // (
                    + &self.rhs.into_lua()
                //  + ")"
            }
        }
    }
}
impl IntoLua for SpecifierQualifier {
    fn into_lua(&self) -> String {
        match self {
            SpecifierQualifier::TypeSpecifier(t) => get_practical_type(t.clone()),
            SpecifierQualifier::TypeQualifier(t) => t.into_lua(),
            // SpecifierQualifier::Extension(_) => todo!(),
            _ => "--[[umm]]".to_string(),
        }
    }
}
impl IntoLua for ast::TypeName {
    fn into_lua(&self) -> String {
        let mut str = "".to_string();
        // for spec in self.specifiers.iter().take(self.specifiers.len() - 1) {
        //     // println!("{:?}", spec);
        //     if let SpecifierQualifier::TypeSpecifier(t) = &spec.node {
        //         str.push_str("--[[");
        //         let luad = get_practical_type(t.clone());
        //         if self.declarator.is_some() {
        //             str.push_str(&transform_type(
        //                 self.declarator.clone().unwrap().node.derived,
        //                 luad.clone(),
        //             ));
        //         } else {
        //             str.push_str(&luad);
        //         }
        //         str.push_str("]]");
        //     }
        // }
        if self.specifiers.last().is_some() {
            let spec = self.specifiers.last().unwrap();
            if let SpecifierQualifier::TypeSpecifier(t) = &spec.node {
                let luad = get_practical_type(t.clone());
                if self.declarator.is_some() {
                    str.push_str(&transform_type(
                        self.declarator.clone().unwrap().node.derived,
                        luad.clone(),
                    ));
                } else {
                    str.push_str(&luad);
                }
            }
        }
        str
    }
}
impl IntoLua for ast::CastExpression {
    fn into_lua(&self) -> String {
        "(".to_owned() + &self.expression.into_lua() + " as " + &self.type_name.into_lua() + ")"
    }
}
impl IntoLua for ast::ConditionalExpression {
    fn into_lua(&self) -> String {
        "(function()if (".to_owned()
            + &self.condition.into_lua()
            + ") then return ("
            + &self.then_expression.into_lua()
            + "); else return ("
            + &self.else_expression.into_lua()
            + "); end end)()"
    }
}
impl IntoLua for ast::Expression {
    fn into_lua(&self) -> String {
        match self {
            // todo
            Expression::Identifier(b) => b.into_lua(),
            Expression::Constant(b) => b.into_lua(),
            Expression::StringLiteral(b) => {
                "____C.Str(".to_owned() + &b.node.join("..").into_lua() + ")"
            }
            Expression::GenericSelection(s) => s.into_lua(),
            Expression::Member(m) => m.into_lua(),
            Expression::Call(b) => b.into_lua(),
            Expression::CompoundLiteral(c) => c.into_lua(),
            Expression::SizeOfTy(t) => t.into_lua(),
            Expression::SizeOfVal(s) => s.into_lua(),
            Expression::Comma(comm) => {
                let mut s = "(function()".to_string();
                for v in comm.iter().take(comm.len() - 1) {
                    s = s + &v.into_lua() + ";"
                }
                s = s + "return " + &comm.last().unwrap().into_lua();
                s = s + " end)()";
                s
            } // "--[[ comma ]]".to_string(),
            Expression::UnaryOperator(s) => s.into_lua(),
            Expression::Statement(s) => s.into_lua(),
            Expression::Cast(s) => {
                "(function()local _=".to_owned() + &s.into_lua() + ";return _;end)()"
            }
            Expression::BinaryOperator(s) => s.into_lua(),
            Expression::AlignOf(_) => "--[[ align of ]]".to_string(),
            Expression::Conditional(c) => c.into_lua(),
            Expression::OffsetOf(_) => "--[[ offset of ]]".to_string(),
            Expression::VaArg(_) => "--[[ va arg ]]".to_string(), // _ => "(1)--[[EXPRESSION PLACEHOLDER]]".to_string(),
        }
    }
}
impl IntoLua for ast::CaseRange {
    fn into_lua(&self) -> String {
        // tod, who?
        "--[[CASE RANGE]]".to_string()
    }
}
impl IntoLua for ast::Label {
    fn into_lua(&self) -> String {
        match self {
            Label::Identifier(i) => i.into_lua(),
            Label::Case(ex) => ex.into_lua(),
            Label::CaseRange(ex) => ex.into_lua(),
            Label::Default => "default".to_string(),
        }
    }
}
impl IntoLua for ast::LabeledStatement {
    fn into_lua(&self) -> String {
        "::".to_owned() + &self.label.into_lua() + "::\n" + &self.statement.into_lua()
    }
}
impl IntoLua for ast::IfStatement {
    fn into_lua(&self) -> String {
        let mut s = "if (".to_owned() + &self.condition.into_lua() + ") then\n";
        s.push_str(&indent(self.then_statement.into_lua(), 3));
        if self.else_statement.is_some() {
            s.push_str("else\n");
            s.push_str(&indent(self.else_statement.clone().unwrap().into_lua(), 3));
        }
        s.push_str("end");
        s.to_string()
    }
}
impl IntoLua for ast::WhileStatement {
    fn into_lua(&self) -> String {
        "while (".to_owned()
            + &self.expression.into_lua()
            + ") do\n"
            + &indent(self.statement.into_lua() + "\n::continue::", 3)
            + "\nend"
    }
}
impl IntoLua for ast::DoWhileStatement {
    fn into_lua(&self) -> String {
        "repeat\n".to_owned()
            + &indent(self.statement.into_lua() + "\n::continue::", 3)
            + "\nuntil not ("
            + &self.expression.into_lua()
            + ")"
    }
}
impl IntoLua for ForInitializer {
    fn into_lua(&self) -> String {
        match self {
            ForInitializer::Empty => "".to_string(),
            ForInitializer::Expression(i) => i.into_lua(),
            ForInitializer::Declaration(i) => i.into_lua(),
            ForInitializer::StaticAssert(i) => i.into_lua(),
        }
    }
}
impl IntoLua for ast::ForStatement {
    fn into_lua(&self) -> String {
        let mut str = "-- start for loop\n".to_string();
        str.push_str(&(self.initializer.into_lua() + "\n"));
        str.push_str("while ");
        if self.condition.is_some() {
            str.push_str(&("(".to_owned() + &self.condition.clone().unwrap().into_lua() + ")"));
        } else {
            str.push_str("true");
        }
        str.push_str(" do\n");
        str.push_str(&(indent(self.statement.into_lua(), 3) + "\n"));
        if self.step.is_some() {
            str.push_str(
                &(indent(self.step.clone().unwrap().into_lua() + "\n::continue::", 3) + "\n"),
            );
        }
        str.push_str("end");
        str
    }
}
fn semicolonize(str: String) -> String {
    if str.trim_end().ends_with(";") | str.trim().is_empty() {
        str
    } else {
        // println!("{} -> {}", str, str.trim().is_empty());
        str + ";"
    }
}
impl IntoLua for ast::Statement {
    fn into_lua(&self) -> String {
        semicolonize(match self {
            Statement::Labeled(t) => t.into_lua(),
            Statement::Compound(t) => t
                .iter()
                .map(|b| b.into_lua())
                .collect::<Vec<String>>()
                .join("\n"),
            Statement::Expression(t) => {
                if t.is_some() {
                    t.as_ref().unwrap().into_lua()
                } else {
                    "".to_string()
                }
            }
            Statement::If(t) => t.into_lua(),
            // Statement::Switch(t) => t.into_lua(),
            Statement::While(t) => t.into_lua(),
            Statement::DoWhile(t) => t.into_lua(),
            Statement::For(t) => t.into_lua(),
            Statement::Goto(t) => "goto ".to_owned() + &t.into_lua(),
            // Statement::Asm(t) => t.into_lua(),
            Statement::Return(t) => {
                if t.is_some() {
                    "do return (".to_owned() + t.as_ref().unwrap().into_lua().as_str() + ") end"
                } else {
                    "do return end".to_string()
                }
            }
            Statement::Continue => "goto continue".to_string(),
            Statement::Break => "break".to_string(),
            _ => "--[[ unhandled statement ]]".to_string(),
            //_ => format!("--[[{:?}]]", self),
        }) + "\n"
    }
}
impl IntoLua for ast::Identifier {
    fn into_lua(&self) -> String {
        let keywords = [
            "while", "for", "do", "if", "repeat", "until", "end", "break", "then", "else", "elseif",
        ];
        let mut s = self.name.clone();
        if keywords.contains(&s.as_str()) {
            s = "_______________".to_owned().to_owned() + &s // I mean, it'll work...
        };
        s
    }
}
impl IntoLua for ast::StructKind {
    fn into_lua(&self) -> String {
        match self {
            StructKind::Struct => "--[[struct]]".to_string(),
            StructKind::Union => "--[[union]]".to_string(),
        }
    }
}
impl IntoLua for ast::StructType {
    fn into_lua(&self) -> String {
        if self.identifier.is_some() {
            self.kind.into_lua() + &self.identifier.clone().unwrap().into_lua()
        } else {
            self.kind.into_lua()
        }
        // declarations don't matter!!!!
    }
}
impl IntoLua for ast::Enumerator {
    fn into_lua(&self) -> String {
        // TODO: the expressions that come with this
        self.identifier.into_lua()
    }
}
impl IntoLua for ast::EnumType {
    fn into_lua(&self) -> String {
        if self.identifier.is_some() {
            let mut number = 0;
            "local enum ".to_owned()
                + &self.identifier.clone().unwrap().into_lua()
                + "\n"
                + &indent(
                    self.enumerators
                        .iter()
                        .map(|x| "\"".to_string() + &x.into_lua() + "\"")
                        .collect::<Vec<String>>()
                        .join("\n"),
                    3,
                )
                + "end\n"
                // + "local "
                + &self
                    .enumerators
                    .iter()
                    .map(|x| x.into_lua())
                    .collect::<Vec<String>>()
                    .join(",")
                + " = "
                + &self
                    .enumerators
                    .iter()
                    .map(|x| {
                        if x.node.expression.is_some() {
                            x.node.expression.clone().unwrap().into_lua()
                        } else {
                            number = number + 1;
                            (number - 1).to_string()
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
        } else {
            "--[[one enum, no identifier, please]]".to_string()
        }
    }
}
impl IntoLua for ast::TypeSpecifier {
    fn into_lua(&self) -> String {
        match self {
            TypeSpecifier::Void => "void".to_string(),
            TypeSpecifier::Char => "char".to_string(),
            TypeSpecifier::Short => "short".to_string(),
            TypeSpecifier::Int => "int".to_string(),
            TypeSpecifier::Long => "long".to_string(),
            TypeSpecifier::Float => "float".to_string(),
            TypeSpecifier::Double => "double".to_string(),
            TypeSpecifier::Signed => "signed".to_string(),
            TypeSpecifier::Unsigned => "unsigned".to_string(),
            TypeSpecifier::Bool => "bool".to_string(),
            TypeSpecifier::Complex => "complex".to_string(),
            TypeSpecifier::Struct(d) => d.into_lua(),
            TypeSpecifier::TypedefName(i) => i.into_lua(),
            TypeSpecifier::Enum(t) => t.into_lua(),
            _ => "nil--[[unimpl. type spec]]".to_string(),
        }
    }
}
impl IntoLua for ast::Attribute {
    fn into_lua(&self) -> String {
        self.name.node.to_owned()
            + "("
            + &self
                .arguments
                .iter()
                .map(|x| x.into_lua())
                .collect::<Vec<String>>()
                .join(", ")
            + ")"
    }
}
impl IntoLua for ast::AvailabilityVersion {
    fn into_lua(&self) -> String {
        let mut s = "v".to_owned() + &self.major.clone();
        if self.minor.is_some() {
            s = s + &self.minor.clone().unwrap()
        }
        if self.subminor.is_some() {
            s = s + &self.subminor.clone().unwrap()
        }
        s
    }
}
impl IntoLua for ast::AvailabilityClause {
    fn into_lua(&self) -> String {
        match self {
            ast::AvailabilityClause::Introduced(ver) => "I".to_owned() + &ver.into_lua(),
            ast::AvailabilityClause::Deprecated(ver) => "D".to_owned() + &ver.into_lua(),
            ast::AvailabilityClause::Obsoleted(ver) => "O".to_owned() + &ver.into_lua(),
            ast::AvailabilityClause::Unavailable => "U".to_string(),
            ast::AvailabilityClause::Message(ve) => "M{".to_owned() + &ve.node.join(",") + "}",
            ast::AvailabilityClause::Replacement(ve) => "R{".to_owned() + &ve.node.join(",") + "}",
        }
    }
}
impl IntoLua for ast::AvailabilityAttribute {
    fn into_lua(&self) -> String {
        self.platform.into_lua()
            + ": "
            + &self
                .clauses
                .iter()
                .map(|x| x.into_lua())
                .collect::<Vec<String>>()
                .join(",")
    }
}
impl IntoLua for ast::Extension {
    fn into_lua(&self) -> String {
        match self {
            ast::Extension::Attribute(attr) => "Attribute(".to_owned() + &attr.into_lua() + ")",
            ast::Extension::AsmLabel(sv) => {
                "AsmLabel(".to_owned()
                    + &sv
                        .node
                        .iter()
                        .map(|x| x.into_lua())
                        .collect::<Vec<String>>()
                        .join(",")
                    + ")"
            }
            ast::Extension::AvailabilityAttribute(av) => {
                "Availability(".to_owned() + &av.into_lua() + ")"
            }
        }
    }
}
impl IntoLua for ast::StructDeclaration {
    fn into_lua(&self) -> String {
        match self {
            StructDeclaration::Field(sf) => "--[[field]]".to_string(),
            StructDeclaration::StaticAssert(aa) => "--[[assert]]".to_string(),
        }
    }
}
// impl IntoLua for ast::DeclarationSpecifier {
//     fn into_lua(&self) -> String {
//     }
// }
impl IntoLua for ast::TypeQualifier {
    fn into_lua(&self) -> String {
        match self {
            TypeQualifier::Const => "const",
            TypeQualifier::Restrict => "restrict",
            TypeQualifier::Volatile => "volatile",
            TypeQualifier::Nonnull => "nonnull",
            TypeQualifier::NullUnspecified => "nullunspec",
            TypeQualifier::Nullable => "nullable",
            TypeQualifier::Atomic => "atomic",
        }
        .to_string()
    }
}
fn transform_type<T: IntoLua>(deriv: Vec<Node<DerivedDeclarator>>, var: T) -> String {
    let mut s = var.into_lua();
    let mut to_apply = Vec::new();
    for v in deriv.iter() {
        if let DerivedDeclarator::Array(_) = v.node {
            to_apply.push(v);
        }
    }
    for v in deriv.iter() {
        if let DerivedDeclarator::Pointer(_) = v.node {
            to_apply.push(v);
        }
    }
    for v in to_apply.iter() {
        match &v.node {
            DerivedDeclarator::Array(dec) => match dec.node.clone().size {
                ArraySize::Unknown => s = ("{".to_owned() + &s + "}"),
                ArraySize::VariableUnknown => s = ("{".to_owned() + &s + "}--[[variable unknown]]"),
                ArraySize::VariableExpression(e) => s = ("{".to_owned() + &s + "}"),
                ArraySize::StaticExpression(e) => s = ("{".to_owned() + &s + "}"),
            },
            DerivedDeclarator::Pointer(pq) => {
                let mut ptr_names: Vec<&str> = Vec::with_capacity(7); // 7 type qualifiers
                for _ in 0..7 {
                    ptr_names.push("");
                }
                for p in pq.iter() {
                    if let PointerQualifier::TypeQualifier(tq) = &p.node {
                        match tq.node {
                            TypeQualifier::Const => ptr_names[6] = "C",
                            TypeQualifier::Restrict => {
                                ptr_names[5] = "R";
                            }
                            TypeQualifier::Volatile => {
                                ptr_names[4] = "V";
                            }
                            TypeQualifier::Nonnull => {
                                ptr_names[3] = "N";
                            }
                            TypeQualifier::NullUnspecified => {
                                ptr_names[2] = "U";
                            }
                            TypeQualifier::Nullable => {
                                ptr_names[1] = "B";
                            }
                            TypeQualifier::Atomic => {
                                ptr_names[0] = "A";
                            }
                            _ => {}
                        }
                    }
                }
                ptr_names = ptr_names
                    .iter()
                    .cloned()
                    .filter(|x| !x.is_empty())
                    .collect::<Vec<&str>>();
                if ptr_names.len() > 0 {
                    s = ptr_names.concat() + "_Ptr<" + &s + ">";
                } else {
                    s = "Ptr<".to_owned() + &s + ">";
                }
            }
            _ => {}
        }
    }
    s
}
impl IntoLua for ast::ParameterDeclaration {
    fn into_lua(&self) -> String {
        let mut s = "".to_string();
        if self.declarator.is_some() && !self.declarator.clone().unwrap().into_lua().is_empty() {
            // declarator has the derived stuff
            // specifier has the type name
            // current bug: ends up with "a: <const>,: realType<aaa>"
            s = self.declarator.clone().unwrap().into_lua();
        }
        let mut iter = self
            .specifiers
            .iter()
            .filter(|x| !x.into_lua().trim().is_empty());
        let mut first = true;
        // let mut temp = "".to_string();
        'outer: loop {
            if first {
                first = false;
            } else {
                break;
                // s.push_str(", ");
            }
            let mut v;
            {
                let a = iter.next();
                if a.is_none() {
                    break;
                }
                v = a.unwrap();
                if v.into_lua().is_empty() | v.into_lua().trim().is_empty() {
                    continue;
                }
            }
            // guaranteed to not be a type qualifier
            // s.push_str("--[[");
            // s.push_str(&format!("{:?}", v));
            // s.push_str("]]");
            if let DeclarationSpecifier::TypeQualifier(t) = &v.node {
                // println!("{:?}", t);
                // s.push_str(&("--[[".to_owned() + &t.into_lua() + "]]"));
                loop {
                    {
                        let a = iter.next();
                        if a.is_none() {
                            break 'outer;
                        }
                        v = a.unwrap();
                        if v.into_lua().is_empty() | v.into_lua().trim().is_empty() {
                            continue;
                        }
                    }
                    if let DeclarationSpecifier::TypeQualifier(t) = &v.node {
                        // s.push_str(&t.into_lua());
                    } else {
                        break;
                    }
                }
            }
            if !v.node.into_lua().trim().is_empty() & self.declarator.is_some() {
                s.push_str(": ");
                s.push_str(&transform_type(
                    self.declarator.clone().unwrap().node.derived,
                    v.node.clone(),
                ));
            }
        }
        // s.truncate(s.len() - 2);
        s
        // self.specifiers
        //     .iter()
        //     .map(|a| {
        //         ": ".to_owned()
        //             + &transform_type(
        //                 self.declarator.clone().unwrap().node.derived,
        //                 a.node.clone(),
        //             )
        //     })
        //     .collect::<Vec<String>>()
        //     .join(",")
        //     .as_str()
        // } else {
        //     self.specifiers
        //         .iter()
        //         .map(|a| a.into_lua())
        //         .collect::<Vec<String>>()
        //         .join(",")
        // }
    }
}
impl IntoLua for ast::FunctionDeclarator {
    fn into_lua(&self) -> String {
        self.parameters
            .clone()
            .iter()
            // .filter(|x| !x.node.declarator.is_none())
            .map(|a| a.into_lua())
            .filter(|x| !x.trim().is_empty())
            .collect::<Vec<String>>()
            .join(", ")
            + {
                if self.ellipsis == Ellipsis::Some {
                    ", ..."
                } else {
                    ""
                }
            }
        // "--[[(function declarator)]]".to_string()
    }
}
impl IntoLua for ast::DeclaratorKind {
    fn into_lua(&self) -> String {
        match self {
            DeclaratorKind::Abstract => "ABSTRACT".to_string(),
            DeclaratorKind::Identifier(b) => b.into_lua(),
            DeclaratorKind::Declarator(b) => b.into_lua(),
        }
    }
}
impl IntoLua for ast::Declarator {
    fn into_lua(&self) -> String {
        // println!("{:?}\n", self);
        // let mut s: String = "Aaa".to_string();
        // s
        self.kind.into_lua()
    }
}
impl IntoLua for ast::DerivedDeclarator {
    fn into_lua(&self) -> String {
        // todo!("extremely todo")
        // println!("{:?}", self);
        match self {
            DerivedDeclarator::Pointer(k) => {
                if k.len() > 0 {
                    k.iter().for_each(|a| println!("{:?}", a));
                    "--[[".to_owned() + ("*".to_owned().repeat(k.len()).as_str()) + "]]"
                } else {
                    "".to_string()
                }
            }
            DerivedDeclarator::Array(k) => "--[[umm array]]".to_string(),
            DerivedDeclarator::Function(k) => k.into_lua(),
            DerivedDeclarator::KRFunction(k) => {
                "function(".to_owned()
                    + &k.iter()
                        .map(|x| x.into_lua())
                        .collect::<Vec<String>>()
                        .join(", ")
                    + ")"
            }
            DerivedDeclarator::Block(k) => "--[[umm block]]".to_string(),
            _ => format!("--[[{:?}]]", self).to_owned(),
        }
    }
}
impl IntoLua for ast::InitDeclarator {
    fn into_lua(&self) -> String {
        self.declarator.into_lua()
    }
}
impl IntoLua for ast::InitializerListItem {
    fn into_lua(&self) -> String {
        self.initializer.node.into_lua()
    }
}
impl IntoLua for ast::Initializer {
    fn into_lua(&self) -> String {
        match self {
            Initializer::Expression(b) => b.into_lua(),
            Initializer::List(b) => {
                "____C.List({".to_owned()
                    + &b.iter()
                        .map(|x| x.into_lua())
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "})"
            }
        }
    }
}
impl IntoLua for ast::Declaration {
    fn into_lua(&self) -> String {
        // todo!("implement this...");
        if self.declarators.len() == 0 {
            for x in self.specifiers.iter() {
                if let DeclarationSpecifier::TypeSpecifier(k) = &x.node {
                    if let TypeSpecifier::Enum(e) = &k.node {
                        return e.node.into_lua();
                    }
                }
            }
            return "".to_string();
        }
        "local ".to_string()+&self.declarators
            .iter()
            .map(|b|b.into_lua())
            .collect::<Vec<String>>()
            .join(", ")
            // + "--[["
            // TODO: types on these variables!
            // + ": "
            // + &self.declarators.iter().map(|b|{
            //     transform_type(
            //         b.node.declarator.node.derived.clone(),
            //         b.node.declarator.node.derived.iter().map(|x|x.into_lua()).collect::<Vec<String>>().join(", "))
            // }).into_iter().collect::<Vec<String>>().join(", ")
            + " = "
            + &self
                .declarators
                .iter()
                .map(|a| {
                    if a.node.initializer.is_some() {
                        a.node.initializer.clone().unwrap().into_lua()
                    // } else if let DeclaratorKind::Identifier(t) = &a.node.declarator.node.kind.node {
                    //     t.into_lua()
                    } else {
                        "____C.Uninitialized()".to_string()
                    }
                })
                .collect::<Vec<String>>()
                .join(", ")+";"
        // + "]]"
        // "".to_string()
    }
}
impl IntoLua for ast::StaticAssert {
    fn into_lua(&self) -> String {
        // println!("{:?}\n", self);
        // todo!("impl this 2");
        "".to_string()
    }
}
impl IntoLua for ast::FunctionDefinition {
    fn into_lua(&self) -> String {
        // println!("{:?}\n", self);
        // todo!("trait this 3");
        let mut str = "function _D.".to_owned();
        str.push_str(self.declarator.into_lua().as_str());
        str.push('(');
        str.push_str(
            self.declarator
                .node
                .derived
                .clone()
                .into_iter()
                .filter(|x| {
                    if let DerivedDeclarator::KRFunction(_) = &x.node {
                        false
                    } else {
                        true
                    }
                })
                .map(|b| b.into_lua())
                .filter(|x| !x.is_empty())
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );
        str.push(')');
        if self.specifiers.len() > 0 {
            str.push_str(": ");
            let mut has_others = false;
            for spec in self.specifiers.iter() {
                if let DeclarationSpecifier::TypeSpecifier(_) = &spec.node {
                    str.push_str(&spec.into_lua());
                } else if !has_others {
                    has_others = true
                }
            }
            if has_others {
                str.push_str(" --");
                for spec in self.specifiers.iter() {
                    // how do I invert this?
                    if let DeclarationSpecifier::TypeSpecifier(_) = &spec.node {
                    } else {
                        str.push_str(&spec.into_lua());
                    }
                }
            }
            str.push_str("\n");
        }
        str.push_str(indent(self.statement.into_lua(), 3).as_str());
        str.push_str("end");
        str
    }
}

impl IntoLua for ast::DeclarationSpecifier {
    fn into_lua(&self) -> String {
        match self {
            // NOTE: storage class serves NO purpose to me
            DeclarationSpecifier::StorageClass(s) => "".to_string(), //"--[[dspec storage class]]".to_string(),
            DeclarationSpecifier::TypeSpecifier(s) => get_practical_type(s.clone()),
            DeclarationSpecifier::TypeQualifier(s) => s.into_lua(), //"--[[dspec type qualifier]]".to_string(),
            DeclarationSpecifier::Function(s) => match s.node {
                FunctionSpecifier::Noreturn => "function(...: unknown)".to_string(),
                FunctionSpecifier::Inline => "function(...: unknown)".to_string(),
            }, //"--[[dspec function]]".to_string(),
            DeclarationSpecifier::Alignment(s) => "--[[dspec alignment]]".to_string(),
            DeclarationSpecifier::Extension(s) => {
                "".to_string() /*"--[[ext: ".to_owned()
                               + &s.iter()
                                   .map(|x| x.into_lua())
                                   .collect::<Vec<String>>()
                                   .join(", ")
                               + "]]"*/
            }
            _ => "--[[decl. specifier]]".to_string(),
        }
    }
}
impl IntoLua for ast::ExternalDeclaration {
    fn into_lua(&self) -> String {
        match self {
            ast::ExternalDeclaration::Declaration(b) => {
                if !b.into_lua().is_empty() {
                    if b.node.declarators.len() == 0 {
                        return b.into_lua();
                    }
                    if true {
                        return "".to_string();
                    }
                    b.node.declarators
                        .iter()
                        .map(|b|b.into_lua())
                        .collect::<Vec<String>>()
                        .join(", ")
                        // + "--[["
                        // TODO: types on these variables!
                        // + ": "
                        // + &b.node.declarators.iter().map(|b|{
                        //     transform_type(
                        //         b.node.declarator.node.derived.clone(),
                        //         b.node.declarator.node.derived.iter().map(|x|x.into_lua()).collect::<Vec<String>>().join(", "))
                        // }).into_iter().collect::<Vec<String>>().join(", ")
                        + " = "
                        + &b.node
                            .declarators
                            .iter()
                            .map(|a| {
                                if a.node.initializer.is_some() {
                                    a.node.initializer.clone().unwrap().into_lua()
                                } else if let DeclaratorKind::Identifier(t) = &a.node.declarator.node.kind.node {
                                    t.into_lua()
                                } else {"____C.Uninitialized()--[[maybe]]".to_string()}
                            })
                            .collect::<Vec<String>>()
                            .join(", ")+";"
                } else {
                    "--[[ ".to_string() + &format!("{:?}", b.node) + "]]"
                }
            }
            ast::ExternalDeclaration::StaticAssert(b) => {
                "--(ASSERT) local ".to_owned() + &b.into_lua() + "=" + &b.into_lua() + ";"
            }
            ast::ExternalDeclaration::FunctionDefinition(b) => b.into_lua(),
        }
    }
}
use clap::Parser;
use clio::*;
#[derive(Parser)]
#[clap(name = "lilac")]
struct Args {
    #[clap(value_parser, default_value = "-")]
    input: Vec<Input>,
    #[clap(long, short, value_parser, default_value = "-")]
    output: Output,
    #[clap(short)]
    c: bool,
    #[clap(short)]
    l: Vec<String>,
}

fn main() {
    let config = Config {
        cpp_command: "clang".to_string(),
        cpp_options: vec!["-nostdlib".to_string(), "-E".to_string()],
        flavor: lang_c::driver::Flavor::ClangC11,
    };
    let arg = Args::parse();
    if arg.l.iter().any(|x| x == "m") {
        // "linker" mode
        let mut cmd = Command::new("tlld");
        for file in arg.input.iter() {
            cmd.arg(file.path().as_os_str());
        }
        cmd.arg("-o");
        cmd.arg(arg.output.path().as_os_str());
        cmd.spawn().unwrap();
        // print!(
        //     "{}",
        //     Into::<OsStr>::into(cmd.output().unwrap().stdout)
        //         .into_string()
        //         .unwrap()
        // );
        return;
    }
    let mut s = "".to_string();
    let mut first = true;
    for file in arg.input.iter() {
        if first {
            first = false;
        } else {
            s = s + "\n\n";
        }
        s = s + "--FILE " + file.path().to_str().unwrap() + "\nlocal _D = {};\n";
        let result = parse(&config, file.path().to_str().unwrap());
        if result.is_err() {
            print!("{}", result.err().unwrap());
            return;
        }
        for node in result.unwrap().unit.0.iter() {
            s = s + &format!(
                "{}\n",
                // this doesn't work!!!
                // I don't know why!!!
                // it _should_ work, but it's not.
                // and I can't really debug it any better.
                node.into_lua()
                    .split("\n")
                    .filter(|&b| { !b.trim().is_empty() })
                    .collect::<Vec<&str>>()
                    .join("\n")
            )
        }
        s = s + "return _D;"
    }
    let output = arg.output.path();
    if arg.output.is_local() {
        let mut f = File::create(output.path()).expect("can open file");
        write!(f, "{}", s).expect("writing okay");
        f.set_modified(SystemTime::now());
    } else {
        print!("{}", s);
    }
}
