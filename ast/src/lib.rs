use ptree::{Style, TreeItem};
use std::{
    borrow::Cow,
    collections::HashMap,
    io::Write,
    sync::{Arc, Mutex},
};

#[derive(Clone, Copy, Debug, Hash)]
pub enum BinaryOp {
    Add,
    Subtract,
    LessThan,
}

impl BinaryOp {
    pub fn perform(&self, arg_1: u64, arg_2: u64) -> u64 {
        match self {
            Self::Add => arg_1 + arg_2,
            Self::Subtract => arg_1 - arg_2,
            Self::LessThan => {
                if arg_1 < arg_2 {
                    1
                } else {
                    0
                }
            }
        }
    }
}

pub type BoxedAST = Box<ExpressionAST>;

#[derive(Clone, Debug, Hash)]
pub enum ExpressionAST {
    /// <cond> <true-result> <false-result>
    If {
        cond: BoxedAST,
        true_block: BoxedAST,
        false_block: BoxedAST,
    },

    /// <value>
    Value(u64),

    /// <identifier>
    Identifier(String),

    /// <value> <value>
    BinaryOp {
        op: BinaryOp,
        lhs: BoxedAST,
        rhs: BoxedAST,
    },

    /// <expression>*
    Scope { body: Vec<BoxedAST> },

    /// let <identifier> = <value>
    Let { ident: String, value: BoxedAST },

    /// <name>(<arguments>)
    FunctionCall {
        fn_name: String,
        arguments: Vec<BoxedAST>,
    },

    /// <expression>: type
    TypedExpression {
        expr_type: type_system::Type,
        expr: BoxedAST,
    },

    /// handle <expression> { <interpreters> }
    HandleExpression {
        interpreters: Vec<FnItem>,
        expr: BoxedAST,
    },
}

impl ExpressionAST {
    pub fn as_identifier(&self) -> Option<String> {
        match self {
            Self::Identifier(name) => Some(name.clone()),
            _ => None,
        }
    }

    pub fn evaluate(&self, vars: &ExpressionScope) -> Option<u64> {
        match &self {
            Self::Scope { body } => {
                vars.new_scope();
                let res = body
                    .iter()
                    .map(move |ast| ast.evaluate(&vars))
                    .last()
                    .and_then(|opt| opt);
                vars.leave_scope();
                res
            }
            Self::Let { ident, value } => {
                let value = value.evaluate(&vars).unwrap();
                vars.insert_value(&ident, value);
                None
            }
            Self::Identifier(name) => Some(
                vars.get_value(name)
                    .expect(&format!("var {:?} to be known", &name)),
            ),
            Self::Value(v) => Some(*v),
            Self::If {
                cond,
                true_block,
                false_block,
            } => match cond.evaluate(&vars) {
                Some(v) if v > 0 => true_block.evaluate(&vars),
                _ => false_block.evaluate(&vars),
            },
            Self::BinaryOp { op, lhs, rhs } => {
                let lhs = lhs.evaluate(&vars).unwrap();
                let rhs = rhs.evaluate(&vars).unwrap();
                Some(op.perform(lhs, rhs))
            }
            Self::FunctionCall { fn_name, arguments } => {
                let fn_item = vars
                    .get_fn_item(&fn_name)
                    .expect(&format!("function {} to exist", &fn_name));

                let values = arguments.iter().collect::<Vec<_>>();
                assert_eq!(fn_item.fn_sig.arguments.len(), values.len());

                let mut body = vec![];

                for ((name, _arg_type), value) in fn_item.fn_sig.arguments.iter().zip(values) {
                    body.push(ExpressionAST::Let {
                        ident: name.to_string(),
                        value: value.clone(),
                    });
                }
                //call_block.with_child(&ast.children[0]);

                ExpressionAST::new_scope(&body).evaluate(&vars)
            }
            Self::TypedExpression { expr, .. } => expr.evaluate(&vars),
            Self::HandleExpression { .. } => {
                todo!()
            }
        }
    }
    pub fn traverse<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut ExpressionAST),
    {
        f(self)
    }

    pub fn get_children(&self) -> Vec<ExpressionAST> {
        match self.clone() {
            Self::If {
                cond,
                true_block,
                false_block,
            } => vec![*cond, *true_block, *false_block],
            Self::Let { value, .. } => vec![*value],
            Self::BinaryOp { lhs, rhs, .. } => vec![*lhs, *rhs],
            Self::FunctionCall { arguments, .. } => {
                arguments.into_iter().map(|arg| *arg.clone()).collect()
            }
            Self::HandleExpression { expr, .. } => vec![*expr],
            Self::Scope { body } => body.into_iter().map(|ast| *ast.clone()).collect(),
            Self::TypedExpression { expr, .. } => vec![*expr],

            Self::Value(..) | Self::Identifier(..) => vec![],
        }
    }

    pub fn new_scope(body: &[ExpressionAST]) -> Self {
        Self::Scope {
            body: body.into_iter().cloned().map(Box::new).collect(),
        }
    }
}

impl TreeItem for ExpressionAST {
    type Child = Self;
    fn write_self<W: Write>(&self, f: &mut W, style: &Style) -> std::io::Result<()> {
        write!(f, "{}", style.paint(format!("{:?}", self)))
    }
    fn children(&self) -> Cow<'_, [Self::Child]> {
        Cow::from(self.get_children())
    }
}

#[derive(Clone, Debug, Default)]
struct ExpressionScopeData {
    vars: HashMap<String, ExpressionAST>,
    fn_items: HashMap<String, FnDecl>,
}

#[derive(Clone, Debug)]
pub struct ExpressionScope {
    data: Arc<Mutex<Vec<ExpressionScopeData>>>,
}
impl Default for ExpressionScope {
    fn default() -> Self {
        Self {
            data: Arc::new(Mutex::new(vec![ExpressionScopeData::default()])),
        }
    }
}

impl ExpressionScope {
    pub fn new_scope(&self) {
        self.data
            .lock()
            .unwrap()
            .push(ExpressionScopeData::default());
    }

    pub fn leave_scope(&self) {
        self.data.lock().unwrap().pop();
    }

    pub fn insert_value(&self, name: &str, value: u64) {
        self.data
            .lock()
            .unwrap()
            .iter_mut()
            .last()
            .unwrap()
            .vars
            .insert(name.to_string(), ExpressionAST::Value(value));
    }

    pub fn insert_ast(&self, name: &str, value: &ExpressionAST) {
        self.data
            .lock()
            .unwrap()
            .iter_mut()
            .last()
            .unwrap()
            .vars
            .insert(name.to_string(), value.clone());
    }

    pub fn insert_fn_item(&self, item: &FnDecl) {
        self.data
            .lock()
            .unwrap()
            .iter_mut()
            .last()
            .unwrap()
            .fn_items
            .insert(item.fn_sig.name.clone(), item.clone());
    }

    pub fn get_ast(&self, name: &str) -> Option<ExpressionAST> {
        let data = self.data.lock().unwrap();
        for scope in data.iter().rev() {
            if let Some(value_expr) = scope.vars.get(name) {
                return Some(value_expr.clone());
            }
        }
        None
    }

    pub fn get_value(&self, name: &str) -> Option<u64> {
        self.get_ast(name)?.evaluate(self)
    }

    pub fn get_fn_item(&self, name: &str) -> Option<FnDecl> {
        let data = self.data.lock().unwrap();
        for scope in data.iter().rev() {
            if let Some(item_decl) = scope.fn_items.get(name) {
                return Some(item_decl.clone());
            }
        }
        None
    }
}

#[derive(Debug, Clone, Hash)]
pub struct EffectDecl {
    pub name: String,
    pub actions: Vec<FnDecl>,
}

#[derive(Debug, Clone, Hash)]
pub struct External;

#[derive(Debug, Clone, Hash)]
pub struct FunctionSignature {
    pub external: Option<External>,
    pub name: String,
    pub arguments: Vec<(String, type_system::Type)>,
    pub effects: Vec<String>,
    pub return_type: type_system::Type,
}

impl FunctionSignature {
    pub fn into_fn_item(&self, body: &ExpressionAST) -> FnItem {
        FnItem {
            fn_sig: self.clone(),
            body: body.clone(),
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct FnItem {
    pub fn_sig: FunctionSignature,
    pub body: ExpressionAST,
}

#[derive(Debug, Clone, Hash)]
pub struct FnDecl {
    pub fn_sig: FunctionSignature,
}

#[derive(Debug, Clone, Hash)]
pub enum ItemDecl {
    EffectDecl(EffectDecl),
    FnDecl(FnDecl),
    FnItem(FnItem),
}

impl ItemDecl {
    pub fn as_effect(&self) -> Option<&EffectDecl> {
        match self {
            Self::EffectDecl(decl) => Some(decl),
            _ => None,
        }
    }

    pub fn as_fn_decl(&self) -> Option<&FnDecl> {
        match self {
            Self::FnDecl(decl) => Some(decl),
            _ => None,
        }
    }

    pub fn as_fn_item(&self) -> Option<&FnItem> {
        match self {
            Self::FnItem(item) => Some(item),
            _ => None,
        }
    }

    pub fn get_function_signature(&self) -> Option<&FunctionSignature> {
        match self {
            Self::FnDecl(FnDecl { fn_sig }) | Self::FnItem(FnItem { fn_sig, .. }) => Some(fn_sig),
            _ => None,
        }
    }
}

impl ItemDecl {
    pub fn name(&self) -> String {
        match self {
            Self::EffectDecl(decl) => &decl.name,
            Self::FnDecl(FnDecl { fn_sig }) | Self::FnItem(FnItem { fn_sig, .. }) => &fn_sig.name,
        }
        .clone()
    }
}

pub mod type_system {
    use crate::{ExpressionAST, ExpressionScope};

    #[derive(Clone, Debug, PartialEq, Hash)]
    pub enum Type {
        Void,
        U64,
        NamedTyped(String),
        Tuple(Vec<Type>),
        Fn(Vec<Type>),
    }

    pub trait TypeSystem {
        fn get_type(&self, vars: &ExpressionScope) -> Option<Type>;
    }

    impl TypeSystem for ExpressionAST {
        fn get_type(&self, vars: &ExpressionScope) -> Option<Type> {
            Some(match &self {
                Self::Value(..) | Self::BinaryOp { .. } => Type::U64,
                Self::Identifier(name) => vars.get_ast(&name).unwrap().get_type(vars)?,
                Self::Scope { body } if body.len() == 0 => Type::Void,
                Self::Scope { body } => {
                    vars.new_scope();
                    let res = body
                        .iter()
                        .map(|expr| expr.get_type(vars))
                        .last()
                        .unwrap()?;
                    vars.leave_scope();
                    res
                }
                Self::Let { ident, value } => {
                    vars.insert_ast(&ident, value);
                    Type::Void
                }
                Self::If {
                    cond,
                    true_block,
                    false_block,
                } => {
                    let true_block_type = true_block.get_type(vars);
                    let false_block_type = false_block.get_type(vars);
                    let types = true_block_type
                        .into_iter()
                        .chain(false_block_type.into_iter())
                        .collect::<Vec<_>>();
                    match &types[..] {
                        [true_block, false_block] => {
                            if true_block == false_block {
                                true_block.clone()
                            } else {
                                //TODO: convert to Result
                                return None;
                            }
                        }
                        [block] => block.clone(),
                        [] => return None,
                        _ => unreachable!(),
                    }
                }
                Self::FunctionCall { fn_name, .. } => {
                    let fn_def = vars.get_ast(&fn_name).unwrap();
                    let return_type = match fn_def.get_type(vars)? {
                        Type::Fn(args) => args.into_iter().last().unwrap(),
                        _ => panic!(),
                    }
                }
                Self::TypedExpression { expr_type, .. } => expr_type.clone(),
                Self::HandleExpression { .. } => todo!(),
            })
        }
    }
}
