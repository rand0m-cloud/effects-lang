use ptree::{write_tree, Style, TreeItem};
use std::{
    borrow::Cow,
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
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

#[derive(Clone, Debug, Hash)]
pub enum ExpressionOperation {
    /// <cond> <true-result> <false-result>
    If,

    /// <value>
    Value(u64),

    /// <identifier>
    Identifier(String),

    /// <value> <value>
    BinaryOp(BinaryOp),

    /// <expression>*
    Scope,

    /// let <identifier> = <value>
    Let(String),

    /// <name>(<arguments>)
    FunctionCall(String),

    /// <expression>: type
    TypedExpression { expr_type: type_system::Type },

    /// handle <expression> { <interpreters> }
    HandleExpression {
        interpreters: Vec<FnItem>,
        expr: Box<ExpressionAST>,
    },
}

impl ExpressionOperation {
    pub fn as_identifier(&self) -> Option<String> {
        match self {
            Self::Identifier(name) => Some(name.clone()),
            _ => None,
        }
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
            .insert(
                name.to_string(),
                ExpressionAST::new(ExpressionOperation::Value(value)),
            );
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

#[derive(Clone, Hash)]
pub struct ExpressionAST {
    pub operation: ExpressionOperation,
    pub children: Vec<ExpressionAST>,
}

impl TreeItem for ExpressionAST {
    type Child = Self;
    fn write_self<W: Write>(&self, f: &mut W, style: &Style) -> std::io::Result<()> {
        write!(f, "{}", style.paint(format!("{:?}", self.operation)))
    }
    fn children(&self) -> Cow<'_, [Self::Child]> {
        Cow::from(&self.children[..])
    }
}

impl std::fmt::Debug for ExpressionAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut tree = Vec::new();
        write_tree(self, &mut tree).unwrap();
        f.write_str(&format!("{}", std::str::from_utf8(&tree).unwrap()))
    }
}

impl ExpressionAST {
    pub fn new(operation: ExpressionOperation) -> Self {
        Self {
            operation,
            children: vec![],
        }
    }

    pub fn with_child(&mut self, expression: &ExpressionAST) -> &mut Self {
        self.children.push(expression.clone());
        self
    }

    pub fn evaluate(&self, vars: &ExpressionScope) -> Option<u64> {
        use ExpressionOperation::*;
        match &self.operation {
            Scope => {
                vars.new_scope();
                let res = self
                    .children
                    .iter()
                    .map(move |ast| ast.evaluate(&vars))
                    .last()
                    .and_then(|opt| opt);
                vars.leave_scope();
                res
            }
            Let(name) => {
                if let [value] = &self.children[..] {
                    let value = value.evaluate(&vars).unwrap();
                    vars.insert_value(&name, value);
                    None
                } else {
                    todo!()
                }
            }
            Identifier(name) => Some(
                vars.get_value(name)
                    .expect(&format!("var {:?} to be known", &name)),
            ),
            Value(v) => Some(*v),
            If => {
                if let [cond, true_value, false_value] = &self.children[..] {
                    match cond.evaluate(&vars) {
                        Some(v) if v > 0 => true_value.evaluate(&vars),
                        _ => false_value.evaluate(&vars),
                    }
                } else {
                    todo!()
                }
            }
            BinaryOp(binary_op) => {
                if let [arg_1, arg_2] = &self.children[..] {
                    let arg_1 = arg_1.evaluate(&vars).unwrap();
                    let arg_2 = arg_2.evaluate(&vars).unwrap();
                    Some(binary_op.perform(arg_1, arg_2))
                } else {
                    todo!()
                }
            }
            FunctionCall(name) => {
                let fn_item = vars
                    .get_fn_item(&name)
                    .expect(&format!("function {} to exist", &name));

                let values = self.children.iter().collect::<Vec<_>>();
                assert_eq!(fn_item.fn_sig.arguments.len(), values.len());

                let mut call_block = ExpressionAST::new(Scope);

                for ((name, _arg_type), value) in fn_item.fn_sig.arguments.iter().zip(values) {
                    call_block
                        .with_child(ExpressionAST::new(Let(name.to_string())).with_child(&value));
                }
                //call_block.with_child(&ast.children[0]);

                call_block.evaluate(&vars)
            }
            TypedExpression { .. } => {
                if let [body] = &self.children[..] {
                    body.evaluate(&vars)
                } else {
                    todo!()
                }
            }
            HandleExpression { .. } => {
                todo!()
            }
        }
    }
    pub fn traverse<F>(&mut self, f: F)
    where
        F: FnMut(&mut ExpressionAST),
    {
        Self::traverse_ast(self, Arc::new(Mutex::new(f)));
    }

    fn traverse_ast<F>(ast: &mut ExpressionAST, f: Arc<Mutex<F>>)
    where
        F: FnMut(&mut ExpressionAST),
    {
        let hasher = BuildHasherDefault::<DefaultHasher>::default();

        let original_hash = {
            let mut hasher = hasher.build_hasher();
            ast.hash(&mut hasher);
            hasher.finish()
        };

        (f.lock().unwrap())(ast);

        let new_hash = {
            let mut hasher = hasher.build_hasher();
            ast.hash(&mut hasher);
            hasher.finish()
        };

        if original_hash != new_hash {
            println!("ast changed! {} to {}\n{:?}", original_hash, new_hash, &ast);
            Self::traverse_ast(ast, f.clone());
            return;
        }

        for child in &mut ast.children {
            Self::traverse_ast(child, f.clone());
        }
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
            use super::ExpressionOperation::*;
            Some(match &self.operation {
                Value(..) | BinaryOp(..) => Type::U64,
                Identifier(name) => vars.get_ast(&name).unwrap().get_type(vars)?,
                Scope if self.children.len() == 0 => Type::Void,
                Scope => {
                    vars.new_scope();
                    let res = self
                        .children
                        .iter()
                        .map(|expr| expr.get_type(vars))
                        .last()
                        .unwrap()?;
                    vars.leave_scope();
                    res
                }
                Let(name) => {
                    let body = self.children.get(0).unwrap();
                    vars.insert_ast(&name, body);
                    Type::Void
                }
                If => {
                    if let [_cond, true_body, false_body] = &self.children[..] {
                        let true_body_type = true_body.get_type(vars);
                        let false_body_type = false_body.get_type(vars);
                        let types = true_body_type
                            .into_iter()
                            .chain(false_body_type.into_iter())
                            .collect::<Vec<_>>();
                        match &types[..] {
                            [true_body, false_body] => {
                                if true_body == false_body {
                                    true_body.clone()
                                } else {
                                    //TODO: convert to Result
                                    return None;
                                }
                            }
                            [body] => body.clone(),
                            [] => return None,
                            _ => unreachable!(),
                        }
                    } else {
                        todo!()
                    }
                }
                FunctionCall(name) => {
                    let fn_def = vars.get_ast(&name).unwrap();
                    let return_type = match fn_def.get_type(vars)? {
                        Type::Fn(args) => args.into_iter().last().unwrap(),
                        _ => panic!(),
                    };
                    return_type
                }
                TypedExpression { expr_type } => expr_type.clone(),
                HandleExpression { .. } => todo!(),
            })
        }
    }
}
