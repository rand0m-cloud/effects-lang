use ast::*;
use std::collections::HashMap;
use thiserror::*;

#[derive(Debug, Clone)]
pub enum IROp {
    Value(u64),
    Label(String),
    IfElseBlock { cond: IR, tblock: IR, fblock: IR },
    BinaryOp(BinaryOp),
    CallFunction(String, Vec<IR>),
    GetArgument(u64),
    FunctionDefinition(String, u64, IR),
}

#[derive(Clone)]
pub struct IR {
    ops: Vec<IROp>,
}

impl std::fmt::Debug for IR {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self.ops)
    }
}

impl AsRef<[IROp]> for IR {
    fn as_ref(&self) -> &[IROp] {
        &self.ops
    }
}

impl IR {
    pub fn new(ops: &[IROp]) -> Self {
        Self {
            ops: ops.iter().cloned().collect(),
        }
    }

    pub fn append(&mut self, ops: &[IROp]) {
        self.ops.extend(ops.iter().cloned());
    }
}

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("unknown identifier {0}")]
    UnknownIdentifier(String),

    #[error("unknown variable {0}")]
    UnknownVariable(String),

    #[error("missing main function")]
    MissingMain,
}
pub fn build_ir(item: &FnItem, environment: &[ItemDecl]) -> Result<IR, CodegenError> {
    #[derive(Debug)]
    struct Env {
        unique_number: u64,
        vars: ExpressionScope,
        items: HashMap<String, FnItem>,
        all_items: Vec<ItemDecl>,
        fn_args: Vec<String>,
    }
    impl Env {
        fn new(item: &FnItem, environment: &[ItemDecl]) -> Self {
            let fn_args = item
                .fn_sig
                .arguments
                .iter()
                .map(|(name, _ty)| name)
                .cloned()
                .collect::<Vec<_>>();
            let vars = environment
                .iter()
                .filter_map(|item| item.as_fn_decl())
                .fold(ExpressionScope::default(), |scope, item| {
                    scope.insert_fn_item(item);
                    scope
                });
            Self {
                unique_number: 0,
                vars,
                items: environment
                    .iter()
                    .filter_map(|item| item.as_fn_item())
                    .map(|item| (item.fn_sig.name.clone(), item.clone()))
                    .collect::<HashMap<_, _>>(),
                fn_args,
                all_items: environment.iter().cloned().collect(),
            }
        }
        fn unique_number(&mut self) -> u64 {
            let res = self.unique_number;
            self.unique_number += 1;
            res
        }
    }
    fn go(expression: &ExpressionAST, env: &mut Env) -> Result<IR, CodegenError> {
        use ExpressionOperation::*;
        let ir = match &expression.operation {
            Value(value) => IR::new(&[IROp::Value(*value)]),
            Identifier(name) => {
                if let Some(arg_pos) = env.fn_args.iter().position(|arg_name| arg_name == name) {
                    IR::new(&[IROp::GetArgument(arg_pos as u64)])
                } else {
                    go(
                        &env.vars
                            .get_ast(&name)
                            .ok_or(CodegenError::UnknownVariable(name.clone()))?,
                        env,
                    )?
                }
            }
            If => {
                if let [cond, true_body, false_body] = &expression.children[..] {
                    let mut cond = go(&cond, env)?;
                    let true_body = go(&true_body, env)?;
                    let false_body = go(&false_body, env)?;
                    IR::new(&[IROp::IfElseBlock {
                        cond,
                        tblock: true_body,
                        fblock: false_body,
                    }])
                } else {
                    todo!()
                }
            }
            BinaryOp(op) => {
                if let [arg_1, arg_2] = &expression.children[..] {
                    let arg_1 = go(&arg_1, env)?;
                    let mut arg_2 = go(&arg_2, env)?;
                    arg_2.append(arg_1.as_ref());
                    arg_2.append(&[IROp::BinaryOp(*op)]);
                    arg_2
                } else {
                    todo!()
                }
            }
            Scope => {
                let mut scope = IR::new(&[]);
                env.vars.new_scope();
                for child in &expression.children {
                    scope.append(go(child, env)?.as_ref());
                }
                env.vars.leave_scope();
                scope
            }
            Let(name) => {
                if let [body] = &expression.children[..] {
                    env.vars.insert_ast(&name, &body);
                    go(body, env)?
                } else {
                    todo!()
                }
            }
            FunctionCall(name) => {
                let mut ir = Vec::new();
                for expr in expression.children.iter().rev() {
                    ir.push(go(expr, env)?);
                }
                IR::new(&[IROp::CallFunction(name.clone(), ir)])
            }
            TypedExpression { .. } => go(expression.children.get(0).unwrap(), env)?,
            HandleExpression { interpreters, expr } => {
                let mut ir = IR::new(&[]);

                env.vars.new_scope();

                for interpreter in interpreters {
                    env.items
                        .insert(interpreter.fn_sig.name.clone(), interpreter.clone());
                    ir.append(build_ir(interpreter, &env.all_items)?.as_ref());
                }
                ir.append(go(expr, env)?.as_ref());

                env.vars.leave_scope();
                ir
            }
        };
        Ok(ir)
    }

    //start of function

    let mut env = Env::new(item, environment);
    Ok(IR::new(&[IROp::FunctionDefinition(
        item.fn_sig.name.clone(),
        item.fn_sig.arguments.len() as u64,
        go(&item.body, &mut env)?,
    )]))
}
