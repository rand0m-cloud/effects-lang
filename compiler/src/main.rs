mod ir;
mod rust_backend;
mod validation;
use crate::{
    ir::{build_ir, CodegenError, IR},
    rust_backend::export_to_rust,
    validation::validate_item,
};
use ast::*;
use parser::{construct_ast, tokenize_file, ParsingError};
use std::sync::Mutex;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("missing input argument for source files")]
    MissingInput,

    #[error("{0}")]
    TokenizeFailed(
        #[source]
        #[from]
        ParsingError,
    ),

    #[error("io error: {0}")]
    IoError(
        #[source]
        #[from]
        std::io::Error,
    ),

    #[error("{0}")]
    CodegenError(
        #[source]
        #[from]
        CodegenError,
    ),
}

#[derive(Debug, Clone)]
struct CompilerData {
    items: Vec<ItemDecl>,
}

#[derive(Debug)]
pub struct Compiler {
    data: Mutex<CompilerData>,
}

impl Compiler {
    pub fn new(input: &str) -> Result<Self, CompilerError> {
        let tokens = tokenize_file(input)?;
        let items = construct_ast(&tokens)?;
        Ok(Self {
            data: Mutex::new(CompilerData { items }),
        })
    }
    #[cfg(off)]
    pub fn optimize(&self) {
        let ast = &mut self.data.lock().unwrap().ast;
        ast.traverse(|expr: &mut ExpressionAST| match expr.operation {
            ExpressionOperation::Scope if expr.children.len() == 1 => {
                let new = expr.children.get(0).unwrap();
                *expr = new.clone();
            }
            ExpressionOperation::Scope if expr.children.len() > 2 => {
                let let_stmts = expr
                    .children
                    .iter()
                    .cloned()
                    .filter_map(|expr| match &expr.operation {
                        ExpressionOperation::Let(name) => {
                            Some((name.clone(), expr.children.get(0).unwrap().clone()))
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                for (name, value) in let_stmts {
                    expr.traverse(|expr: &mut ExpressionAST| match &expr.operation {
                        ExpressionOperation::Identifier(var_name) if *name == *var_name => {
                            *expr = value.clone();
                        }
                        _ => {}
                    });
                }

                let fn_defs = expr
                    .children
                    .iter()
                    .cloned()
                    .filter_map(|expr| match expr.operation {
                        ExpressionOperation::FunctionDef {
                            name, arguments, ..
                        } => Some((name, arguments, expr.children.get(0).unwrap().clone())),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                for (name, arguments, body) in fn_defs {
                    expr.traverse(|expr: &mut ExpressionAST| match &expr.operation {
                        ExpressionOperation::FunctionCall(method_name) if *name == *method_name => {
                            assert_eq!(arguments.len(), expr.children.len());
                            let let_block = arguments
                                .iter()
                                .zip(expr.children.iter())
                                .map(|((name, _arg_type), expr)| {
                                    ExpressionAST::new(ExpressionOperation::Let(name.clone()))
                                        .with_child(&expr)
                                        .clone()
                                })
                                .fold(
                                    ExpressionAST::new(ExpressionOperation::Scope),
                                    |mut acc, let_expr| {
                                        acc.with_child(&let_expr);
                                        acc
                                    },
                                );

                            *expr = let_block.clone().with_child(&body).clone();
                        }
                        _ => {}
                    });
                }
            }
            _ => {}
        });
    }

    pub fn emit_ir(&self) -> Result<IR, CompilerError> {
        let main_fn = self
            .data
            .lock()
            .unwrap()
            .items
            .iter()
            .filter_map(|item| item.as_fn_item())
            .find(|item| &item.fn_sig.name == "main")
            .cloned()
            .ok_or(CodegenError::MissingMain)?;
        Ok(build_ir(&main_fn, &self.data.lock().unwrap().items)?)
    }
}

fn main() {
    fn run() -> Result<(), CompilerError> {
        let filename = std::env::args().nth(1).ok_or(CompilerError::MissingInput)?;
        let contents = std::fs::read_to_string(filename)?;
        let compiler = Compiler::new(&contents)?;

        let items = compiler.data.lock().unwrap().items.clone();
        compiler.data.lock().unwrap().items.iter().for_each(|item| {
            validate_item(item, &items).unwrap_or_else(|err| {
                println!(
                    "failed to validate item {:?}\n{}\n\n{:?}",
                    &item.name(),
                    err,
                    &item
                )
            })
        });
        let rust_export = compiler
            .data
            .lock()
            .unwrap()
            .items
            .iter()
            .filter_map(|item| item.as_fn_item())
            .map(|item| {
                (
                    item,
                    build_ir(item, &items).unwrap_or_else(|err| {
                        println!(
                            "failed to build ir item {:?}\n{}\n\n{:?}",
                            &item.fn_sig.name, err, &item
                        );
                        IR::new(&[])
                    }),
                )
            })
            .inspect(|(_, ir)| println!("built IR: {:#?}", ir))
            .fold(String::new(), |mut acc, (item, ir)| {
                acc += "// FnItem:";
                acc += &item.fn_sig.name;
                acc += "\n";
                acc += &export_to_rust(&ir);
                acc
            });

        println!("produced this rust code output\n{}", rust_export);

        Ok(())
    }
    run().map_err(|err| println!("{}", err)).unwrap_or(());
}
