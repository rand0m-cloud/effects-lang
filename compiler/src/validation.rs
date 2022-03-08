//TODO: refactor effect type checking with HashSet (is_subset, is_superset)
use ast::*;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

pub fn validate_item(item: &ItemDecl, environment: &[ItemDecl]) -> Result<(), anyhow::Error> {
    validate_syntax(item)?;
    typecheck(item, environment)?;
    Ok(())
}

#[derive(Debug, Error)]
pub enum SyntaxError {}

//TODO
fn validate_syntax(item: &ItemDecl) -> Result<(), SyntaxError> {
    Ok(())
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("unused effects {0:?}")]
    UnusedEffects(Vec<String>),

    #[error("missing effects annotation {0:?}")]
    MissingEffects(Vec<String>),

    #[error("missing interpreter function {0}")]
    MissingInterpreter(String),

    #[error("unknown function {0}")]
    UnknownFunction(String),

    #[error("expected {0} arguments, found {1} arguments")]
    IncorrectNumberOfArguments(usize, usize),

    #[error("unknown variable {0}")]
    UnknownVariable(String),

    #[error("type mismatch; expected {0:?}, got {1:?}")]
    TypeMismatch(type_system::Type, type_system::Type),
}

#[derive(Clone, Debug, Default)]
pub struct Module {
    fns: HashMap<String, ItemDecl>,
}

impl Module {
    pub fn add_items(&mut self, environment: &[ItemDecl]) -> Result<(), ()> {
        for item in environment {
            match &item {
                ItemDecl::FnDecl(_) | ItemDecl::FnItem(_) => {
                    let fn_sig = item.get_function_signature().unwrap();
                    if self.fns.insert(fn_sig.name.clone(), item.clone()).is_some() {
                        return Err(());
                    }
                }
                ItemDecl::EffectDecl(decl) => {
                    for action in &decl.actions {
                        if self
                            .fns
                            .insert(
                                format!("{}::{}", &decl.name, &action.fn_sig.name),
                                item.clone(),
                            )
                            .is_some()
                        {
                            return Err(());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn get_item(&self, name: &str) -> Option<&ItemDecl> {
        self.fns.get(name)
    }
}

fn typecheck(item: &ItemDecl, environment: &[ItemDecl]) -> Result<(), TypeError> {
    match item {
        ItemDecl::FnItem(FnItem {
            fn_sig:
                FunctionSignature {
                    arguments,
                    effects,
                    return_type,
                    ..
                },
            body,
        }) => {
            #[derive(Debug, Default, Clone)]
            struct Env {
                used_effects: Vec<String>,
                handled_effects: Vec<String>,
                vars: ExpressionScope,
                function_arguments: HashMap<String, type_system::Type>,
            }
            fn go(
                module: &Module,
                ast: &ExpressionAST,
                env: &mut Env,
            ) -> Result<type_system::Type, TypeError> {
                Ok(match &ast.operation {
                    ExpressionOperation::If => {
                        let (cond, tblock, fblock) =
                            if let [cond, tblock, fblock] = &ast.children[..] {
                                (cond, tblock, fblock)
                            } else {
                                panic!()
                            };

                        let cond_ty = go(module, &cond, env)?;
                        let tblock_ty = go(module, &tblock, env)?;
                        let fblock_ty = go(module, &fblock, env)?;

                        if cond_ty != type_system::Type::U64 {
                            return Err(TypeError::TypeMismatch(type_system::Type::U64, cond_ty));
                        }

                        if tblock_ty != fblock_ty {
                            return Err(TypeError::TypeMismatch(tblock_ty, fblock_ty));
                        }

                        tblock_ty
                    }
                    ExpressionOperation::TypedExpression { expr_type: ty } => {
                        let child = &ast.children[0];
                        let expr_ty = go(module, &child, env)?;
                        if ty != &expr_ty {
                            return Err(TypeError::TypeMismatch(ty.clone(), expr_ty));
                        }
                        ty.clone()
                    }
                    ExpressionOperation::Let(name) => {
                        env.vars.insert_ast(name, &ast.children[0]);
                        type_system::Type::Void
                    }
                    ExpressionOperation::Identifier(name) => {
                        if let Some(ty) = env.function_arguments.get(name) {
                            return Ok(ty.clone());
                        }
                        let ast = env
                            .vars
                            .get_ast(name)
                            .ok_or(TypeError::UnknownVariable(name.clone()))?;
                        go(module, &ast, env)?
                    }
                    ExpressionOperation::Scope if ast.children.len() == 0 => {
                        type_system::Type::Void
                    }
                    ExpressionOperation::Scope => {
                        env.vars.new_scope();
                        let mut result = None;
                        for child in &ast.children {
                            result = Some(go(module, &child, env)?);
                        }
                        env.vars.leave_scope();
                        result.unwrap()
                    }
                    ExpressionOperation::Value(_) => type_system::Type::U64,
                    ExpressionOperation::BinaryOp(_) => {
                        let (left, right) = if let [left, right] = &ast.children[..] {
                            (left, right)
                        } else {
                            panic!();
                        };

                        let left_ty = go(module, &left, env)?;
                        let right_ty = go(module, &right, env)?;
                        if left_ty != type_system::Type::U64 {
                            return Err(TypeError::TypeMismatch(type_system::Type::U64, left_ty));
                        }

                        if right_ty != type_system::Type::U64 {
                            return Err(TypeError::TypeMismatch(type_system::Type::U64, right_ty));
                        }

                        type_system::Type::U64
                    }
                    ExpressionOperation::HandleExpression { interpreters, expr } => {
                        let effects = interpreters
                            .iter()
                            .filter_map(|item| {
                                Some(
                                    module
                                        .get_item(&item.fn_sig.name)?
                                        .as_effect()
                                        .cloned()?
                                        .name,
                                )
                            })
                            .collect::<Vec<_>>();

                        for eff in &effects {
                            env.handled_effects.push(eff.clone());
                        }

                        let expr_type = go(module, &expr, env)?;

                        for _eff in &effects {
                            env.handled_effects.pop();
                        }
                        expr_type
                    }
                    ExpressionOperation::FunctionCall(func_name) => {
                        let fn_sig = module
                            .get_item(func_name)
                            .and_then(|item| match item {
                                ItemDecl::FnDecl(_) | ItemDecl::FnItem(_) => {
                                    item.get_function_signature()
                                }
                                ItemDecl::EffectDecl(EffectDecl { name, actions }) => {
                                    let fn_def = actions.iter().find(|action| {
                                        &format!("{}::{}", name, &action.fn_sig.name) == func_name
                                    })?;
                                    Some(&fn_def.fn_sig)
                                }
                            })
                            .ok_or(TypeError::UnknownFunction(func_name.clone()))?;
                        if fn_sig.arguments.len() != ast.children.len() {
                            return Err(TypeError::IncorrectNumberOfArguments(
                                fn_sig.arguments.len(),
                                ast.children.len(),
                            ));
                        }

                        for child in &ast.children {
                            go(module, &child, env)?;
                        }

                        for effect in &fn_sig.effects {
                            if env
                                .handled_effects
                                .iter()
                                .find(|eff| eff == &effect)
                                .is_none()
                                && env.used_effects.iter().find(|eff| eff == &effect).is_none()
                            {
                                env.used_effects.push(effect.clone());
                            }
                        }

                        fn_sig.return_type.clone()
                    }
                })
            }

            let mut module = Module::default();
            module
                .add_items(environment)
                .expect("adding items to module to not fail");

            let mut env = Env::default();
            env.function_arguments = arguments.iter().cloned().collect();
            go(&module, body, &mut env)?;

            let used_effects = env.used_effects.iter().collect::<HashSet<_>>();
            let declared_effects = effects.iter().collect::<HashSet<_>>();

            if !declared_effects.is_superset(&used_effects) {
                return Err(TypeError::MissingEffects(
                    used_effects
                        .difference(&declared_effects)
                        .cloned()
                        .cloned()
                        .collect(),
                ));
            };

            if used_effects.len() < declared_effects.len() {
                return Err(TypeError::UnusedEffects(
                    declared_effects
                        .difference(&used_effects)
                        .cloned()
                        .cloned()
                        .collect(),
                ));
            }

            Ok(())
        }
        //TODO
        ItemDecl::EffectDecl { .. } => Ok(()),
        ItemDecl::FnDecl { .. } => Ok(()),
    }
}
