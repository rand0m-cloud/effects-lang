use crate::ir::{IROp, IR};
use ast::BinaryOp;

pub fn export_binary_op(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        x @ _ => todo!("{:?}", x),
    }
    .to_string()
}

pub fn export_to_rust(ir: &IR) -> String {
    let mut result = String::new();
    let mut ir_iter = ir.as_ref().iter();
    while let Some(op) = ir_iter.next() {
        result += &match op {
            IROp::FunctionDefinition(name, argument_count, body) => {
                let arguments = (0..*argument_count).map(|i| format!("arg{}: u64", i)).fold(
                    String::new(),
                    |mut acc, arg| {
                        acc += &arg;
                        acc += ",";
                        acc
                    },
                );
                format!(
                    "fn {}({}) {{\n{}\n}}\n",
                    &name.replace("::", "_"),
                    &arguments,
                    &export_to_rust(body)
                )
            }
            IROp::CallFunction(name, args) => format!(
                "{}({});\n",
                &name.replace("::", "_"),
                args.iter().fold(String::new(), |mut acc, arg| {
                    acc += &export_to_rust(arg);
                    acc += ", ";
                    acc
                })
            ),
            IROp::Value(value) => value.to_string(),
            IROp::GetArgument(i) => format!("arg{}", i),
            IROp::Label(label) => format!("\n//{}\n", label),
            IROp::BinaryOp(op) => export_binary_op(op),
            IROp::IfElseBlock {
                cond,
                tblock,
                fblock,
            } => format!(
                "if {} {{ {} }} else {{ {} }}",
                export_to_rust(cond),
                export_to_rust(tblock),
                export_to_rust(fblock)
            ),
        };
    }
    result
}
