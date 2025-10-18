use crate::{
    NodeReg, StringInt,
    ast::{Literal, Node, NodeId},
};

pub fn print_expr(nodes: &NodeReg, strint: &StringInt, depth: usize, node_id: NodeId) {
    print_expr_with_config(nodes, strint, depth, node_id, &PrintConfig::default());
}

pub struct PrintConfig {
    pub indent_size: usize,
    pub use_colors: bool,
    pub show_node_ids: bool,
    pub compact_literals: bool,
}

impl Default for PrintConfig {
    fn default() -> Self {
        Self {
            indent_size: 2,
            use_colors: false,
            show_node_ids: false,
            compact_literals: true,
        }
    }
}

pub fn print_expr_with_config(
    nodes: &NodeReg,
    strint: &StringInt,
    depth: usize,
    node_id: NodeId,
    config: &PrintConfig
) {
    let indent = " ".repeat(depth * config.indent_size);
    let child_indent = " ".repeat((depth + 1) * config.indent_size);

    match nodes.get(node_id.raw()) {
        Some(node) => {
            match &node.node {
                Node::Block { statements } => {
                    println!("{}Block {{", indent);
                    for (i, stmt) in statements.iter().enumerate() {
                        if i > 0 { println!(); }
                        print_expr_with_config(nodes, strint, depth + 1, *stmt, config);
                    }
                    println!("{}}}", indent);
                }

                Node::Declaration { name, value, ann, export } => {
                    let name_str = strint.resolve(name.raw()).unwrap_or("?");
                    let export_prefix = if *export { "export " } else { "" };

                    match ann {
                        Some(ann_id) => {
                            let ann_str = strint.resolve(ann_id.raw()).unwrap_or("unknown");
                            print!("{}{}let {}: {} =", indent, export_prefix, name_str, ann_str);
                        }
                        None => print!("{}{}let {} =", indent, export_prefix, name_str),
                    }

                    if let Some(value) = value {
                        println!();
                        print_expr_with_config(nodes, strint, depth + 1, *value, config);
                    } else {
                        println!(" <uninitialized>");
                    }
                }

                Node::Assign { target, node: value } => {
                    println!("{}Assignment:", indent);
                    println!("{}Target:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *target, config);
                    println!("{}Value:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *value, config);
                }

                Node::OpAssign { left, op, right } => {
                    println!("{}Compound Assignment ({}):", indent, op);
                    println!("{}Target:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *left, config);
                    println!("{}Value:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *right, config);
                }

                Node::BinaryOp { left, op, right } => {
                    println!("{}Binary Operation ({}):", indent, op);
                    println!("{}Left:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *left, config);
                    println!("{}Right:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *right, config);
                }

                Node::UnaryOp { op, expr } => {
                    println!("{}Unary Operation ({}):", indent, op);
                    print_expr_with_config(nodes, strint, depth + 1, *expr, config);
                }

                Node::FunctionCall { node, args } => {
                    println!("{}Function Call:", indent);
                    println!("{}Function:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *node, config);

                    if !args.is_empty() {
                        println!("{}Arguments ({}):", child_indent, args.len());
                        for (i, arg) in args.iter().enumerate() {
                            println!("{}  [{}]:", child_indent, i);
                            print_expr_with_config(nodes, strint, depth + 3, *arg, config);
                        }
                    } else {
                        println!("{}Arguments: (none)", child_indent);
                    }
                }

                Node::Function { params, body } => {
                    println!("{}Function Definition:", indent);

                    if !params.is_empty() {
                        println!("{}Parameters ({}):", child_indent, params.len());
                        for (i, param) in params.iter().enumerate() {
                            let param_name = strint.resolve(param.name.raw()).unwrap_or("?");
                            match param.type_ {
                                Some(type_node) => {
                                    println!("{}  [{}] {}: ", child_indent, i, param_name);
                                    print_expr_with_config(nodes, strint, depth + 3, type_node, config);
                                }
                                None => {
                                    println!("{}  [{}] {}: any", child_indent, i, param_name);
                                }
                            }
                        }
                    } else {
                        println!("{}Parameters: (none)", child_indent);
                    }

                    println!("{}Body:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *body, config);
                }

                Node::IfExpr { condition, then_branch, else_branch } => {
                    println!("{}If Expression:", indent);
                    println!("{}Condition:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *condition, config);
                    println!("{}Then:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *then_branch, config);

                    if let Some(else_branch) = else_branch {
                        println!("{}Else:", child_indent);
                        print_expr_with_config(nodes, strint, depth + 2, *else_branch, config);
                    }
                }

                Node::WhileExpr { condition, body } => {
                    println!("{}While Loop:", indent);
                    println!("{}Condition:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *condition, config);
                    println!("{}Body:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *body, config);
                }

                Node::IndexAccess { base, index } => {
                    println!("{}Index Access:", indent);
                    println!("{}Base:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *base, config);
                    println!("{}Index:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *index, config);
                }

                Node::FieldAccess { object, field } => {
                    let field_name = strint.resolve(field.raw()).unwrap_or("?");
                    println!("{}Field Access ({}):", indent, field_name);
                    println!("{}Object:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *object, config);
                }

                Node::Return(expr) => {
                    println!("{}Return:", indent);
                    if let Some(expr) = expr {
                        print_expr_with_config(nodes, strint, depth + 1, *expr, config);
                    } else {
                        println!("{}(implicit nil)", child_indent);
                    }
                }

                Node::Break(expr) => {
                    println!("{}Break:", indent);
                    if let Some(expr) = expr {
                        print_expr_with_config(nodes, strint, depth + 1, *expr, config);
                    } else {
                        println!("{}(no value)", child_indent);
                    }
                }

                Node::Reference(node_id) => {
                    println!("{}Reference (&):", indent);
                    print_expr_with_config(nodes, strint, depth + 1, *node_id, config);
                }

                Node::Import(node) => {
                    println!("{}Import:", indent);
                    print_expr_with_config(nodes, strint, depth + 1, *node, config);
                }

                Node::Export(expr) => {
                    println!("{}Export:", indent);
                    print_expr_with_config(nodes, strint, depth + 1, *expr, config);
                }

                Node::Inc(node_id) => {
                    println!("{}Increment:", indent);
                    print_expr_with_config(nodes, strint, depth + 1, *node_id, config);
                }

                Node::Dec(node_id) => {
                    println!("{}Decrement:", indent);
                    print_expr_with_config(nodes, strint, depth + 1, *node_id, config);
                }

                Node::ForExpr { init, condition, update, body } => {
                    println!("{}For Loop:", indent);
                    println!("{}Init:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *init, config);
                    println!("{}Condition:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *condition, config);
                    println!("{}Update:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *update, config);
                    println!("{}Body:", child_indent);
                    print_expr_with_config(nodes, strint, depth + 2, *body, config);
                }

                // Literals
                Node::Identifier(name) => {
                    let name_str = strint.resolve(name.raw()).unwrap_or("?");
                    if config.compact_literals {
                        println!("{}\"{}\"", indent, name_str);
                    } else {
                        println!("{}Identifier: \"{}\"", indent, name_str);
                    }
                }

                Node::Literal(literal) => {
                    print_literal(literal, strint, nodes, indent, child_indent, depth, config);
                }
            }
        }
        None => {
            println!("{}ERROR: Node ID {} not found", indent, node_id.raw());
        }
    }
}

fn print_literal(
    literal: &Literal,
    strint: &StringInt,
    nodes: &NodeReg,
    indent: String,
    child_indent: String,
    depth: usize,
    config: &PrintConfig
) {
    match literal {
        Literal::Number(n) => {
            if config.compact_literals {
                println!("{}{}", indent, n);
            } else {
                println!("{}Number: {}", indent, n);
            }
        }

        Literal::String(s) => {
            let s_str = strint.resolve(s.raw()).unwrap_or("?");
            if config.compact_literals {
                println!("{}\"{}\"", indent, s_str);
            } else {
                println!("{}String: \"{}\"", indent, s_str);
            }
        }

        Literal::Boolean(b) => {
            if config.compact_literals {
                println!("{}{}", indent, b);
            } else {
                println!("{}Boolean: {}", indent, b);
            }
        }

        Literal::Nil => {
            if config.compact_literals {
                println!("{}nil", indent);
            } else {
                println!("{}Nil", indent);
            }
        }

        Literal::Array { elements } => {
            println!("{}Array [{}]:", indent, elements.len());
            for (i, elem) in elements.iter().enumerate() {
                println!("{}[{}]:", child_indent, i);
                print_expr_with_config(nodes, strint, depth + 2, *elem, config);
            }
        }

        Literal::Tuple { elements } => {
            println!("{}Tuple ({}):", indent, elements.len());
            for (i, elem) in elements.iter().enumerate() {
                println!("{}({}):", child_indent, i);
                print_expr_with_config(nodes, strint, depth + 2, *elem, config);
            }
        }

        Literal::Table { elements } => {
            println!("{}Table [{}]:", indent, elements.len());
            for (i, (key, value)) in elements.iter().enumerate() {
                println!("{}Entry {}:", child_indent, i);
                println!("{}  Key:", child_indent);
                print_expr_with_config(nodes, strint, depth + 3, *key, config);
                println!("{}  Value:", child_indent);
                print_expr_with_config(nodes, strint, depth + 3, *value, config);
            }
        }

        Literal::Object { elements } => {
            println!("{}Object {{{}}}:", indent, elements.len());
            for (key, value) in elements {
                let key_str = strint.resolve(key.0).unwrap_or("?");
                println!("{}.{}:", child_indent, key_str);
                print_expr_with_config(nodes, strint, depth + 2, *value, config);
            }
        }

        Literal::FString { parts } => {
            println!("{}Format String [{}]:", indent, parts.len());
            for (i, part) in parts.iter().enumerate() {
                println!("{}Part {}:", child_indent, i);
                print_expr_with_config(nodes, strint, depth + 2, *part, config);
            }
        }
    }
}

// Convenience function for pretty printing with better formatting
pub fn pretty_print(nodes: &NodeReg, strint: &StringInt, node_id: NodeId) {
    let config = PrintConfig {
        indent_size: 2,
        use_colors: false,
        show_node_ids: false,
        compact_literals: true,
    };

    println!("AST Structure:");
    println!("==============");
    print_expr_with_config(nodes, strint, 0, node_id, &config);
    println!("==============");
}

// Function for compact debugging output
pub fn debug_print(nodes: &NodeReg, strint: &StringInt, node_id: NodeId) {
    let config = PrintConfig {
        indent_size: 1,
        use_colors: false,
        show_node_ids: true,
        compact_literals: true,
    };

    println!("Debug AST:");
    print_expr_with_config(nodes, strint, 0, node_id, &config);
}
