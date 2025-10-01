use crate::{
    ast::{Literal, Node, NodeId}, NodeReg, Noder, StringInt
};

pub fn print_expr(nodes: &NodeReg, strint: &StringInt, depth: usize, node_id: NodeId) {
    let tab = "  ";
    match nodes.get(node_id) {
        Some(node) => {
            let tabs = tab.repeat(depth);
            match &node.node {
                Node::FieldAccess { object, field } => {
                    let field = strint.resolve(*field).unwrap();
                    println!("{}FieldAccess .{}", tabs, field);
                    print_expr(nodes, strint, depth + 1, *object);
                }
                Node::Import(node) => {
                    println!("{}Import", tabs);
                    print_expr(nodes, strint, depth + 1, *node);
                }
                Node::Export(expr) => {
                    println!("{}Export", tabs);
                    print_expr(nodes, strint, depth + 1, *expr);
                }
                Node::Reference(node_id) => {
                    println!("&(");
                    print_expr(nodes, strint, depth + 1, *node_id);
                    println!(")");
                }
                Node::Declaration { name, value, ann, export } => {
                    match ann {
                        Some(ann) => {
                            let ann = strint.resolve(*ann).unwrap_or("unknown");
                            println!(
                                "{}{}let {}: {} = ",
                                if *export { "export " } else { "" },
                                tabs,
                                strint.resolve(*name).unwrap(),
                                ann
                            );
                        }
                        None => println!("{}let {} = ", tabs, strint.resolve(*name).unwrap()),
                    }
                    if let Some(value) = value {
                        print_expr(nodes, strint, depth + 1, *value);
                    }
                }
                Node::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    println!("{}if:", tabs);
                    print_expr(nodes, strint, depth + 1, *condition);
                    println!("{}then:", tabs);
                    print_expr(nodes, strint, depth + 1, *then_branch);
                    if let Some(else_branch) = else_branch {
                        println!("{}else:", tabs);
                        print_expr(nodes, strint, depth + 1, *else_branch);
                    }
                }
                Node::WhileExpr { condition, body } => {
                    println!("{}while:", tabs);
                    print_expr(nodes, strint, depth + 1, *condition);
                    println!("{}do:", tabs);
                    print_expr(nodes, strint, depth + 1, *body);
                }
                Node::Literal(literal) => match literal {
                    Literal::Number(n) => {
                        println!("{}Number({})", tabs, n);
                    }
                    Literal::String(s) => {
                        let s = strint.resolve(*s).unwrap();
                        println!("{}String(\"{}\")", tabs, s);
                    }
                    Literal::FString {
                        parts
                    } => {
                        let parts = parts.iter().map(|nid| nid.to_string()).collect::<Vec<_>>().join(", ");
                        println!("{}FString([{}])", tabs, parts);
                    }
                    Literal::Boolean(b) => {
                        println!("{}Boolean({})", tabs, b);
                    }
                    Literal::Nil => {
                        println!("{}Nil", tabs);
                    }
                    Literal::Array { elements } => {
                        println!("{}Array [", tabs);
                        for elem in elements {
                            print_expr(nodes, strint, depth + 1, *elem);
                        }
                        println!("{}]", tabs);
                    }
                    Literal::Tuple { elements } => {
                        println!("{}Tuple (", tabs);
                        for elem in elements {
                            print_expr(nodes, strint, depth + 1, *elem);
                        }
                        println!("{} )", tabs);
                    }
                },
                Node::Identifier(name) => {
                    let name = strint.resolve(*name).unwrap();
                    println!("{}Identifier(\"{}\")", tabs, name);
                }
                Node::TypeIdent(type_ident) => {
                    let base = strint.resolve(type_ident.base).unwrap();
                    println!(
                        "{}TypeIdent({}{})",
                        tabs,
                        base,
                        "[]".repeat(type_ident.dims as usize)
                    );
                }
                Node::ForExpr {
                    init,
                    condition,
                    update,
                    body,
                } => {}
                Node::Assign { name, node: value } => {
                    let name = strint.resolve(*name).unwrap_or("unknown");
                    println!("{}\"{}\" = :", tabs, name);
                    print_expr(nodes, strint, depth + 1, *value);
                }
                Node::Function { params, body } => {
                    let param_strs: Vec<String> = params
                        .iter()
                        .map(|p| {
                            let name = strint.resolve(p.name).unwrap();
                            let type_name = if let Some(type_) = strint.resolve(p.type_.base) {
                                format!("{:?}", type_)
                            } else {
                                "unknown".to_string()
                            };
                            format!("{}: {}", name, type_name)
                        })
                        .collect();
                    println!("{}Function({}) {{", tabs, param_strs.join(", "));
                    print_expr(nodes, strint, depth + 1, *body);
                    println!("{}}}", tabs);
                }
                Node::FunctionCall { node, args } => {
                    println!("{}call (", tabs);
                    print_expr(nodes, strint, depth + 1, *node);
                    println!("{}with args:", tabs);
                    for arg in args {
                        print_expr(nodes, strint, depth + 1, *arg);
                    }
                    println!("{});", tabs);
                }
                Node::UnaryOp { op, expr } => {
                    let op_str = op.to_string();
                    println!("{}UnaryOp({})", tabs, op_str);
                    print_expr(nodes, strint, depth + 1, *expr);
                }
                Node::BinaryOp { left, op, right } => {
                    let op_str = op.to_string();
                    println!("{}BinaryOp({})", tabs, op_str);
                    print_expr(nodes, strint, depth + 1, *left);
                    print_expr(nodes, strint, depth + 1, *right);
                }
                Node::Block { statements } => {
                    println!("{}Block {{", tabs);
                    for stmt in statements {
                        print_expr(nodes, strint, depth + 1, *stmt);
                    }
                    println!("{}}}", tabs);
                }
                Node::IndexAccess { base, index } => {
                    println!("{}IndexAccess [", tabs);
                    print_expr(nodes, strint, depth + 1, *base);
                    print_expr(nodes, strint, depth + 1, *index);
                    println!("{}]", tabs);
                }
                Node::Return(expr) => {
                    println!("{}Return", tabs);
                    if let Some(expr) = expr {
                        print_expr(nodes, strint, depth + 1, *expr);
                    }
                }
                Node::Break(expr) => {
                    println!("{}Break", tabs);
                    if let Some(expr) = expr {
                        print_expr(nodes, strint, depth + 1, *expr);
                    }
                }
            }
        }
        None => {
            println!("Node ID {} not found.", node_id);
        }
    }
}
