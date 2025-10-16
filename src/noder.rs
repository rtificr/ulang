use anyhow::{anyhow, bail};
use pest::{Parser, iterators::Pair};
use slab::Slab;
use string_interner::StringInterner;

use crate::{
    ast::{FuncParam, Literal, Node, NodeId, Operator, Span, SpannedNode, StringId, TypeIdent}, err::ParseError, NodeReg, Rule, StringInt, TypeReg, UParser
};

pub struct Noder {
    nodes: NodeReg,
    strint: StringInt,
    types: TypeReg,
}
impl Noder {
    pub fn new() -> Self {
        Noder {
            nodes: Slab::new(),
            strint: StringInterner::new(),
            types: TypeReg::new(),
        }
    }
    pub fn use_strint(&mut self, strint: StringInt) {
        self.strint = strint;
    }

    /// Helper method to safely handle pairs that must return Some(NodeId)
    fn require_node(&mut self, pair: Pair<Rule>) -> anyhow::Result<NodeId> {
        let rule = pair.as_rule();
        match self.handle_pair(pair)? {
            Some(id) => Ok(id),
            None => bail!("Expected node but got None for rule: {:?}", rule),
        }
    }

    /// Helper method to create a spanned node from a Pest pair
    fn insert_spanned_node(&mut self, node: Node, span: Span) -> NodeId {
        let spanned_node = SpannedNode::new(node, span);
        NodeId(self.nodes.insert(spanned_node))
    }

    pub fn handle_pair(&mut self, pair: Pair<Rule>) -> anyhow::Result<Option<NodeId>> {
        let span = Span::new(pair.as_span().start(), pair.as_span().end());
        let r = match pair.as_rule() {
            Rule::let_expr | Rule::export_let_expr => {
                let is_export = pair.as_rule() == Rule::export_let_expr;
                let mut inner_rules = pair.into_inner();
                let name = inner_rules.nth(1).unwrap().as_str();
                let value = self.handle_pair(inner_rules.next().unwrap())?;
                let node = Node::Declaration {
                    name: StringId(self.strint.get_or_intern(name)),
                    ann: None,
                    value,
                    export: is_export,
                };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::EOI => None,
            Rule::file => {
                let mut ids = Vec::new();
                for (i, inner) in pair.into_inner().enumerate() {
                    match self.handle_pair(inner) {
                        Ok(Some(id)) => {
                            ids.push(id);
                        }
                        Ok(None) => {}
                        Err(e) => {}
                    }
                }
                Some(self.insert_spanned_node(Node::Block { statements: ids }, span))
            }
            Rule::reference_expr => {
                let node = self
                    .handle_pair(
                        pair.into_inner()
                            .next()
                            .ok_or(anyhow!("No body found for reference expression"))?,
                    )?
                    .ok_or(anyhow!("Couldn't handle pair in reference expression"))?;
                Some(self.insert_spanned_node(Node::Reference(node), span))
            }
            Rule::program => {
                let mut ids = Vec::new();
                for inner in pair.into_inner() {
                    if let Some(id) = self.handle_pair(inner)? {
                        ids.push(id);
                    }
                }
                Some(self.insert_spanned_node(Node::Block { statements: ids }, span))
            }
            Rule::expr => {
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::let_annotated | Rule::export_let_annotated => {
                let is_export = pair.as_rule() == Rule::export_let_annotated;
                let mut inner_rules = pair.into_inner();
                let name = inner_rules.nth(1).unwrap().as_str();
                let ann = inner_rules.next().unwrap();
                let ann = Some(self.require_node(ann)?);
                let value = self.handle_pair(inner_rules.next().unwrap())?;
                let node = Node::Declaration {
                    name: crate::ast::StringId::from_raw(self.strint.get_or_intern(name)),
                    ann,
                    value,
                    export: is_export,
                };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::if_expr => {
                let mut inner_rules = pair.into_inner();

                // Skip the 'if' keyword and get the condition expression
                let mut current_pair = inner_rules.next().unwrap();
                while current_pair.as_rule() == Rule::r#if {
                    current_pair = inner_rules.next().unwrap();
                }

                let condition = self.require_node(current_pair)?;
                let then_branch = self.require_node(inner_rules.next().unwrap())?;
                let else_branch = if let Some(else_pair) = inner_rules.next() {
                    // Skip 'else' keyword if present
                    if else_pair.as_rule() == Rule::r#else {
                        if let Some(else_expr) = inner_rules.next() {
                            self.handle_pair(else_expr)?
                        } else {
                            None
                        }
                    } else {
                        self.handle_pair(else_pair)?
                    }
                } else {
                    None
                };
                let node = Node::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::while_expr => {
                let mut inner_rules = pair.into_inner();

                // Skip the 'while' keyword and get the condition and body expressions
                let mut current_pair = inner_rules.next().unwrap();
                while current_pair.as_rule() == Rule::r#while {
                    current_pair = inner_rules.next().unwrap();
                }

                let condition = self.require_node(current_pair)?;
                let body = self.require_node(inner_rules.next().unwrap())?;
                let node = Node::WhileExpr { condition, body };
                Some(self.insert_spanned_node(node, span))
            }
            // Rule::for_expr => {
            //     let mut inner_rules = pair.into_inner();
            //     let init = inner_rules.next().unwrap().into_inner();
            //     let condition = self.handle_pair(inner_rules.next().unwrap())?.unwrap();
            //     let update = inner_rules.next().unwrap().into_inner();
            //     let body = inner_rules.next().unwrap().into_inner();

            //     let mut init_nodes = Vec::new();
            //     for stmt_pair in init {
            //         if let Some(stmt_id) = self.handle_pair(stmt_pair)? {
            //             init_nodes.push(stmt_id);
            //         }
            //     }

            //     let mut update_nodes = Vec::new();
            //     for stmt_pair in update {
            //         if let Some(stmt_id) = self.handle_pair(stmt_pair)? {
            //             update_nodes.push(stmt_id);
            //         }
            //     }

            //     let mut body_nodes = Vec::new();
            //     for stmt_pair in body {
            //         if let Some(stmt_id) = self.handle_pair(stmt_pair)? {
            //             body_nodes.push(stmt_id);
            //         }
            //     }

            //     let node = Node::ForExpr {
            //         init: init_nodes,
            //         condition,
            //         update: update_nodes,
            //         body: body_nodes,
            //     };
            //     Some(self.insert_spanned_node(node, span))
            // }
            Rule::function_expr => {
                let mut inner_rules = pair.into_inner();

                // Skip any tokens that aren't parameter_list or expr (like the fn keyword)
                let mut current_pair = inner_rules.next().unwrap();
                while current_pair.as_rule() != Rule::parameter_list
                    && current_pair.as_rule() != Rule::expr
                    && current_pair.as_rule() != Rule::block_expr
                {
                    current_pair = inner_rules.next().unwrap();
                }

                let (params, body_pair) = if current_pair.as_rule() == Rule::parameter_list {
                    // We have parameters, so parse them
                    let mut params_vec = Vec::new();
                    for param_pair in current_pair.into_inner() {
                        let mut param_inner = param_pair.into_inner();
                        let name = param_inner.next().unwrap().as_str();
                        let type_node = if let Some(type_ann_pair) = param_inner.next() {
                            self.handle_pair(type_ann_pair)?.ok_or(anyhow!("No type annotation for function declaration"))?
                        } else {
                            return Err(ParseError::MissingTypeAnnotation {
                                ident: name.to_string(),
                            }
                            .into());
                        };
                        params_vec.push(FuncParam {
                            name: crate::ast::StringId::from_raw(self.strint.get_or_intern(name)),
                            type_: type_node,
                        });
                    }
                    // The next item should be the body expression - skip any intermediate tokens
                    let mut body_pair = inner_rules.next().unwrap();
                    while body_pair.as_rule() != Rule::expr
                        && body_pair.as_rule() != Rule::block_expr
                    {
                        body_pair = inner_rules.next().unwrap();
                    }
                    (params_vec, body_pair)
                } else {
                    // No parameters, current_pair is the body
                    (Vec::new(), current_pair)
                };

                let body = self.handle_pair(body_pair)?;
                if body.is_none() {
                    return Err(ParseError::EmptyFunctionBody.into());
                }
                let node = Node::Function {
                    params,
                    body: body.unwrap(),
                };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::postfix => {
                let mut inner_rules = pair.into_inner();
                let primary_pair = inner_rules.next().unwrap();
                let mut base_expr = self.require_node(primary_pair)?;

                // Process each postfix operation in sequence
                for suffix_pair in inner_rules {
                    base_expr = match suffix_pair.as_rule() {
                        Rule::call_suffix => {
                            let mut args = Vec::new();
                            for arg_pair in suffix_pair.into_inner() {
                                if let Some(arg_id) = self.handle_pair(arg_pair)? {
                                    args.push(arg_id);
                                }
                            }

                            let node = Node::FunctionCall {
                                node: base_expr,
                                args,
                            };
                            self.insert_spanned_node(node, span)
                        }
                        Rule::index_suffix => {
                            // Array/object indexing
                            let index_expr = suffix_pair.into_inner().next().unwrap();
                            let index = self.require_node(index_expr)?;
                            let node = Node::IndexAccess {
                                base: base_expr,
                                index,
                            };
                            self.insert_spanned_node(node, span)
                        }
                        Rule::field_suffix => {
                            // Field access
                            let field_name = suffix_pair.into_inner().next().unwrap();
                            let node = Node::FieldAccess {
                                object: base_expr,
                                field: crate::ast::StringId::from_raw(self.strint.get_or_intern(field_name.as_str())),
                            };
                            self.insert_spanned_node(node, span)
                        }
                        _ => bail!("Unexpected postfix operation: {:?}", suffix_pair.as_rule()),
                    };
                }

                Some(base_expr)
            }
            Rule::return_expr => {
                let inner = pair.into_inner().next();
                let return_id = if let Some(expr_pair) = inner {
                    self.handle_pair(expr_pair)?
                } else {
                    None
                };
                let node = Node::Return(return_id); // Placeholder NodeId
                Some(self.insert_spanned_node(node, span))
            }
            Rule::break_expr => {
                let inner = pair.into_inner().next();
                let break_id = if let Some(expr_pair) = inner {
                    self.handle_pair(expr_pair)?
                } else {
                    None
                };
                let node = Node::Break(break_id); // Placeholder NodeId
                Some(self.insert_spanned_node(node, span))
            }
            Rule::block_expr => {
                let mut statements = Vec::new();
                for stmt_pair in pair.into_inner() {
                    if let Some(stmt_id) = self.handle_pair(stmt_pair)? {
                        statements.push(stmt_id);
                    }
                }
                let node = Node::Block { statements };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::parameter_list => {
                // This rule is typically handled by parent rules, but if called directly,
                // it should delegate to its children
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::parameter => {
                // This rule is typically handled by parent rules, but if called directly,
                // it should delegate to its children
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::assignment => {
                // assignment = postfix "=" expr
                let mut inner_rules = pair.into_inner();
                let lhs_pair = inner_rules.next().unwrap();
                let rhs_pair = inner_rules.next().unwrap();

                // We expect the LHS to be a postfix expression. Parse it and
                // inspect the produced node to determine the target kind.
                let lhs_node = match self.handle_pair(lhs_pair)? {
                    Some(id) => id,
                    None => return Ok(None),
                };

                let value = match self.handle_pair(rhs_pair)? {
                    Some(id) => id,
                    None => return Ok(None),
                };

                // Accept any postfix as an assignment target; store the LHS node id
                // as the target and the RHS as the assigned node. Validation of
                // whether the target is writable happens at runtime.
                let node = Node::Assign { target: lhs_node, node: value };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::logical_or => {
                let inner_rules: Vec<_> = pair.into_inner().collect();
                if inner_rules.len() == 1 {
                    // Single operand, just forward it
                    return self.handle_pair(inner_rules.into_iter().next().unwrap());
                } else if inner_rules.len() == 2 {
                    // Binary operation: operand1 || operand2
                    let left = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let right = match self.handle_pair(inner_rules[1].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let op = Operator::Or;
                    let node = Node::BinaryOp { left, op, right };
                    Some(self.insert_spanned_node(node, span))
                } else {
                    // More than 2 operands - chain operations left to right
                    let mut result = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    for operand in inner_rules.iter().skip(1) {
                        let right = match self.handle_pair(operand.clone())? {
                            Some(id) => id,
                            None => return Ok(None),
                        };
                        let op = Operator::Or;
                        let node = Node::BinaryOp {
                            left: result,
                            op,
                            right,
                        };
                        result = self.insert_spanned_node(node, span);
                    }
                    Some(result)
                }
            }
            Rule::logical_and => {
                let inner_rules: Vec<_> = pair.into_inner().collect();
                if inner_rules.len() == 1 {
                    // Single operand, just forward it
                    return self.handle_pair(inner_rules.into_iter().next().unwrap());
                } else if inner_rules.len() == 2 {
                    // Binary operation: operand1 && operand2
                    let left = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let right = match self.handle_pair(inner_rules[1].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let op = Operator::And;
                    let node = Node::BinaryOp { left, op, right };
                    Some(self.insert_spanned_node(node, span))
                } else {
                    // More than 2 operands - chain operations left to right
                    let mut result = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    for operand in inner_rules.iter().skip(1) {
                        let right = match self.handle_pair(operand.clone())? {
                            Some(id) => id,
                            None => return Ok(None),
                        };
                        let op = Operator::And;
                        let node = Node::BinaryOp {
                            left: result,
                            op,
                            right,
                        };
                        result = self.insert_spanned_node(node, span);
                    }
                    Some(result)
                }
            }
            Rule::comparison => {
                let full_text = pair.as_str();
                let pair_start = pair.as_span().start();
                let inner_rules: Vec<_> = pair.into_inner().collect();

                if inner_rules.len() == 1 {
                    // Single operand, just forward it
                    return self.handle_pair(inner_rules.into_iter().next().unwrap());
                } else if inner_rules.len() == 2 {
                    // Binary operation: operand1 == operand2 (or other comparison)
                    let left = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let right = match self.handle_pair(inner_rules[1].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };

                    // Extract the operator by looking at the text between operands
                    let left_end = inner_rules[0].as_span().end();
                    let right_start = inner_rules[1].as_span().start();
                    let operator_text =
                        full_text[left_end - pair_start..right_start - pair_start].trim();

                    let op = match operator_text {
                        "==" => Operator::Equal,
                        "!=" => Operator::NotEqual,
                        "<=" => Operator::LessThanOrEqual,
                        ">=" => Operator::GreaterThanOrEqual,
                        "<" => Operator::LessThan,
                        ">" => Operator::GreaterThan,
                        _ => Operator::Equal, // fallback
                    };
                    let node = Node::BinaryOp { left, op, right };
                    Some(self.insert_spanned_node(node, span))
                } else {
                    // More than 2 operands - chain operations left to right
                    let mut result = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    for (i, operand) in inner_rules.iter().skip(1).enumerate() {
                        let right = match self.handle_pair(operand.clone())? {
                            Some(id) => id,
                            None => return Ok(None),
                        };

                        // Extract operator between previous operand and current one
                        let prev_end = inner_rules[i].as_span().end();
                        let curr_start = operand.as_span().start();
                        let operator_text =
                            full_text[prev_end - pair_start..curr_start - pair_start].trim();

                        let op = match operator_text {
                            "==" => Operator::Equal,
                            "!=" => Operator::NotEqual,
                            "<=" => Operator::LessThanOrEqual,
                            ">=" => Operator::GreaterThanOrEqual,
                            "<" => Operator::LessThan,
                            ">" => Operator::GreaterThan,
                            _ => Operator::Equal, // fallback
                        };
                        let node = Node::BinaryOp {
                            left: result,
                            op,
                            right,
                        };
                        result = self.insert_spanned_node(node, span);
                    }
                    Some(result)
                }
            }
            Rule::additive => {
                let full_text = pair.as_str();
                let pair_start = pair.as_span().start();
                let inner_rules: Vec<_> = pair.into_inner().collect();

                if inner_rules.len() == 1 {
                    // Single operand, just forward it
                    return self.handle_pair(inner_rules.into_iter().next().unwrap());
                } else if inner_rules.len() == 2 {
                    // Binary operation: operand1 + operand2 (or -)
                    let left = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let right = match self.handle_pair(inner_rules[1].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };

                    // Extract the operator by looking at the text between operands
                    let left_end = inner_rules[0].as_span().end();
                    let right_start = inner_rules[1].as_span().start();
                    let operator_text =
                        full_text[left_end - pair_start..right_start - pair_start].trim();

                    let op = match operator_text {
                        "+" => Operator::Add,
                        "-" => Operator::Subtract,
                        _ => Operator::Add, // fallback
                    };
                    let node = Node::BinaryOp { left, op, right };
                    Some(self.insert_spanned_node(node, span))
                } else {
                    // More than 2 operands - chain operations left to right
                    let mut result = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    for (i, operand) in inner_rules.iter().skip(1).enumerate() {
                        let right = match self.handle_pair(operand.clone())? {
                            Some(id) => id,
                            None => return Ok(None),
                        };

                        // Extract operator between previous operand and current one
                        let prev_end = inner_rules[i].as_span().end();
                        let curr_start = operand.as_span().start();
                        let operator_text =
                            full_text[prev_end - pair_start..curr_start - pair_start].trim();

                        let op = match operator_text {
                            "+" => Operator::Add,
                            "-" => Operator::Subtract,
                            _ => Operator::Add, // fallback
                        };
                        let node = Node::BinaryOp {
                            left: result,
                            op,
                            right,
                        };
                        result = self.insert_spanned_node(node, span);
                    }
                    Some(result)
                }
            }
            Rule::multiplicative => {
                let full_text = pair.as_str();
                let pair_start = pair.as_span().start();
                let inner_rules: Vec<_> = pair.into_inner().collect();

                if inner_rules.len() == 1 {
                    // Single operand, just forward it
                    match self.handle_pair(inner_rules.into_iter().next().unwrap())? {
                        Some(id) => Some(id),
                        None => None,
                    }
                } else if inner_rules.len() == 2 {
                    // Binary operation: operand1 * operand2 (or / or %)
                    let left = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    let right = match self.handle_pair(inner_rules[1].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };

                    // Extract the operator by looking at the text between operands
                    let left_end = inner_rules[0].as_span().end();
                    let right_start = inner_rules[1].as_span().start();
                    let operator_text =
                        full_text[left_end - pair_start..right_start - pair_start].trim();

                    let op = match operator_text {
                        "*" => Operator::Multiply,
                        "/" => Operator::Divide,
                        "%" => Operator::Modulus,
                        _ => Operator::Multiply, // fallback
                    };
                    let node = Node::BinaryOp { left, op, right };
                    Some(self.insert_spanned_node(node, span))
                } else {
                    // More than 2 operands - chain operations left to right
                    let mut result = match self.handle_pair(inner_rules[0].clone())? {
                        Some(id) => id,
                        None => return Ok(None),
                    };
                    for (i, operand) in inner_rules.iter().skip(1).enumerate() {
                        let right = match self.handle_pair(operand.clone())? {
                            Some(id) => id,
                            None => return Ok(None),
                        };

                        // Extract operator between previous operand and current one
                        let prev_end = inner_rules[i].as_span().end();
                        let curr_start = operand.as_span().start();
                        let operator_text =
                            full_text[prev_end - pair_start..curr_start - pair_start].trim();

                        let op = match operator_text {
                            "*" => Operator::Multiply,
                            "/" => Operator::Divide,
                            "%" => Operator::Modulus,
                            _ => Operator::Multiply, // fallback
                        };
                        let node = Node::BinaryOp {
                            left: result,
                            op,
                            right,
                        };
                        result = self.insert_spanned_node(node, span);
                    }
                    Some(result)
                }
            }
            Rule::unary => {
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::primary => {
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::negate => {
                let mut inner_rules = pair.into_inner();
                let expr = self.require_node(inner_rules.next().unwrap())?;
                let op = Operator::Subtract; // Using Subtract for unary negation
                let node = Node::UnaryOp { op, expr };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::not => {
                let mut inner_rules = pair.into_inner();
                let expr = self.require_node(inner_rules.next().unwrap())?;
                let op = Operator::Not;
                let node = Node::UnaryOp { op, expr };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::type_annotation => {
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::identifier => {
                let name = pair.as_str();
                let node = Node::Identifier(crate::ast::StringId::from_raw(self.strint.get_or_intern(name)));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::r#let => {
                panic!(
                    "Encountered raw 'let' token - this should be handled by let_expr or let_annotated"
                );
            }
            Rule::r#if => {
                panic!("Encountered raw 'if' token - this should be handled by if_expr");
            }
            Rule::r#else => {
                panic!("Encountered raw 'else' token - this should be handled by if_expr");
            }
            Rule::r#while => {
                panic!("Encountered raw 'while' token - this should be handled by while_expr");
            }
            Rule::r#fn => {
                panic!("Encountered raw 'fn' token - this should be handled by function_expr");
            }
            Rule::r#return => {
                panic!("Encountered raw 'return' token - this should be handled by return_expr");
            }
            Rule::r#true => {
                let node = Node::Literal(Literal::Boolean(true));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::r#false => {
                let node = Node::Literal(Literal::Boolean(false));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::bitwise_and => {
                let mut inner = pair.into_inner();
                let a = inner.next().unwrap();
                let left = match self.handle_pair(a)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let Some(b) = inner.next() else {
                    return Ok(Some(left));
                };
                let right = match self.handle_pair(b)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let op = Operator::BitAnd;
                let node = Node::BinaryOp { left, op, right };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::bitwise_or => {
                let mut inner = pair.into_inner();
                let a = inner.next().unwrap();
                let left = match self.handle_pair(a)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let Some(b) = inner.next() else {
                    return Ok(Some(left));
                };
                let right = match self.handle_pair(b)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let op = Operator::BitOr;
                let node = Node::BinaryOp { left, op, right };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::bitwise_xor => {
                let mut inner = pair.into_inner();
                let a = inner.next().unwrap();
                let left = match self.handle_pair(a)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let Some(b) = inner.next() else {
                    return Ok(Some(left));
                };
                let right = match self.handle_pair(b)? {
                    Some(id) => id,
                    None => return Ok(None),
                };
                let op = Operator::BitXor;
                let node = Node::BinaryOp { left, op, right };
                Some(self.insert_spanned_node(node, span))
            }
            Rule::import_expr => {
                let mut inner = pair.into_inner().into_iter();
                let Some(path) = self.handle_pair(inner.nth(1).unwrap())? else {
                    bail!("Expected path in import expression");
                };
                let node = Node::Import(path);
                Some(self.insert_spanned_node(node, span))
            }
            Rule::nil => {
                let node = Node::Literal(Literal::Nil);
                Some(self.insert_spanned_node(node, span))
            }
            Rule::literal => {
                let inner = pair.into_inner().next().unwrap();
                self.handle_pair(inner)?
            }
            Rule::boolean => {
                let value = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!("Invalid boolean value: {}", pair.as_str()),
                };
                let node = Node::Literal(Literal::Boolean(value));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::number => {
                let value = pair.into_inner().next().unwrap();
                self.handle_pair(value)?
            }
            Rule::decimal => {
                let s = pair.as_str();
                let number = s
                    .parse::<f64>()
                    .map_err(|e| ParseError::InvalidDecimalFormat {
                        value: s.to_string(),
                        source: e,
                    })?;
                let node = Node::Literal(Literal::Number(number));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::hex => {
                let s = pair.as_str();
                let significant = &s[2..];
                let str = "0".repeat(16 - significant.len()) + significant;
                let number =
                    u64::from_str_radix(&str, 16).map_err(|e| ParseError::InvalidHexFormat {
                        value: s.to_string(),
                        source: e,
                    })?;
                let node = Node::Literal(Literal::Number(number as f64));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::binary => {
                let s = pair.as_str();
                let significant = &s[2..];
                let str = "0".repeat(64 - significant.len()) + significant;
                let number =
                    u64::from_str_radix(&str, 2).map_err(|e| ParseError::InvalidBinaryFormat {
                        value: s.to_string(),
                        source: e,
                    })?;
                let node = Node::Literal(Literal::Number(number as f64));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::octal => {
                let s = pair.as_str();
                let significant = &s[2..];
                let str = "0".repeat(8 - significant.len()) + significant;
                let number =
                    u64::from_str_radix(&str, 8).map_err(|e| ParseError::InvalidOctalFormat {
                        value: s.to_string(),
                        source: e,
                    })?;
                let node = Node::Literal(Literal::Number(number as f64));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::string => {
                let s = pair.as_str();
                let unescaped = &s[1..s.len() - 1];
                let node = Node::Literal(Literal::String(StringId(self.strint.get_or_intern(unescaped))));
                Some(self.insert_spanned_node(node, span))
            }
            Rule::WHITESPACE => todo!(),
            Rule::SPACES => todo!(),
            Rule::COMMENT => todo!(),
            Rule::r#break => todo!(),
            Rule::import => {
                panic!("Encountered raw 'import' token - this should be handled by import_expr");
            }
            Rule::exponent => {
                let inner_rules: Vec<_> = pair.into_inner().collect();
                if inner_rules.len() == 1 {
                    return self.handle_pair(inner_rules.into_iter().next().unwrap());
                } else {
                    let mut rules = inner_rules.into_iter();
                    let a = self.handle_pair(rules.next().unwrap())?;
                    let b = self.require_node(rules.next().unwrap())?;
                    let op = Operator::Exponent;
                    let node = Node::BinaryOp {
                        left: a.unwrap(),
                        op,
                        right: b,
                    };
                    Some(self.insert_spanned_node(node, span))
                }
            }
            Rule::fstring => {
                let fstring_text = pair.as_str();
                if fstring_text.len() >= 3
                    && fstring_text.starts_with("f\"")
                    && fstring_text.ends_with("\"")
                {
                    let content = &fstring_text[2..fstring_text.len() - 1];
                    let parts = self.parse_fstring_manual(content)?;
                    let fstring_node = Node::Literal(Literal::FString { parts });
                    Some(self.insert_spanned_node(fstring_node, span))
                } else {
                    None
                }
            }
            Rule::array => {
                let inner = pair
                    .into_inner()
                    .map(|p| self.handle_pair(p))
                    .collect::<Vec<_>>();
                if inner.iter().any(|r| r.is_err()) {
                    return Err(inner.into_iter().find_map(|r| r.err()).unwrap());
                }
                let elements: Vec<NodeId> = inner.into_iter().filter_map(|r| r.unwrap()).collect();
                let node = Node::Literal(Literal::Array { elements });
                Some(self.insert_spanned_node(node, span))
            }
            Rule::tuple => {
                let inner = pair
                    .into_inner()
                    .map(|p| self.handle_pair(p))
                    .collect::<Vec<_>>();
                if inner.iter().any(|r| r.is_err()) {
                    return Err(inner.into_iter().find_map(|r| r.err()).unwrap());
                }
                let elements: Vec<NodeId> = inner.into_iter().filter_map(|r| r.unwrap()).collect();
                let node = Node::Literal(Literal::Tuple { elements });
                Some(self.insert_spanned_node(node, span))
            }
            Rule::table => {
                let inner = pair.into_inner();
                let elements: Vec<anyhow::Result<_>> = inner.into_iter().map(|p| self.handle_pair(p)).collect();
                let elements: Vec<NodeId> = elements.into_iter()
                    .map(|r| r.unwrap().unwrap())
                    .collect();
                    
                let mut windows = Vec::new();
                for chunk in elements.chunks(2) {
                    if chunk.len() == 2 {
                        let key = chunk[0].clone();
                        let value = chunk[1].clone();
                        windows.push((key, value));
                    } else {
                        bail!("Table element chunk does not have exactly 2 elements");
                    }
                }
                
                let node = Node::Literal(Literal::Table { elements: windows });
                Some(self.insert_spanned_node(node, span))
            }
            Rule::call_suffix | Rule::index_suffix | Rule::field_suffix => None,
        };
        Ok(r)
    }
    pub fn parse_type_ident(&mut self, str: &str) -> anyhow::Result<TypeIdent> {
        let str = str.trim();
        let mut name = String::new();
        let mut dims = 0;
        let mut open_count = 0;
        for c in str.chars() {
            match c {
                '[' => {
                    open_count += 1;
                }
                ']' => {
                    if open_count == 0 {
                        bail!("Unexpected closing bracket in type identifier");
                    }
                    open_count -= 1;
                    dims += 1;
                }
                _ => {
                    name.push(c);
                }
            }
        }
        if open_count != 0 {
            bail!("Unmatched opening bracket(s) in type identifier");
        }

        Ok(TypeIdent {
            base: crate::ast::StringId::from_raw(self.strint.get_or_intern(&name)),
            dims,
        })
    }
    pub fn parse_fstring_manual(&mut self, content: &str) -> anyhow::Result<Vec<NodeId>> {
        let mut parts = Vec::new();
        let mut current_text = String::new();
        let mut chars = content.chars().peekable();

        let add_text_part = |noder: &mut Self, text: &mut String, parts: &mut Vec<NodeId>| {
            if !text.is_empty() {
                let unescaped = text.replace("{{", "{").replace("}}", "}");
                let string_id = crate::ast::StringId::from_raw(noder.strint.get_or_intern(&unescaped));
                let node = Node::Literal(Literal::String(string_id));
                let span = Span {
                    start: 0,
                    end: text.len(),
                }; // Approximate span
                let node_id = noder.insert_spanned_node(node, span);
                parts.push(node_id);
                text.clear();
            }
        };

        while let Some(ch) = chars.next() {
            match ch {
                '{' => {
                    if chars.peek() == Some(&'{') {
                        // Escaped {{ - add literal { to text
                        chars.next(); // consume second {
                        current_text.push('{');
                    } else {
                        // Start of expression - add any accumulated text first
                        add_text_part(self, &mut current_text, &mut parts);

                        // Parse expression until closing }
                        let mut expr_text = String::new();
                        let mut brace_count = 1;

                        while let Some(expr_ch) = chars.next() {
                            match expr_ch {
                                '{' => {
                                    brace_count += 1;
                                    expr_text.push(expr_ch);
                                }
                                '}' => {
                                    brace_count -= 1;
                                    if brace_count == 0 {
                                        break;
                                    }
                                    expr_text.push(expr_ch);
                                }
                                _ => {
                                    expr_text.push(expr_ch);
                                }
                            }
                        }

                        // Parse the expression text using Pest
                        let trimmed_expr = expr_text.trim();
                        if !trimmed_expr.is_empty() {
                            match UParser::parse(Rule::expr, &trimmed_expr) {
                                Ok(mut pairs) => {
                                    if let Some(pair) = pairs.next() {
                                        if let Some(expr_id) = self.handle_pair(pair)? {
                                            parts.push(expr_id);
                                        }
                                    }
                                }
                                Err(_) => {
                                    // If expression parsing fails, treat it as text
                                    let text = format!("{{{}}}", expr_text);
                                    current_text.push_str(&text);
                                }
                            }
                        }
                    }
                }
                '}' => {
                    if chars.peek() == Some(&'}') {
                        // Escaped }} - add literal } to text
                        chars.next(); // consume second }
                        current_text.push('}');
                    } else {
                        // Unmatched } - treat as regular text
                        current_text.push(ch);
                    }
                }
                _ => {
                    current_text.push(ch);
                }
            }
        }

        // Add any remaining text
        add_text_part(self, &mut current_text, &mut parts);

        Ok(parts)
    }

    pub fn finish(self) -> (NodeReg, StringInt, TypeReg) {
        (self.nodes, self.strint, self.types)
    }
}
