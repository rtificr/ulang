use std::{collections::HashMap, fmt::Display};

use interns::Symbol;
use slab::Slab;
use string_interner::{StringInterner, backend::BucketBackend};

pub type NodeId = usize;
pub type TypeId = Symbol<usize>;
pub type StringId = usize;

/// Represents a character span in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
    
    /// Combine two spans to create a span that covers both
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// A spanned AST node that includes both the node data and its source location
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedNode {
    pub node: Node,
    pub span: Span,
}

impl SpannedNode {
    pub fn new(node: Node, span: Span) -> Self {
        Self { node, span }
    }
    
    /// Delegate to the underlying node's as_identifier method
    pub fn as_identifier(&self) -> Option<StringId> {
        self.node.as_identifier()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Import(NodeId),
    Export(NodeId),
    Literal(Literal),
    Identifier(StringId),
    TypeIdent(TypeIdent),
    FieldAccess { object: NodeId, field: StringId },
    IfExpr {
        condition: NodeId,
        then_branch: NodeId,
        else_branch: Option<NodeId>,
    },
    WhileExpr { condition: NodeId, body: NodeId },
    ForExpr { init: NodeId, condition: NodeId, update: NodeId, body: NodeId },
    Declaration { name: StringId, ann: Option<NodeId>, value: Option<NodeId>, export: bool },
    Assign { name: StringId, node: NodeId },
    Function { params: Vec<FuncParam>, body: NodeId },
    FunctionCall { node: NodeId, args: Vec<NodeId> },
    UnaryOp { op: Operator, expr: NodeId },
    BinaryOp { left: NodeId, op: Operator, right: NodeId },
    Block { statements: Vec<NodeId> },
    IndexAccess { base: NodeId, index: NodeId },
    Reference(NodeId),
    Return(Option<NodeId>),
    Break(Option<NodeId>),
}

impl Node {
    pub fn as_identifier(&self) -> Option<StringId> {
        if let Node::Identifier(name) = self {
            Some(*name)
        } else {
            None
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncParam {
    pub name: StringId,
    pub type_: TypeIdent,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeIdent {
    pub base: StringId,
    pub dims: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    And,
    Or,
    Not,
    BitAnd,
    BitOr,
    BitXor,
    Exponent,
}
impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulus => "%",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
            Operator::GreaterThanOrEqual => ">=",
            Operator::LessThanOrEqual => "<=",
            Operator::And => "&&",
            Operator::Or => "||",
            Operator::Not => "!",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::Exponent => "**",
        };
        write!(f, "{}", op_str)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(StringId),
    FString {
        parts: Vec<NodeId>,
    },
    Boolean(bool),
    Nil,
    Array { elements: Vec<NodeId> },
    Tuple { elements: Vec<NodeId> },
}