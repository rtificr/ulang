use std::fmt::Display;

use interns::Symbol;
// slab and string-interner imports removed; AST uses the raw alias types via macro

/// Small helper macro to implement conversions and formatting for simple newtypes
macro_rules! impl_id {
    // Optionally allow a third identifier which will become a raw type alias
    ($name:ident, $raw:ty $(, $raw_alias:ident)?) => {
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        pub struct $name(pub $raw);

        impl $name {
            /// Construct the newtype from the raw underlying value.
            #[inline]
            pub fn from_raw(k: $raw) -> Self {
                $name(k)
            }
            /// Return the underlying raw representation for interop with
            /// external crates (e.g. slab, slotmap, interners) that expect
            /// the primitive symbol/index type.
            #[inline]
            pub fn raw(self) -> $raw {
                self.0
            }
        }
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.0)
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self.0)
            }
        }
        $(
            /// Raw type alias for the underlying primitive used by `$name`.
            pub type $raw_alias = $raw;
        )?
    };
}

impl_id!(NodeId, usize, NodeIdRaw);
impl_id!(StringId, usize, StringIdRaw);
impl_id!(TypeId, Symbol<usize>, TypeIdRaw);

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
    FieldAccess { object: NodeId, field: StringId },
    IfExpr {
        condition: NodeId,
        then_branch: NodeId,
        else_branch: Option<NodeId>,
    },
    WhileExpr { condition: NodeId, body: NodeId },
    ForExpr { init: NodeId, condition: NodeId, update: NodeId, body: NodeId },
    Declaration { name: StringId, ann: Option<NodeId>, value: Option<NodeId>, export: bool },
    Assign { target: NodeId, node: NodeId },
    Function { params: Vec<FuncParam>, body: NodeId },
    FunctionCall { node: NodeId, args: Vec<NodeId> },
    UnaryOp { op: Operator, expr: NodeId },
    OpAssign { left: NodeId, op: Operator, right: NodeId },
    BinaryOp { left: NodeId, op: Operator, right: NodeId },
    Block { statements: Vec<NodeId> },
    IndexAccess { base: NodeId, index: NodeId },
    Inc(NodeId),
    Dec(NodeId),
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
    pub type_: Option<NodeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeIdent {
    pub base: StringId,
    pub dims: u8,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
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
    // Compound assignment operators
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
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
            Operator::PreInc | Operator::PostInc => "++",
            Operator::PreDec | Operator::PostDec => "--",
            Operator::AddAssign => "+=",
            Operator::SubAssign => "-=",
            Operator::MulAssign => "*=",
            Operator::DivAssign => "/=",
            Operator::ModAssign => "%=",
            Operator::BitAndAssign => "&=",
            Operator::BitOrAssign => "|=",
            Operator::BitXorAssign => "^=",
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
    Table {
        elements: Vec<(NodeId, NodeId)>
    },
    Object {
        elements: Vec<(StringId, NodeId)>
    }
}
