use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    Mismatch { expected: String, found: String },
}
impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::Mismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
        }
    }
}
impl std::error::Error for TypeError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    MissingTypeAnnotation { ident: String },
    InvalidAssignmentTarget,
    InvalidDecimalFormat { value: String, source: std::num::ParseFloatError },
    InvalidBinaryFormat { value: String, source: std::num::ParseIntError },
    InvalidHexFormat { value: String, source: std::num::ParseIntError },
    InvalidOctalFormat { value: String, source: std::num::ParseIntError },
    InvalidFStringFormat { value: String },
    EmptyFStringExpression,
    EmptyFunctionBody,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MissingTypeAnnotation { ident } => write!(f, "Missing type annotation for identifier {}", ident),
            ParseError::EmptyFunctionBody => write!(f, "Function body is empty"),
            ParseError::InvalidAssignmentTarget => write!(f, "Invalid assignment target"),
            ParseError::InvalidDecimalFormat { value, source } => {
                write!(f, "Invalid decimal format '{}': {}", value, source)
            }
            ParseError::InvalidBinaryFormat { value, source } => {
                write!(f, "Invalid binary format '{}': {}", value, source)
            }
            ParseError::InvalidHexFormat { value, source } => {
                write!(f, "Invalid hex format '{}': {}", value, source)
            }
            ParseError::InvalidOctalFormat { value, source } => {
                write!(f, "Invalid octal format '{}': {}", value, source)
            }
            ParseError::InvalidFStringFormat { value } => {
                write!(f, "Invalid f-string format '{}'", value)
            }
            ParseError::EmptyFStringExpression => {
                write!(f, "Empty expression in f-string")
            }
        }
    }
}
impl std::error::Error for ParseError {}