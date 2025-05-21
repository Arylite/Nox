//! AST (Abstract Syntax Tree) module for a simple programming language.
//! This module defines the data structures used to represent the syntax tree of the parsed code.
//! It includes various types of expressions, statements, and the program structure itself.
//! The AST is used for semantic analysis, optimization, and code generation.

use std::fmt;
use std::collections::HashMap;

/// Source location tracking for better error messages
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// Node represents any AST node with location information
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T> {
    pub value: T,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperation,
        right: Box<Expression>,
    },
    UnaryOperation {
        operator: UnaryOperation,
        operand: Box<Expression>,
    },
    ArrayLiteral(Vec<Expression>),
    ObjectLiteral(HashMap<String, Expression>),
    MemberAccess {
        object: Box<Expression>,
        property: String,
    },
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Lambda {
        parameters: Vec<String>,
        body: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),  // Changed from i64 to f64 to support floating point
    String(String),
    Boolean(bool),
    Null,
    Char(char),  // Added support for character literals
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(Box<Type>),
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Custom(String),
    Optional(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Number,
    String,
    Boolean,
    Char,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration {
        name: String,
        type_annotation: Option<Type>,
        value: Expression,
        is_mutable: bool,  // Added to support mut keyword
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<(String, Option<Type>)>,  // Parameter name and optional type
        return_type: Option<Type>,
        body: Vec<Statement>,
    },
    IfStatement {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    WhileStatement {
        condition: Expression,
        body: Vec<Statement>,
    },
    ForStatement {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Vec<Statement>,
    },
    ForEachStatement {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    ReturnStatement {
        value: Option<Expression>,
    },
    Expression(Expression),
    Block(Vec<Statement>),
    BreakStatement,
    ContinueStatement,
    MatchStatement {
        value: Expression,
        arms: Vec<MatchArm>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<(String, Type)>,
    },
    EnumDeclaration {
        name: String,
        variants: Vec<EnumVariant>,
    },
    Assignment {
        target: AssignmentTarget,
        value: Expression,
    },
    TryCatchStatement {
        try_block: Vec<Statement>,
        catch_variable: String,
        catch_block: Vec<Statement>,
        finally_block: Option<Vec<Statement>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Variable(String),
    MemberAccess {
        object: Box<Expression>,
        property: String,
    },
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Variable(String),
    Wildcard,
    Destructure {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    Or(Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<(String, Type)>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: String,
    pub items: Vec<ImportItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportItem {
    Named(String, Option<String>),  // Original name, alias
    Wildcard(String),  // Module alias
}

#[derive(Debug, Clone, PartialEq)]
pub enum Export {
    Named(String),
    Declaration(Box<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub modules: Vec<Module>,
    pub main_module: usize,  // Index of the main module
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    And,
    Or,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    NullishCoalescing,
    OptionalChaining,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperation {
    Negate,
    Not,
    BitwiseNot,
    Increment,  // ++
    Decrement,  // --
    TypeOf,
}

/// Visitor pattern for traversing the AST
pub trait Visitor {
    fn visit_program(&mut self, program: &Program);
    fn visit_module(&mut self, module: &Module);
    fn visit_statement(&mut self, statement: &Statement);
    fn visit_expression(&mut self, expression: &Expression);
    fn visit_literal(&mut self, literal: &Literal);
}

/// Basic implementation of the Visitor pattern
impl<T: Visitor> Visitor for &mut T {
    fn visit_program(&mut self, program: &Program) {
        (**self).visit_program(program);
    }

    fn visit_module(&mut self, module: &Module) {
        (**self).visit_module(module);
    }

    fn visit_statement(&mut self, statement: &Statement) {
        (**self).visit_statement(statement);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        (**self).visit_expression(expression);
    }

    fn visit_literal(&mut self, literal: &Literal) {
        (**self).visit_literal(literal);
    }
}

// Implémentations des méthodes utilitaires

impl Expression {
    /// Checks if this expression is constant and can be evaluated at compile time
    pub fn is_constant(&self) -> bool {
        match self {
            Expression::Literal(_) => true,
            Expression::UnaryOperation { operator, operand } => operand.is_constant(),
            Expression::BinaryOperation { left, right, .. } => left.is_constant() && right.is_constant(),
            Expression::ArrayLiteral(items) => items.iter().all(|item| item.is_constant()),
            Expression::ObjectLiteral(fields) => fields.values().all(|value| value.is_constant()),
            _ => false,
        }
    }

    /// Attempts to infer the type of the expression
    pub fn infer_type(&self, type_context: &HashMap<String, Type>) -> Option<Type> {
        match self {
            Expression::Literal(lit) => match lit {
                Literal::Number(_) => Some(Type::Primitive(PrimitiveType::Number)),
                Literal::String(_) => Some(Type::Primitive(PrimitiveType::String)),
                Literal::Boolean(_) => Some(Type::Primitive(PrimitiveType::Boolean)),
                Literal::Char(_) => Some(Type::Primitive(PrimitiveType::Char)),
                Literal::Null => None,
            },
            Expression::Variable(name) => type_context.get(name).cloned(),
            // Other cases would be implemented here
            _ => None,
        }
    }
}

// Formatter implémentations pour faciliter l'affichage
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
            Literal::Char(c) => write!(f, "'{}'", c),
        }
    }
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::Subtract => "-",
            BinaryOperation::Multiply => "*",
            BinaryOperation::Divide => "/",
            BinaryOperation::Modulus => "%",
            BinaryOperation::And => "&&",
            BinaryOperation::Or => "||",
            BinaryOperation::Equal => "==",
            BinaryOperation::NotEqual => "!=",
            BinaryOperation::GreaterThan => ">",
            BinaryOperation::LessThan => "<",
            BinaryOperation::GreaterThanOrEqual => ">=",
            BinaryOperation::LessThanOrEqual => "<=",
            BinaryOperation::BitwiseAnd => "&",
            BinaryOperation::BitwiseOr => "|",
            BinaryOperation::BitwiseXor => "^",
            BinaryOperation::LeftShift => "<<",
            BinaryOperation::RightShift => ">>",
            BinaryOperation::NullishCoalescing => "??",
            BinaryOperation::OptionalChaining => "?.",
        };
        write!(f, "{}", op_str)
    }
}

// Exemple d'utilisation de l'AST
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast_construction() {
        // 2 + 3 * 4
        let expr = Expression::BinaryOperation {
            left: Box::new(Expression::Literal(Literal::Number(2.0))),
            operator: BinaryOperation::Add,
            right: Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal(Literal::Number(3.0))),
                operator: BinaryOperation::Multiply,
                right: Box::new(Expression::Literal(Literal::Number(4.0))),
            }),
        };

        // Vérification de la constance
        assert!(expr.is_constant());
    }
}