use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Return(Some(expr)) => write!(f, "return {};", expr),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Infix(Infix, Box<Expression>, Box<Expression>),
    Prefix(Prefix, Box<Expression>),
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Empty,
    FunctionLiteral(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Infix(op, l, r) => write!(f, "({} {} {})", l, op, r),
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::FloatLiteral(float) => write!(f, "{}", float),
            Expression::Empty => write!(f, ""),
            Expression::Prefix(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expression::BooleanLiteral(bool) => write!(f, "{}", bool),
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral(parameters, body) => {
                write!(f, "function({}) {}", parameters.join(", "), body)
            }
            Expression::Call(function, arguments) => {
                write!(f, "{}({})", function, {
                    arguments
                        .iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                })
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{{ {} }}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Eq,
    NotEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Eq => write!(f, "=="),
            Infix::NotEq => write!(f, "!="),
            Infix::Lt => write!(f, "<"),
            Infix::Gt => write!(f, ">"),
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Bang,
    Minus,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Bang => write!(f, "!"),
            Prefix::Minus => write!(f, "-"),
        }
    }
}