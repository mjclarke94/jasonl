//! Boolean filter expression parser with AND/OR support
//!
//! Syntax examples:
//! - `score > 90`
//! - `is_good = true AND score > 80`
//! - `(is_good = true AND score > 80) OR is_fallback = true`
//! - `NOT is_bad = true`
//! - `tag:important AND score >= 90`
//! - `has:notes OR has:tags`

use crate::data::Conversation;
use crate::db::FileData;

/// A parsed filter expression (AST node)
#[derive(Debug, Clone)]
pub enum FilterExpr {
    /// A simple comparison: field op value
    Comparison {
        field: String,
        op: CompareOp,
        value: Value,
    },
    /// Special filter (tags/notes)
    Special(SpecialFilter),
    /// Logical AND of two expressions
    And(Box<FilterExpr>, Box<FilterExpr>),
    /// Logical OR of two expressions
    Or(Box<FilterExpr>, Box<FilterExpr>),
    /// Logical NOT of an expression
    Not(Box<FilterExpr>),
    /// Always true (empty filter)
    True,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompareOp {
    Gt,  // >
    Lt,  // <
    Gte, // >=
    Lte, // <=
    Eq,  // =
    Neq, // !=
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum SpecialFilter {
    HasTag(String),
    HasAnyTag,
    HasNote,
    NoTags,
    NoNote,
}

impl FilterExpr {
    /// Parse a filter expression string
    pub fn parse(input: &str) -> Result<Self, String> {
        let input = input.trim();
        if input.is_empty() {
            return Ok(FilterExpr::True);
        }

        let tokens = tokenize(input)?;
        if tokens.is_empty() {
            return Ok(FilterExpr::True);
        }

        let mut parser = Parser::new(&tokens);
        let expr = parser.parse_or()?;

        if parser.pos < tokens.len() {
            return Err(format!(
                "Unexpected token: {:?}",
                tokens[parser.pos]
            ));
        }

        Ok(expr)
    }

    /// Evaluate the expression against a conversation
    pub fn matches(&self, conv: &Conversation, file_data: Option<&FileData>) -> bool {
        match self {
            FilterExpr::True => true,
            FilterExpr::Comparison { field, op, value } => {
                match_comparison(conv, field, *op, value)
            }
            FilterExpr::Special(special) => match_special(conv, special, file_data),
            FilterExpr::And(left, right) => {
                left.matches(conv, file_data) && right.matches(conv, file_data)
            }
            FilterExpr::Or(left, right) => {
                left.matches(conv, file_data) || right.matches(conv, file_data)
            }
            FilterExpr::Not(inner) => !inner.matches(conv, file_data),
        }
    }
}

fn match_comparison(conv: &Conversation, field: &str, op: CompareOp, value: &Value) -> bool {
    let Some(field_value) = conv.metadata.fields.get(field) else {
        return false;
    };

    match value {
        Value::Number(num) => {
            let Ok(field_num) = field_value.parse::<f64>() else {
                return false;
            };
            match op {
                CompareOp::Gt => field_num > *num,
                CompareOp::Lt => field_num < *num,
                CompareOp::Gte => field_num >= *num,
                CompareOp::Lte => field_num <= *num,
                CompareOp::Eq => (field_num - num).abs() < f64::EPSILON,
                CompareOp::Neq => (field_num - num).abs() >= f64::EPSILON,
            }
        }
        Value::Bool(b) => {
            let field_bool = match field_value.to_lowercase().as_str() {
                "true" => true,
                "false" => false,
                _ => return false,
            };
            match op {
                CompareOp::Eq => field_bool == *b,
                CompareOp::Neq => field_bool != *b,
                _ => false,
            }
        }
        Value::String(s) => {
            match op {
                CompareOp::Eq => field_value == s,
                CompareOp::Neq => field_value != s,
                _ => false,
            }
        }
    }
}

fn match_special(conv: &Conversation, special: &SpecialFilter, file_data: Option<&FileData>) -> bool {
    let Some(fd) = file_data else {
        // No file data means no tags/notes
        return matches!(special, SpecialFilter::NoTags | SpecialFilter::NoNote);
    };

    match special {
        SpecialFilter::HasAnyTag => fd.has_tags(conv.source_line),
        SpecialFilter::HasNote => fd.has_note(conv.source_line),
        SpecialFilter::NoTags => !fd.has_tags(conv.source_line),
        SpecialFilter::NoNote => !fd.has_note(conv.source_line),
        SpecialFilter::HasTag(name) => {
            fd.get_tags(conv.source_line)
                .iter()
                .any(|t| t.name.to_lowercase() == *name)
        }
    }
}

// ============ Tokenizer ============

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    And,
    Or,
    Not,
    Ident(String),
    Op(CompareOp),
    Number(f64),
    Bool(bool),
    String(String),
    // Special filter tokens
    HasTag(String),
    HasTags,
    HasNotes,
    NoTags,
    NoNotes,
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' => {
                chars.next();
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '>' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Op(CompareOp::Gte));
                } else {
                    tokens.push(Token::Op(CompareOp::Gt));
                }
            }
            '<' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Op(CompareOp::Lte));
                } else {
                    tokens.push(Token::Op(CompareOp::Lt));
                }
            }
            '=' => {
                chars.next();
                tokens.push(Token::Op(CompareOp::Eq));
            }
            '!' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Op(CompareOp::Neq));
                } else {
                    // Standalone ! is NOT
                    tokens.push(Token::Not);
                }
            }
            '0'..='9' => {
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if let Ok(num) = num_str.parse::<f64>() {
                    tokens.push(Token::Number(num));
                } else {
                    return Err(format!("Invalid number: {}", num_str));
                }
            }
            '-' => {
                // Check if this is a negative number (followed by digit or dot+digit)
                let mut lookahead = chars.clone();
                lookahead.next(); // skip the '-'
                let is_number = match lookahead.peek() {
                    Some('0'..='9') => true,
                    Some('.') => {
                        lookahead.next();
                        matches!(lookahead.peek(), Some('0'..='9'))
                    }
                    _ => false,
                };
                if is_number {
                    let mut num_str = String::from("-");
                    chars.next(); // consume the '-'
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() || c == '.' {
                            num_str.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if let Ok(num) = num_str.parse::<f64>() {
                        tokens.push(Token::Number(num));
                    } else {
                        return Err(format!("Invalid number: {}", num_str));
                    }
                } else {
                    return Err(format!("Unexpected character: {}", c));
                }
            }
            '.' => {
                // Check if this is a decimal number starting with dot (e.g., .5)
                let mut lookahead = chars.clone();
                lookahead.next(); // skip the '.'
                if matches!(lookahead.peek(), Some('0'..='9')) {
                    let mut num_str = String::from(".");
                    chars.next(); // consume the '.'
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() {
                            num_str.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if let Ok(num) = num_str.parse::<f64>() {
                        tokens.push(Token::Number(num));
                    } else {
                        return Err(format!("Invalid number: {}", num_str));
                    }
                } else {
                    return Err(format!("Unexpected character: {}", c));
                }
            }
            '"' | '\'' => {
                let quote = c;
                chars.next();
                let mut s = String::new();
                while let Some(&c) = chars.peek() {
                    if c == quote {
                        chars.next();
                        break;
                    }
                    s.push(c);
                    chars.next();
                }
                tokens.push(Token::String(s));
            }
            _ if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' || c == '.' || c == ':' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                // Check for keywords and special filters
                let lower = ident.to_lowercase();
                if lower == "and" || lower == "&&" {
                    tokens.push(Token::And);
                } else if lower == "or" || lower == "||" {
                    tokens.push(Token::Or);
                } else if lower == "not" {
                    tokens.push(Token::Not);
                } else if lower == "true" {
                    tokens.push(Token::Bool(true));
                } else if lower == "false" {
                    tokens.push(Token::Bool(false));
                } else if let Some(tag_name) = lower.strip_prefix("tag:") {
                    tokens.push(Token::HasTag(tag_name.to_string()));
                } else if lower == "has:tags" || lower == "has:tag" {
                    tokens.push(Token::HasTags);
                } else if lower == "has:notes" || lower == "has:note" {
                    tokens.push(Token::HasNotes);
                } else if lower == "no:tags" || lower == "no:tag" {
                    tokens.push(Token::NoTags);
                } else if lower == "no:notes" || lower == "no:note" {
                    tokens.push(Token::NoNotes);
                } else {
                    tokens.push(Token::Ident(ident));
                }
            }
            '&' => {
                chars.next();
                if chars.peek() == Some(&'&') {
                    chars.next();
                }
                tokens.push(Token::And);
            }
            '|' => {
                chars.next();
                if chars.peek() == Some(&'|') {
                    chars.next();
                }
                tokens.push(Token::Or);
            }
            ',' => {
                // Comma is treated as AND for backwards compatibility
                chars.next();
                tokens.push(Token::And);
            }
            _ => {
                return Err(format!("Unexpected character: {}", c));
            }
        }
    }

    Ok(tokens)
}

// ============ Parser ============

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.advance() {
            Some(t) if t == expected => Ok(()),
            Some(t) => Err(format!("Expected {:?}, got {:?}", expected, t)),
            None => Err(format!("Expected {:?}, got end of input", expected)),
        }
    }

    /// Parse OR expression (lowest precedence)
    fn parse_or(&mut self) -> Result<FilterExpr, String> {
        let mut left = self.parse_and()?;

        while self.peek() == Some(&Token::Or) {
            self.advance();
            let right = self.parse_and()?;
            left = FilterExpr::Or(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse AND expression
    fn parse_and(&mut self) -> Result<FilterExpr, String> {
        let mut left = self.parse_not()?;

        while self.peek() == Some(&Token::And) {
            self.advance();
            let right = self.parse_not()?;
            left = FilterExpr::And(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse NOT expression
    fn parse_not(&mut self) -> Result<FilterExpr, String> {
        if self.peek() == Some(&Token::Not) {
            self.advance();
            let inner = self.parse_not()?;
            Ok(FilterExpr::Not(Box::new(inner)))
        } else {
            self.parse_atom()
        }
    }

    /// Parse atomic expression (comparison, special filter, or parenthesized expression)
    fn parse_atom(&mut self) -> Result<FilterExpr, String> {
        match self.peek() {
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_or()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Some(Token::HasTag(name)) => {
                let name = name.clone();
                self.advance();
                Ok(FilterExpr::Special(SpecialFilter::HasTag(name)))
            }
            Some(Token::HasTags) => {
                self.advance();
                Ok(FilterExpr::Special(SpecialFilter::HasAnyTag))
            }
            Some(Token::HasNotes) => {
                self.advance();
                Ok(FilterExpr::Special(SpecialFilter::HasNote))
            }
            Some(Token::NoTags) => {
                self.advance();
                Ok(FilterExpr::Special(SpecialFilter::NoTags))
            }
            Some(Token::NoNotes) => {
                self.advance();
                Ok(FilterExpr::Special(SpecialFilter::NoNote))
            }
            Some(Token::Ident(_)) => {
                self.parse_comparison()
            }
            Some(t) => Err(format!("Unexpected token: {:?}", t)),
            None => Err("Unexpected end of expression".to_string()),
        }
    }

    /// Parse a comparison: field op value
    fn parse_comparison(&mut self) -> Result<FilterExpr, String> {
        let field = match self.advance() {
            Some(Token::Ident(s)) => s.clone(),
            _ => return Err("Expected field name".to_string()),
        };

        let op = match self.advance() {
            Some(Token::Op(op)) => *op,
            _ => return Err("Expected comparison operator".to_string()),
        };

        let value = match self.advance() {
            Some(Token::Number(n)) => Value::Number(*n),
            Some(Token::Bool(b)) => Value::Bool(*b),
            Some(Token::String(s)) => Value::String(s.clone()),
            Some(Token::Ident(s)) => {
                // Try to parse as number, otherwise treat as string
                if let Ok(n) = s.parse::<f64>() {
                    Value::Number(n)
                } else {
                    Value::String(s.clone())
                }
            }
            _ => return Err("Expected value".to_string()),
        };

        Ok(FilterExpr::Comparison { field, op, value })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_comparison() {
        let expr = FilterExpr::parse("score > 90").unwrap();
        assert!(matches!(expr, FilterExpr::Comparison { .. }));
    }

    #[test]
    fn test_and_expression() {
        let expr = FilterExpr::parse("score > 90 AND is_good = true").unwrap();
        assert!(matches!(expr, FilterExpr::And(_, _)));
    }

    #[test]
    fn test_or_expression() {
        let expr = FilterExpr::parse("score > 90 OR is_fallback = true").unwrap();
        assert!(matches!(expr, FilterExpr::Or(_, _)));
    }

    #[test]
    fn test_parentheses() {
        let expr = FilterExpr::parse("(score > 90 AND x = 1) OR y = 2").unwrap();
        assert!(matches!(expr, FilterExpr::Or(_, _)));
    }

    #[test]
    fn test_not_expression() {
        let expr = FilterExpr::parse("NOT is_bad = true").unwrap();
        assert!(matches!(expr, FilterExpr::Not(_)));
    }

    #[test]
    fn test_complex_expression() {
        let expr = FilterExpr::parse("(is_X = true AND y.foo > 80) OR is_X = false").unwrap();
        assert!(matches!(expr, FilterExpr::Or(_, _)));
    }

    #[test]
    fn test_comma_as_and() {
        let expr = FilterExpr::parse("score > 90, other < 50").unwrap();
        assert!(matches!(expr, FilterExpr::And(_, _)));
    }

    #[test]
    fn test_special_filters() {
        assert!(matches!(
            FilterExpr::parse("has:tags").unwrap(),
            FilterExpr::Special(SpecialFilter::HasAnyTag)
        ));
        assert!(matches!(
            FilterExpr::parse("tag:important").unwrap(),
            FilterExpr::Special(SpecialFilter::HasTag(_))
        ));
    }

    #[test]
    fn test_empty_filter() {
        let expr = FilterExpr::parse("").unwrap();
        assert!(matches!(expr, FilterExpr::True));
    }

    #[test]
    fn test_negative_numbers() {
        let expr = FilterExpr::parse("score > -10").unwrap();
        if let FilterExpr::Comparison { value: Value::Number(n), .. } = expr {
            assert_eq!(n, -10.0);
        } else {
            panic!("Expected numeric comparison");
        }

        // Decimal starting with dot
        let expr = FilterExpr::parse("score > .5").unwrap();
        if let FilterExpr::Comparison { value: Value::Number(n), .. } = expr {
            assert_eq!(n, 0.5);
        } else {
            panic!("Expected numeric comparison");
        }
    }

    #[test]
    fn test_standalone_dash_dot_rejected() {
        // Standalone dash should error, not be parsed as a number
        assert!(FilterExpr::parse("score > -").is_err());
        // Standalone dot should error
        assert!(FilterExpr::parse("score > .").is_err());
        // Dash not followed by digit should error
        assert!(FilterExpr::parse("score > -abc").is_err());
    }
}
