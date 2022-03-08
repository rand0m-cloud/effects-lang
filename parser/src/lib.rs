use ast::{
    type_system, BinaryOp, EffectDecl, ExpressionAST, ExpressionOperation, External, FnDecl,
    FnItem, FunctionSignature, ItemDecl,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{all_consuming, cut, map, map_opt, opt, recognize, value, verify},
    error::ParseError,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Compare, CompareResult, Finish, IResult, InputIter, InputLength, InputTake, Needed,
    UnspecializedInput,
};
use std::{
    iter::{Cloned, Enumerate},
    slice::Iter,
};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Value(u64),
    If,
    Else,
    Fn,
    Let,
    Equals,
    Plus,
    Minus,
    Period,
    Effect,
    Handle,
    Extern,
    LessThan,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Colon,
    Comma,
    OpenAngledBracket,
    CloseAngledBracket,
}

impl Token {
    pub fn as_expr(&self) -> Option<ExpressionAST> {
        let op = match self {
            Self::Identifier(name) => ExpressionOperation::Identifier(name.into()),
            Self::Value(value) => ExpressionOperation::Value(*value),
            Self::Plus => ExpressionOperation::BinaryOp(BinaryOp::Add),
            Self::Minus => ExpressionOperation::BinaryOp(BinaryOp::Subtract),

            Self::OpenAngledBracket => ExpressionOperation::BinaryOp(BinaryOp::LessThan),
            _ => return None,
        };
        Some(ExpressionAST::new(op))
    }
    pub fn parse<'a, E: ParseError<TokenStream<'a>>>(
        self,
    ) -> impl FnMut(TokenStream<'a>) -> IResult<TokenStream<'a>, Token, E> {
        move |tokens| tag_token(self.clone())(tokens)
    }
}

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("failed to tokenize input\n{0}")]
    NomStr(
        #[source]
        #[from]
        nom::error::Error<String>,
    ),

    #[error("failed to parse tokens\n{0}")]
    NomTokenStream(
        #[source]
        #[from]
        nom::error::Error<OwnedTokenStream>,
    ),
}

impl<'a> From<nom::error::Error<TokenStream<'a>>> for ParsingError {
    fn from(err: nom::error::Error<TokenStream<'a>>) -> Self {
        Self::NomTokenStream(nom::error::Error {
            input: err.input.into(),
            code: err.code,
        })
    }
}

pub fn tokenize_file(input: &str) -> Result<Vec<Token>, ParsingError> {
    let res = all_consuming(terminated(
        many1(preceded(multispace0, tokenize)),
        multispace0,
    ))(input);
    res.finish()
        .map(|(_remainder, output)| output)
        .map_err(|err| {
            let err = nom::error::Error {
                input: err.input.to_string(),
                code: err.code,
            };
            ParsingError::from(err)
        })
}

fn tokenize(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Let, tag("let")),
        value(Token::Equals, tag("=")),
        value(Token::If, tag("if")),
        value(Token::Else, tag("else")),
        value(Token::Fn, tag("fn")),
        value(Token::Effect, tag("effect")),
        value(Token::Handle, tag("handle")),
        value(Token::Extern, tag("extern")),
        alt((
            value(Token::OpenBrace, tag("{")),
            value(Token::CloseBrace, tag("}")),
            value(Token::OpenParenthesis, tag("(")),
            value(Token::CloseParenthesis, tag(")")),
            value(Token::Plus, tag("+")),
            value(Token::Minus, tag("-")),
            value(Token::Period, tag(".")),
            value(Token::Semicolon, tag(";")),
            value(Token::Colon, tag(":")),
            value(Token::Comma, tag(",")),
            value(Token::CloseAngledBracket, tag(">")),
            value(Token::OpenAngledBracket, tag("<")),
        )),
        map(digit1, |numbers: &str| {
            Token::Value(numbers.parse::<u64>().unwrap())
        }),
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |name: &str| Token::Identifier(name.to_string()),
        ),
    ))(input)
}

#[derive(Debug, Clone)]
pub struct OwnedTokenStream {
    tokens: Vec<Token>,
}

impl std::fmt::Display for OwnedTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self.tokens))
    }
}

impl<'a> From<TokenStream<'a>> for OwnedTokenStream {
    fn from(tokens: TokenStream<'a>) -> Self {
        Self {
            tokens: tokens.tokens.to_vec(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    tokens: &'a [Token],
}

impl<'a> From<&'a [Token]> for TokenStream<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }
}

impl UnspecializedInput for TokenStream<'_> {}

impl<'a> InputIter for TokenStream<'a> {
    type Item = Token;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Cloned<Iter<'a, Token>>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().cloned().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter().cloned()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(|b| predicate(b.clone()))
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.tokens.len()))
        }
    }
}

impl<'a> InputTake for TokenStream<'a> {
    fn take(&self, count: usize) -> Self {
        self.tokens[0..count].into()
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        (suffix.into(), prefix.into())
    }
}

impl<'a> InputLength for TokenStream<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a, O: InputLength + InputIter<Item = Token> + InputTake> Compare<O> for TokenStream<'a> {
    fn compare(&self, t: O) -> CompareResult {
        let pos = self
            .tokens
            .iter()
            .zip(t.iter_elements())
            .position(|(a, b)| *a != b);

        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.tokens.len() >= t.input_len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    fn compare_no_case(&self, t: O) -> CompareResult {
        self.compare(t)
    }
}

pub fn construct_ast(tokens: &[Token]) -> Result<Vec<ItemDecl>, ParsingError> {
    let (_input, expr) = all_consuming(many0(parse_item_decl))(tokens.into()).finish()?;
    Ok(expr)
}

fn tag_token<'a, E: ParseError<TokenStream<'a>>>(
    token: Token,
) -> impl FnMut(TokenStream<'a>) -> IResult<TokenStream<'a>, Token, E> {
    move |tokens| {
        map(
            tag(TokenStream::from(&[token.clone()][..])),
            |tokens: TokenStream| tokens.tokens[0].clone(),
        )(tokens)
    }
}

fn parse_scope(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map(
        delimited(
            Token::OpenBrace.parse(),
            many0(parse_expression),
            Token::CloseBrace.parse(),
        ),
        |exprs| {
            let mut ast = ExpressionAST::new(ExpressionOperation::Scope);
            exprs
                .iter()
                .fold(&mut ast, |ast, expr| ast.with_child(expr));
            ast
        },
    )(tokens)
}

fn parse_identifier(tokens: TokenStream) -> IResult<TokenStream, String> {
    map_opt(take(1usize), |tokens: TokenStream| {
        let expr = tokens.tokens[0].as_expr()?;
        match expr.operation {
            ExpressionOperation::Identifier(name) => Some(name),
            _ => None,
        }
    })(tokens)
}

fn parse_value(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map_opt(take(1usize), |tokens: TokenStream| {
        let expr = tokens.tokens[0].as_expr()?;
        match expr.operation {
            ExpressionOperation::Value(_) => Some(expr),
            _ => None,
        }
    })(tokens)
}

fn parse_let(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map(
        tuple((
            Token::Let.parse(),
            cut(parse_identifier),
            Token::Equals.parse(),
            cut(parse_expression),
        )),
        |(_, identifier, _, expr)| {
            ExpressionAST::new(ExpressionOperation::Let(identifier))
                .with_child(&expr)
                .clone()
        },
    )(tokens)
}

fn parse_if(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map(
        tuple((
            Token::If.parse(),
            parse_expression,
            parse_scope,
            Token::Else.parse(),
            parse_scope,
        )),
        |(_if, cond, true_block, _else, false_block)| {
            ExpressionAST::new(ExpressionOperation::If)
                .with_child(&cond)
                .with_child(&true_block)
                .with_child(&false_block)
                .clone()
        },
    )(tokens)
}

fn parse_binary_op(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map_opt(take(1usize), |tokens: TokenStream| {
        let expr = tokens.tokens[0].as_expr()?;
        match expr.operation {
            ExpressionOperation::BinaryOp(_) => Some(expr),
            _ => None,
        }
    })(tokens)
}

fn parse_type(tokens: TokenStream) -> IResult<TokenStream, type_system::Type> {
    use type_system::Type;
    alt((
        map(
            delimited(
                Token::OpenParenthesis.parse(),
                separated_list0(Token::Comma.parse(), parse_type),
                Token::CloseParenthesis.parse(),
            ),
            |args| {
                if !args.is_empty() {
                    Type::Tuple(args)
                } else {
                    Type::Void
                }
            },
        ),
        value(
            Type::U64,
            verify(parse_identifier, |name: &String| name == "u64"),
        ),
        map(parse_identifier, Type::NamedTyped),
    ))(tokens)
}

fn parse_handle_expression(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    map(
        preceded(
            Token::Handle.parse(),
            cut(pair(
                parse_expression,
                delimited(
                    Token::OpenBrace.parse(),
                    many0(parse_fn_item),
                    Token::CloseBrace.parse(),
                ),
            )),
        ),
        |(expr, interpreters)| {
            ExpressionAST::new(ExpressionOperation::HandleExpression {
                interpreters,
                expr: Box::new(expr),
            })
        },
    )(tokens)
}

fn parse_expression(tokens: TokenStream) -> IResult<TokenStream, ExpressionAST> {
    let (tokens, mut expr) = alt((
        parse_handle_expression,
        parse_scope,
        parse_if,
        parse_let,
        parse_value,
        map(parse_scope_resolution, |name| {
            ExpressionAST::new(ExpressionOperation::Identifier(name))
        }),
    ))(tokens)?;

    if let Ok((tokens, mut op)) = parse_binary_op(tokens.clone()) {
        let (tokens, expr2) = parse_expression(tokens)?;
        return Ok((tokens, op.with_child(&expr).with_child(&expr2).clone()));
    }

    // is function call?
    let tokens = if let ExpressionOperation::Identifier(name) = &mut expr.operation {
        let (tokens, res) = opt(delimited(
            Token::OpenParenthesis.parse(),
            many0(terminated(parse_expression, opt(Token::Comma.parse()))),
            Token::CloseParenthesis.parse(),
        ))(tokens)?;

        if let Some(arguments) = res {
            expr = ExpressionAST::new(ExpressionOperation::FunctionCall(name.clone()));
            for arg in arguments {
                expr.with_child(&arg);
            }
        }
        tokens
    } else {
        tokens
    };

    // discard trailing semicolon
    let (tokens, _) = opt(many1(Token::Semicolon.parse()))(tokens)?;

    Ok((tokens, expr))
}

fn parse_effect_decl(tokens: TokenStream) -> IResult<TokenStream, EffectDecl> {
    map(
        preceded(
            Token::Effect.parse(),
            cut(pair(
                parse_identifier,
                delimited(
                    Token::OpenBrace.parse(),
                    many0(delimited(
                        Token::Fn.parse(),
                        cut(tuple((
                            parse_identifier,
                            delimited(
                                Token::OpenParenthesis.parse(),
                                separated_list0(
                                    Token::Comma.parse(),
                                    separated_pair(
                                        parse_identifier,
                                        Token::Colon.parse(),
                                        parse_type,
                                    ),
                                ),
                                Token::CloseParenthesis.parse(),
                            ),
                            opt(preceded(parse_arrow, parse_type)),
                        ))),
                        Token::Semicolon.parse(),
                    )),
                    Token::CloseBrace.parse(),
                ),
            )),
        ),
        |(name, actions)| EffectDecl {
            name: name.clone(),
            actions: actions
                .into_iter()
                .map(|(action_name, arguments, return_type)| FnDecl {
                    fn_sig: FunctionSignature {
                        external: None,
                        name: action_name,
                        arguments,
                        effects: vec![name.clone()],
                        return_type: return_type.unwrap_or(type_system::Type::Void),
                    },
                })
                .collect(),
        },
    )(tokens)
}

fn parse_scope_resolution(tokens: TokenStream) -> IResult<TokenStream, String> {
    map(
        pair(
            parse_identifier,
            many0(preceded(
                pair(Token::Colon.parse(), Token::Colon.parse()),
                cut(parse_identifier),
            )),
        ),
        |(first, rest)| rest.into_iter().fold(first, |acc, x| acc + "::" + &x),
    )(tokens)
}

fn parse_arrow(tokens: TokenStream) -> IResult<TokenStream, ()> {
    map(
        pair(Token::Minus.parse(), Token::CloseAngledBracket.parse()),
        |_| (),
    )(tokens)
}

fn parse_function_signature(tokens: TokenStream) -> IResult<TokenStream, FunctionSignature> {
    let argument = pair(parse_identifier, preceded(Token::Colon.parse(), parse_type));
    map(
        tuple((
            opt(Token::Extern.parse()),
            Token::Fn.parse(),
            cut(tuple((
                parse_scope_resolution,
                delimited(
                    Token::OpenParenthesis.parse(),
                    separated_list0(Token::Comma.parse(), argument),
                    Token::CloseParenthesis.parse(),
                ),
                opt(preceded(
                    Token::Colon.parse(),
                    separated_list1(Token::Comma.parse(), parse_identifier),
                )),
                opt(preceded(parse_arrow, parse_type)),
            ))),
        )),
        |(external, _, (name, arguments, effects, return_type))| FunctionSignature {
            external: external.map(|_| External),
            name,
            arguments,
            effects: effects.unwrap_or_default(),
            return_type: return_type.unwrap_or(type_system::Type::Void),
        },
    )(tokens)
}
fn parse_fn_item(tokens: TokenStream) -> IResult<TokenStream, FnItem> {
    map(
        pair(parse_function_signature, parse_scope),
        |(signature, body)| signature.into_fn_item(&body),
    )(tokens)
}

fn parse_fn_decl(tokens: TokenStream) -> IResult<TokenStream, FnDecl> {
    map(
        terminated(parse_function_signature, Token::Semicolon.parse()),
        |fn_sig| FnDecl { fn_sig },
    )(tokens)
}

fn parse_item_decl(tokens: TokenStream) -> IResult<TokenStream, ItemDecl> {
    alt((
        map(parse_effect_decl, ItemDecl::EffectDecl),
        map(parse_fn_item, ItemDecl::FnItem),
        map(parse_fn_decl, ItemDecl::FnDecl),
    ))(tokens)
}
