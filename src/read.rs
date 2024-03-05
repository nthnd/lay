use anyhow::Result;
use pest::Parser;
use pest_derive::Parser;

use crate::eval::Expr;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Grammar;


pub(crate) fn read(input: &str) -> Result<Vec<Expr>> {
    let ast = Grammar::parse(Rule::input, input)?;

    if std::env::var("DEBUG_LAY").is_ok() {
        pest_ascii_tree::print_ascii_tree(Ok(ast.clone()));
    }

    #[rustfmt::skip]
    fn parse_value(expr: pest::iterators::Pair<'_, Rule>) -> Expr {
        match expr.as_rule() {
            Rule::nil => Expr::Nil,
            Rule::bool => Expr::Bool(expr.as_str().parse().unwrap()),
            Rule::number => Expr::Number(expr.as_str().parse().unwrap()),
            Rule::symbol => Expr::Symbol(expr.as_str().to_string()),
            Rule::string => Expr::String(expr.as_str().trim_start_matches("\"").trim_end_matches("\"").to_string()),
            Rule::list => Expr::List(expr.into_inner().map(parse_value).collect()),
            Rule::EOI
                | Rule::WHITESPACE
                | Rule::inner
                | Rule::symbol_chars | Rule::input
                | Rule::char
                | Rule::expr
                | Rule::COMMENT => 
                unreachable!("{expr:?}"),
        }
    }

    let without_eoi = ast.len() - 1;
    let exprs = ast.take(without_eoi);
    Ok(exprs.map(parse_value).collect())
}
