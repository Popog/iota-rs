#![crate_type="dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc;

use syntax::ast::{Expr, Ident, LitInt, Sign, TokenTree, Ty, UnsuffixedIntLit, Visibility};
use syntax::codemap::{mk_sp, Span, Spanned};
use syntax::diagnostic::FatalError;
use syntax::ext::base::{ExtCtxt, DummyResult, MacEager, MacResult, SyntaxExtension, TTMacroExpander};
use syntax::ext::build::AstBuilder;  // trait for expr_lit
use syntax::fold::Folder;
use syntax::parse::{PResult};
use syntax::parse::common::{SeqSep};
use syntax::parse::parser::{Parser};
use syntax::parse::token::{Token, DelimToken};
use syntax::parse::token::keywords::Keyword;
use syntax::ptr::{P};
use syntax::util::small_vector::SmallVector;
use rustc::plugin::Registry;

// Some helpers to improve readability
const OPEN_PAREN: Token = Token::OpenDelim(DelimToken::Paren);
const CLOSE_PAREN: Token = Token::CloseDelim(DelimToken::Paren);
const COMMA_SEP: SeqSep = SeqSep{sep: Some(Token::Comma), trailing_sep_allowed: true};

// The macro object for expanding each line
struct Iota(u64);

impl TTMacroExpander for Iota {
	fn expand<'cx>(&self, cx: &'cx mut ExtCtxt, sp: Span, token_tree: &[TokenTree]) -> Box<MacResult + 'cx> {
		if token_tree.len() > 0 {
			cx.span_err(sp, "unexpected tokens");
			return DummyResult::any(sp);
		}
		MacEager::expr(cx.expr_lit(sp, LitInt(self.0, UnsuffixedIntLit(Sign::Plus))))
	}
}

fn parse_ident_or_underscore(p: &mut Parser) -> PResult<Option<Ident>> {
	if try!(p.eat(&Token::Underscore)) {
		Ok(None)
	} else {
		Ok(Some(try!(p.parse_ident())))
	}
}

// A single line of consts.
struct IotaItem {
	span: Span,
	idents: Spanned<Vec<Option<Ident>>>,
	types: Spanned<Vec<P<Ty>>>,
	exprs: Spanned<Vec<P<Expr>>>,
	prev: bool,
	tuple: bool,
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
	reg.register_macro("consts", expand_const);
}

fn parse_tuple_items(parser: &mut Parser, prev: Option<IotaItem>) -> PResult<IotaItem> {
	let lo = parser.span.lo;

	// Parse the identifiers
	let idents = try!(parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, parse_ident_or_underscore));

	// Check if we need to use a previous line's types/expressions
	let (types, exprs, prev) = match (prev, parser.check(&Token::Semi)) {
		(Some(x), true) => {
			// Make sure the previous line was a tuple
			if !x.tuple {
				parser.span_err(idents.span, "found tuple, expected identifier");
				return Err(FatalError);
			}
			(x.types, x.exprs, true)
		}
		_ => {
			try!(parser.expect(&Token::Colon));

			// Parse the types
			let types = try!(parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, |p| p.parse_ty_nopanic()));

			try!(parser.expect(&Token::Eq));

			// Parse the expressions
			let exprs = try!(parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, |p| p.parse_expr_nopanic()));

			(types, exprs, false)
		}
	};

	try!(parser.expect(&Token::Semi));

	let span = mk_sp(lo, parser.span.hi);

	Ok(IotaItem{span: span, idents: idents, types: types, exprs: exprs, prev: prev, tuple: true})
}

fn parse_ident_item(parser: &mut Parser, prev: Option<IotaItem>) -> PResult<IotaItem> {
	let lo = parser.span.lo;

	// Parse the identifier
	let ident = try!(parse_ident_or_underscore(parser));
	let ispan = mk_sp(lo, parser.span.hi);

	// Check if we need to use a previous line's types/expressions
	let (types, exprs, prev) = match (prev, parser.check(&Token::Semi)) {
		(Some(x), true) => {
			// Make sure the previous line wasn't a tuple
			if x.tuple {
				parser.span_err(ispan, "found identifier, expected tuple");
				return Err(FatalError);
			}
			(x.types, x.exprs, true)
		}
		_ => {
			try!(parser.expect(&Token::Colon));

			// Parse the type
			let type_ = try!(parser.parse_ty_nopanic());

			try!(parser.expect(&Token::Eq));

			// Parse the expression
			let expr = try!(parser.parse_expr_nopanic());

			let (tspan, espan) = (type_.span, expr.span);
			(Spanned{node: vec![type_], span: tspan}, Spanned{node: vec![expr], span: espan}, false)
		},
	};

	try!(parser.expect(&Token::Semi));

	let span = mk_sp(lo, parser.span.hi);
	let idents = Spanned{node: vec![ident], span: ispan};

	Ok(IotaItem{span: span, idents: idents, types: types, exprs: exprs, prev: prev, tuple: false})
}

fn expand_const(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
	let name = cx.name_of("iota");

	let mut items = vec![];

	let mut parser = cx.new_parser_from_tts(args);
	let mut prev = None;
	for iota in (0u64..) {
		// Check for a visibility qualifier
		let vis = if parser.token.is_keyword(Keyword::Pub) {
				if let Err(_) = parser.bump() { return DummyResult::any(sp); }
				Visibility::Public
			} else { Visibility::Inherited };

		// Parse the current line
		let current = match parser.token {
			OPEN_PAREN => parse_tuple_items(&mut parser, prev),
			Token::Ident(_,_) => parse_ident_item(&mut parser, prev),
			Token::Eof => { break; },
			_ => {
				let token_str = Parser::token_to_string(&parser.token);
				cx.span_err(sp, &format!("expected identifier or `(`, found `{}`", token_str));
				return DummyResult::any(sp);
			},
		};

		// Bail in case of fatal errors
		let current = match current {
			Err(_) => { return DummyResult::any(sp); },
			Ok(v) => v,
		};

		// Check to make sure # of idents == # of types == # of exprs
		match (current.idents.node.len(), current.types.node.len(), current.exprs.node.len(), current.prev) {
			(_, t, e, _) if t == 0 || e == 0 => {
				if t == 0 {
					cx.span_err(current.types.span, "no types found");
				}
				if e == 0 {
					cx.span_err(current.exprs.span, "no expressions found");
				}
				return DummyResult::any(sp);
			},
			(_, t, e, _) if e != t => {
				cx.span_err(current.exprs.span, &format!("expected {} expressions, found {}", t, e));
				return DummyResult::any(sp);
			},
			(0, _, _, _) => {
				cx.span_err(current.idents.span, "no identifiers found");
				prev = Some(current);
				continue;
			},
			(i, t, _, false) if i != t => {
				cx.span_err(current.types.span, &format!("expected {} types, found {}", i, t));
				prev = Some(current);
				continue;
			},
			(i, t, _, true) if i != t => {
				cx.span_err(current.span, &format!("expected {} identifiers, found {}", t, i));
				prev = Some(current);
				continue;
			},
			_ => (),
		}

		// Expand iota!()
		cx.syntax_env.push_frame();
		cx.syntax_env.insert(name, SyntaxExtension::NormalTT(Box::new(Iota(iota)), Some(sp), false));
		let expanded_expr: Vec<_> = {
			let mut exp = cx.expander();
		 	current.exprs.node.iter().map(|expr| exp.fold_expr(expr.clone())).collect()
		};
		cx.syntax_env.pop_frame();

		// Generate all the const items
		for ((ident, type_), expr) in current.idents.node.iter().zip(current.types.node.iter()).zip(expanded_expr) {
			let ident = if let Some(x) = *ident { x } else { continue; };

			let item = cx.item_const(current.span, ident.clone(), type_.clone(), expr)
				.map(|mut item| { item.vis = vis; item });
			items.push(item);
		}

		prev = Some(current);
	}

	MacEager::items(SmallVector::many(items))
}
