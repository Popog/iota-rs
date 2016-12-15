#![crate_type="dylib"]
#![feature(plugin_registrar, rustc_private, alloc)]

extern crate alloc;
extern crate syntax;
extern crate rustc_plugin;

use alloc::rc::Rc;
use syntax::ast::{Expr, Name, Ident, LitKind, Ty, LitIntType, Visibility, NodeId, Item, MacroDef, Attribute, Path};
use syntax::tokenstream::TokenTree;
use syntax::codemap::{mk_sp, Span, Spanned};
use syntax::ext::base::{ExtCtxt, DummyResult, MacEager, MacResult, SyntaxExtension, TTMacroExpander, Resolver, Determinacy};
use syntax::ext::build::AstBuilder;  // trait for expr_lit
use syntax::ext::hygiene::Mark;
use syntax::ext::expand::{Expansion,ExpansionConfig};
use syntax::fold::Folder;
use syntax::parse::PResult;
use syntax::parse::common::SeqSep;
use syntax::parse::parser::Parser;
use syntax::parse::token::{Token, DelimToken};
use syntax::symbol::keywords::Pub;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use rustc_plugin::Registry;

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
		MacEager::expr(cx.expr_lit(sp, LitKind::Int(self.0, LitIntType::Unsuffixed)))
	}
}

enum Lazy<V, F> {
	Poison,
	Lazy(F),
	Value(V),
}

impl<V, F: FnOnce() -> V> Lazy<V, F> {
	fn get(&mut self) -> &mut V {
		use std::mem::replace;
		*self = Lazy::Value(match replace(self, Lazy::Poison) {
			Lazy::Poison => panic!("poisoned"),
			Lazy::Lazy(f) => f(),
			Lazy::Value(v) => v,
		});
		if let Lazy::Value(ref mut v) = *self { v } else { unreachable!() }
	}
}

struct IotaResolver<'a, F>{
	name: Name,
	iota: Lazy<Rc<SyntaxExtension>, F>,
	resolver: &'a mut Resolver,
}

impl<'a, F: FnOnce() -> Rc<SyntaxExtension>> Resolver for IotaResolver<'a, F> {
	fn next_node_id(&mut self) -> NodeId { self.resolver.next_node_id() }
	fn get_module_scope(&mut self, id: NodeId) -> Mark { self.resolver.get_module_scope(id) }
	fn eliminate_crate_var(&mut self, item: P<Item>) -> P<Item> { self.resolver.eliminate_crate_var(item) }
	fn visit_expansion(&mut self, mark: Mark, expansion: &Expansion) { self.resolver.visit_expansion(mark, expansion) }
	fn add_macro(&mut self, scope: Mark, def: MacroDef, export: bool) { self.resolver.add_macro(scope, def, export) }
	fn add_ext(&mut self, ident: Ident, ext: Rc<SyntaxExtension>) { self.resolver.add_ext(ident, ext) }
	fn add_expansions_at_stmt(&mut self, id: NodeId, macros: Vec<Mark>) { self.resolver.add_expansions_at_stmt(id, macros) }
	fn resolve_imports(&mut self) { self.resolver.resolve_imports() }
	fn find_attr_invoc(&mut self, attrs: &mut Vec<Attribute>) -> Option<Attribute> { self.resolver.find_attr_invoc(attrs) }
	fn resolve_macro(&mut self, scope: Mark, path: &Path, force: bool) -> Result<Rc<SyntaxExtension>, Determinacy> {
		if path.global == false && path.segments.len() == 1 && path.segments[0].identifier.name == self.name {
			Ok(self.iota.get().clone())
		} else {
			self.resolver.resolve_macro(scope, path, force)
		}
	}
}

fn parse_ident_or_underscore<'a>(p: &mut Parser<'a>) -> PResult<'a, Option<(Visibility, Ident)>> {
	// Check for a visibility qualifier
	if p.eat_keyword(Pub) {
		Ok(Some((Visibility::Public, p.parse_ident()?)))
	} else if p.eat(&Token::Underscore) {
		Ok(None)
	} else {
		Ok(Some((Visibility::Inherited, p.parse_ident()?)))
	}
}

// A single line of consts.
struct IotaItem {
	span: Span,
	idents: Spanned<Vec<Option<(Visibility, Ident)>>>,
	types: Spanned<Vec<P<Ty>>>,
	exprs: Spanned<Vec<P<Expr>>>,
	prev: bool,
	tuple: bool,
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
	reg.register_macro("consts", expand_const);
}

fn parse_tuple_items<'a>(parser: &mut Parser<'a>, prev: Option<IotaItem>) -> PResult<'a, IotaItem> {
	let lo = parser.span.lo;

	// Parse the identifiers
	let idents = parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, parse_ident_or_underscore)?;

	// Check if we need to use a previous line's types/expressions
	let (types, exprs, prev) = match (prev, parser.eat(&Token::Semi)) {
		(Some(x), true) => {
			// Make sure the previous line was a tuple
			if !x.tuple {
				parser.span_err(idents.span, "found tuple, expected single identifier");
			}
			(x.types, x.exprs, true)
		},
		(None, true) =>
			return Err(parser.span_fatal(idents.span, "missing type information for ident(s)")),
		(_, false) => {
			parser.expect(&Token::Colon)?;

			// Parse the types
			let types = parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, Parser::parse_ty)?;

			parser.expect(&Token::Eq)?;

			// Parse the expressions
			let exprs = parser.parse_seq(&OPEN_PAREN, &CLOSE_PAREN, COMMA_SEP, Parser::parse_expr)?;

			parser.expect(&Token::Semi)?;

			(types, exprs, false)
		}
	};


	let span = mk_sp(lo, parser.span.hi);

	Ok(IotaItem{span: span, idents: idents, types: types, exprs: exprs, prev: prev, tuple: true})
}

fn parse_ident_item<'a>(parser: &mut Parser<'a>, prev: Option<IotaItem>) -> PResult<'a, IotaItem> {
	let lo = parser.span.lo;

	// Parse the identifier
	let ident = parse_ident_or_underscore(parser)?;
	let ispan = mk_sp(lo, parser.span.hi);

	// Check if we need to use a previous line's types/expressions
	let (types, exprs, prev) = match (prev, parser.eat(&Token::Semi)) {
		(Some(x), true) => {
			// Make sure the previous line wasn't a tuple
			if x.tuple {
				parser.span_err(ispan, "found single identifier, expected tuple");
			}
			(x.types, x.exprs, true)
		}
		(None, true) =>
			return Err(parser.span_fatal(ispan, "missing type information for ident")),
		(_, false) => {
			parser.expect(&Token::Colon)?;

			// Parse the type
			let type_ = parser.parse_ty()?;

			parser.expect(&Token::Eq)?;

			// Parse the expression
			let expr = parser.parse_expr()?;

			parser.expect(&Token::Semi)?;

			let (tspan, espan) = (type_.span, expr.span);
			(Spanned{node: vec![type_], span: tspan}, Spanned{node: vec![expr], span: espan}, false)
		},
	};


	let span = mk_sp(lo, parser.span.hi);
	let idents = Spanned{node: vec![ident], span: ispan};

	Ok(IotaItem{span: span, idents: idents, types: types, exprs: exprs, prev: prev, tuple: false})
}

fn expand_const(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
	let iota_name = cx.name_of("iota");

	let mut items = vec![];

	let mut parser = cx.new_parser_from_tts(args);
	let mut prev = None;
	for iota in 0u64.. {
		// Parse the current line
		let current = match parser.token {
			OPEN_PAREN => parse_tuple_items(&mut parser, prev),
			Token::Ident(_) => parse_ident_item(&mut parser, prev),
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
				cx.span_err(current.exprs.span, &format!("expected {} expression(s), found {}", t, e));
				return DummyResult::any(sp);
			},
			(0, _, _, _) => {
				cx.span_err(current.idents.span, "no identifiers found");
				prev = Some(current);
				continue;
			},
			(i, t, _, false) if i != t => {
				cx.span_err(current.types.span, &format!("expected {} type(s), found {}", i, t));
				prev = Some(current);
				continue;
			},
			(i, t, _, true) if i != t => {
				cx.span_err(current.span, &format!("expected {} identifier(s), found {}", t, i));
				prev = Some(current);
				continue;
			},
			_ => (),
		}

		// Expand iota!()
		let expanded_expr: Vec<_> = {
			let mut resolver = IotaResolver{
				name: iota_name,
				iota: Lazy::Lazy(|| Rc::new(SyntaxExtension::NormalTT(Box::new(Iota(iota)), Some(sp), false))),
				resolver: cx.resolver,
			};
			let ecfg = ExpansionConfig{
				crate_name: cx.ecfg.crate_name.clone(),
				features: cx.ecfg.features,
				recursion_limit: cx.ecfg.recursion_limit,
				trace_mac: cx.ecfg.trace_mac,
				should_test: cx.ecfg.should_test,
				single_step: cx.ecfg.single_step,
				keep_macs: cx.ecfg.keep_macs,
			};
			let mut cx = ExtCtxt::new(cx.parse_sess, ecfg, &mut resolver);
			let mut exp = cx.expander();
		 	current.exprs.node.iter().map(|expr| exp.fold_expr(expr.clone())).collect()
		};


		// Generate all the const items
		let mut current = current;
		for ((ident, type_), expr) in current.idents.node.drain(..).zip(current.types.node.iter()).zip(expanded_expr) {
			let (vis, ident) = if let Some(x) = ident { x } else { continue; };

			let item = cx.item_const(current.span, ident, type_.clone(), expr)
				.map(|mut item| { item.vis = vis; item });
			items.push(item);
		}

		prev = Some(current);
	}

	MacEager::items(SmallVector::many(items))
}
