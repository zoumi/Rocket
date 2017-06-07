mod meta_item_ext;
mod arg_ext;
mod parser_ext;
mod ident_ext;
mod span_ext;

pub use self::arg_ext::ArgExt;
pub use self::meta_item_ext::MetaItemExt;
pub use self::parser_ext::ParserExt;
pub use self::ident_ext::IdentExt;
pub use self::span_ext::SpanExt;

use std::convert::AsRef;

use syntax;
use syntax::parse::token::Token;
use syntax::tokenstream::TokenTree;
use syntax::ast::{self, Item, Expr};
use syntax::ext::base::{Annotatable, ExtCtxt};
use syntax::codemap::{Span, Spanned, DUMMY_SP};
use syntax::ext::quote::rt::ToTokens;
use syntax::print::pprust::item_to_string;
use syntax::ptr::P;
use syntax::fold::Folder;
use syntax::ast::{Attribute, Lifetime, LifetimeDef, Ty};
use syntax::attr::HasAttrs;

pub fn span<T>(t: T, span: Span) -> Spanned<T> {
    Spanned { node: t, span: span }
}

pub fn sep_by_tok<T>(ecx: &ExtCtxt, things: &[T], token: Token) -> Vec<TokenTree>
    where T: ToTokens
{
    let mut output: Vec<TokenTree> = vec![];
    for (i, thing) in things.iter().enumerate() {
        output.extend(thing.to_tokens(ecx));
        if i < things.len() - 1 {
            output.push(TokenTree::Token(DUMMY_SP, token.clone()));
        }
    }

    output
}

pub fn option_as_expr<T: ToTokens>(ecx: &ExtCtxt, opt: &Option<T>) -> P<Expr> {
    match *opt {
        Some(ref item) => quote_expr!(ecx, Some($item)),
        None => quote_expr!(ecx, None),
    }
}

pub fn emit_item(items: &mut Vec<Annotatable>, item: P<Item>) {
    debug!("Emitting item:\n{}", item_to_string(&item));
    items.push(Annotatable::Item(item));
}

pub fn attach_and_emit(out: &mut Vec<Annotatable>, attr: Attribute, to: Annotatable) {
    syntax::attr::mark_used(&attr);
    syntax::attr::mark_known(&attr);

    // Attach the attribute to the user's function and emit it.
    if let Annotatable::Item(user_item) = to {
        let item = user_item.map_attrs(|mut attrs| {
            attrs.push(attr);
            attrs
        });

        emit_item(out, item);
    }
}

macro_rules! quote_enum {
    ($ecx:expr, $var:expr => $(::$root:ident)+
     { $($variant:ident),+ ; $($extra:pat => $result:expr),* }) => ({
        use syntax::codemap::DUMMY_SP;
        use syntax::ast::Ident;
        use $(::$root)+::*;
        let root_idents = vec![$(Ident::from_str(stringify!($root))),+];
        match $var {
            $($variant => {
                let variant = Ident::from_str(stringify!($variant));
                let mut idents = root_idents.clone();
                idents.push(variant);
                $ecx.path_global(DUMMY_SP, idents)
            })+
            $($extra => $result),*
        }
    })
}

pub struct TyLifetimeRemover;

// FIXME: Doesn't work for T + whatever.
impl Folder for TyLifetimeRemover {
    fn fold_opt_lifetime(&mut self, _: Option<Lifetime>) -> Option<Lifetime> {
        None
    }

    fn fold_lifetime_defs(&mut self, _: Vec<LifetimeDef>) -> Vec<LifetimeDef> {
        vec![]
    }

    fn fold_lifetimes(&mut self, _: Vec<Lifetime>) -> Vec<Lifetime> {
        vec![]
    }
}

pub fn strip_ty_lifetimes(ty: P<Ty>) -> P<Ty> {
    TyLifetimeRemover.fold_ty(ty)
}

// Lifted from Rust's lexer, except this takes a `char`, not an `Option<char>`.
fn ident_start(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' ||
    (c > '\x7f' && c.is_xid_start())
}

// Lifted from Rust's lexer, except this takes a `char`, not an `Option<char>`.
fn ident_continue(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
    c == '_' || (c > '\x7f' && c.is_xid_continue())
}

pub fn is_valid_ident<S: AsRef<str>>(s: S) -> bool {
    let string = s.as_ref();
    if string.is_empty() {
        return false;
    }

    for (i, c) in string.chars().enumerate() {
        if i == 0 {
            if !ident_start(c) {
                return false;
            }
        } else if !ident_continue(c) {
            return false;
        }
    }

    true
}

// Makes every 'attr_name' attribute uses in `item` as known so we don't get a
// compile-time error from rustc. This will go away with macros 1.1/2.
pub fn mark_known_attrs(item: &ast::Item, attr_name: &str) {
    for attr in item.attrs.iter() {
        if attr.check_name(attr_name) {
            syntax::attr::mark_known(attr);
        }
    }

    if let ast::ItemKind::Struct(ast::VariantData::Struct(ref fs, _), _) = item.node {
        for attr in fs.iter().flat_map(|field| field.attrs.iter()) {
            if attr.check_name(attr_name) {
                syntax::attr::mark_known(attr);
            }
        }
    }
}

use syn;

pub fn syn_strip_lifetimes(mut ty: syn::Ty) -> syn::Ty {
    fn strip(ty: &mut syn::Ty) {
        match *ty {
            syn::Ty::Rptr(ref mut lifetime, ref mut mut_ty) => {
                *lifetime = None;
                strip(&mut mut_ty.ty);
            }
            syn::Ty::Slice(ref mut ty) => strip(&mut **ty),
            syn::Ty::Array(ref mut ty, _) => strip(&mut **ty),
            syn::Ty::Ptr(ref mut mut_ty) => strip(&mut mut_ty.ty),
            syn::Ty::BareFn(ref mut bare_fn_ty) => {
                bare_fn_ty.lifetimes = vec![];
                if let syn::FunctionRetTy::Ty(ref mut ty) = bare_fn_ty.output {
                    strip(ty);
                }

                for input in bare_fn_ty.inputs.iter_mut() {
                    strip(&mut input.ty);
                }
            }
            syn::Ty::Tup(ref mut tys) => {
                for ty in tys.iter_mut() {
                    strip(ty)
                }
            }
            syn::Ty::Path(ref mut opt_qself, ref mut path) => {
                if let Some(ref mut qself) = *opt_qself {
                    strip(&mut qself.ty);
                }

                for segment in path.segments.iter_mut() {
                    match segment.parameters {
                        syn::PathParameters::AngleBracketed(ref mut param_data) => {
                            param_data.lifetimes = vec![];
                            for ty in param_data.types.iter_mut() {
                                strip(ty);
                            }

                            for binding in param_data.bindings.iter_mut() {
                                strip(&mut binding.ty);
                            }
                        }
                        syn::PathParameters::Parenthesized(ref mut param_data) => {
                            for ty in param_data.inputs.iter_mut() {
                                strip(ty);
                            }

                            if let Some(ref mut ty) = param_data.output {
                                strip(ty);
                            }
                        }
                    }
                }
            }
            syn::Ty::Paren(ref mut ty) => strip(&mut **ty),
            syn::Ty::Never | syn::Ty::Infer | syn::Ty::Mac(_) => {}
            syn::Ty::TraitObject(_) | syn::Ty::ImplTrait(_) => { /* TODO: ?? */ }
        }
    }

    strip(&mut ty);
    ty
}
