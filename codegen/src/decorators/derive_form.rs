use {syn, quote};
use quote::*;

use syntax::ast;
use syntax::ext::base::{Annotatable, ExtCtxt};
use syntax::codemap::{Span, FilePathMapping};
use syntax::parse::{ParseSess, parse_item_from_source_str};
use syntax::print::pprust::item_to_string;

use utils;

static ITEM_ERR: &str = "`FromForm` can only be derived for structs with named fields.";

// TODO: Use proper logging to emit the error messages.
pub fn from_form_derive(
    ecx: &mut ExtCtxt,
    span: Span,
    _meta_item: &ast::MetaItem,
    annotated: &Annotatable,
    push: &mut FnMut(Annotatable)
) {
    let item = match *annotated {
        Annotatable::Item(ref item) => item,
        _ => ecx.span_fatal(span, ITEM_ERR)
    };

    utils::mark_known_attrs(item, "form");

    let source_string = item_to_string(item);
    let ast = syn::parse_macro_input(&source_string).unwrap();
    match impl_derive(&ast) {
        Ok(tokens) => {
            let impl_source = tokens.to_string();
            let sess = ParseSess::new(FilePathMapping::empty());
            match parse_item_from_source_str("ROCKET".into(), impl_source, &sess) {
                Ok(Some(item)) => push(Annotatable::Item(item)),
                _ => panic!("'derive_form' error. Report to Rocket's issue tracker.")
            };
        }
        Err(error_string) => {
            ecx.struct_span_err(item.span, &error_string)
                .span_label(span, "from this `FromForm` derive")
                .emit();
        }
    }
}

// Returns the original ident of a given `field` as well as the ident requested
// by the user: (original ident, user requested ident). The user requested ident
// is just the original ident except when overrides via #[form(field = "name")].
fn field_idents(field: &syn::Field) -> Result<(syn::Ident, syn::Ident), String> {
    let malformed_attr = "malformed `form` struct field attribute; \
                          expected `#[form(field = \"name\")]`".to_string();

    let mut form_attrs = field.attrs.iter().filter(|attr| attr.name() == "form");
    let original_ident = field.ident.as_ref().unwrap().clone();
    let user_ident = if let Some(attr) = form_attrs.next() {
        if form_attrs.next().is_some() {
            return Err("a struct field may not have more than \
                       one `#[form(..)]` attribute".into());
        }

        let list_items = match attr.value {
            syn::MetaItem::List(_, ref items) if items.len() == 1 => items,
            _ => return Err(malformed_attr)
        };

        let inner_item = match list_items[0] {
            syn::NestedMetaItem::MetaItem(ref item) => item,
            _ => return Err(malformed_attr)
        };

        if inner_item.name() != "field" {
            return Err(malformed_attr)
        }

        let form_field = match *inner_item {
            syn::MetaItem::NameValue(_, syn::Lit::Str(ref s, _)) => s,
            _ => return Err(malformed_attr)
        };

        syn::Ident::new(form_field.as_str())
    } else {
        original_ident.clone()
    };

    Ok((original_ident, user_ident))
}

// Returns the `impl` for `FromForm`.
fn impl_derive(ast: &syn::DeriveInput) -> Result<quote::Tokens, String> {
    let name = &ast.ident;

    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let impl_ty_generics = ast.generics.ty_params.iter().map(|ty| quote!(#ty));
    let mut lifetime_generics = ast.generics.lifetimes.iter().map(|l| quote!(#l));

    let lifetime = lifetime_generics.next().unwrap_or(quote!('f));
    let all_generics: Vec<_> = lifetime_generics.chain(impl_ty_generics).collect();
    let generics = quote!(<#lifetime #(, #all_generics)*>);

    let (str_names, idents, types) = match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref fields)) => {
            let field_idents = fields.iter()
                .map(field_idents)
                .collect::<Result<Vec<_>, _>>()?;

            let (mut idents, mut str_names) = (vec![], vec![]);
            for (ident, name) in field_idents {
                idents.push(ident);
                str_names.push(name.to_string());
            }

            let types: Vec<_> = fields.iter()
                .map(|field| utils::syn_strip_lifetimes(field.ty.clone()))
                .map(|ty| quote! { #ty })
                .collect();

            (str_names, idents, types)
        }
        _ => return Err(ITEM_ERR.into())
    };

    // Ensure no two fields have the same name.
    let mut sorted_strs = str_names.clone();
    sorted_strs.sort();
    for i in 1..sorted_strs.len() {
        if sorted_strs[i] == sorted_strs[i - 1] {
            return Err(format!("more than one field with name '{}'", sorted_strs[i]));
        }
    }

    // Ensure all field names are valid.
    if let Some(f) = str_names.iter().filter(|n| !utils::is_valid_ident(n)).next() {
        return Err(format!("invalid field name: '{}'", f));
    }

    let (str_names, idents, types) = (&str_names, &idents, &types);
    let (idents_copy, str_names_copy) = (idents, str_names);
    Ok(quote! {
        #[allow(unreachable_code, unreachable_patterns)]
        impl #generics ::rocket::request::FromForm<#lifetime> for #name #ty_generics
            #where_clause
        {
            type Error = ();
            fn from_form(__items: &mut ::rocket::request::FormItems<#lifetime>,
                         __strict: bool,
                        ) -> Result<#name #ty_generics, ()>
            {
                use ::rocket::http::RawStr;
                use ::rocket::request::FromFormValue;

                #(let mut #idents: Option<#types> = None;)*
                for (__k, __v) in __items {
                    match __k.as_str() {
                        #(
                            #str_names => {
                                let __r = RawStr::from_str(__v);
                                #idents = match FromFormValue::from_form_value(__r) {
                                    Ok(__v) => Some(__v),
                                    Err(__e) => {
                                        println!("    => Error parsing field '{}': {:?}",
                                                 #str_names_copy, __e);
                                        return Err(());
                                    }
                                };
                            },
                         )*
                        _ => {
                            if __strict && __k != "_method" {
                                println!("    => {}={} has no matching field in struct.", __k, __v);
                                return Err(());
                            }
                        }
                    };
                }

                Ok(#name {
                    #(
                        #idents: match #idents_copy {
                            Some(__val) => __val,
                            None => match <#types as FromFormValue>::default() {
                                Some(__val) => __val,
                                None => {
                                    println!("    => '{}' did not parse.",
                                             stringify!(#str_names));
                                    return Err(());
                                }
                            }
                        },
                    )*
                })
            }
        }
    })
}
