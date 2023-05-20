use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;
use syn::Token;

fn escape_char(ch: char) -> impl Iterator<Item = char> {
    match ch {
        '0'..='9' => {
            let v = "_0_1_2_3_4_5_6_7_8_9";
            let offset = (ch as u32 - '0' as u32) as usize * 2;
            v[offset..offset + 2].chars()
        }
        ' ' | ',' | ':' | ';' => "_".chars(),
        '<' => "_lt_".chars(),
        '>' => "_gt_".chars(),
        '=' => "_eq_".chars(),
        _ => "__".chars(),
    }
}

fn process_lit_name(
    names: Vec<syn::LitStr>,
) -> Result<syn::Ident, impl Into<proc_macro::TokenStream>> {
    use unicode_ident::*;
    let first_span = names.get(0).unwrap().span();
    let chars = names
        .into_iter()
        .flat_map(|lit| lit.value().chars().collect::<Vec<_>>());
    let mut ident_name = String::new();
    let mut origin_char_count = 0usize;
    for (idx, ch) in chars.enumerate() {
        let is_usable = if idx == 0 {
            is_xid_start(ch)
        } else {
            is_xid_continue(ch)
        };
        if is_usable {
            ident_name.push(ch);
            origin_char_count += 1;
        } else {
            ident_name.extend(escape_char(ch));
        }
    }
    if ident_name == "" || ident_name == "_" {
        /*
            let span: proc_macro::Span = first_span.into();
            span.error("cannot create ident").emit();
            return None;
        */
        return Err(syn::Error::new(first_span, "cannot create ident").into_compile_error());
    }
    if origin_char_count <= 3 {
        /*
            let span: proc_macro::Span = first_span.clone().into();
            span.warning("too few unescaped chars make different identifiers easy to collision")
                .emit();
        */
    }
    Ok(syn::Ident::new(&ident_name, first_span))
}

struct LitNameItemArgs {
    fn_lit_name: Vec<syn::LitStr>,
}

impl syn::parse::Parse for LitNameItemArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let attr_content;
        let _attr_paren_token = syn::parenthesized!(attr_content in input);
        let mut fn_lit_name: Vec<syn::LitStr> = vec![attr_content.parse()?];
        while !attr_content.is_empty() {
            fn_lit_name.push(attr_content.parse()?);
        }
        if !input.is_empty() {
            return Err(input.lookahead1().error());
        }
        Ok(LitNameItemArgs { fn_lit_name })
    }
}

#[proc_macro_attribute]
pub fn name(attr: TokenStream, input: TokenStream) -> TokenStream {
    let LitNameItemArgs { fn_lit_name } = parse_macro_input!(attr as LitNameItemArgs);
    let mut input = parse_macro_input!(input as syn::Item);
    let ident_place = match &mut input {
        syn::Item::Const(syn::ItemConst { ident, .. })
        | syn::Item::Enum(syn::ItemEnum { ident, .. })
        | syn::Item::Fn(syn::ItemFn {
            sig: syn::Signature { ident, .. },
            ..
        })
        | syn::Item::Macro(syn::ItemMacro {
            ident: Some(ident), ..
        })
        | syn::Item::Mod(syn::ItemMod { ident, .. })
        | syn::Item::Static(syn::ItemStatic { ident, .. })
        | syn::Item::Struct(syn::ItemStruct { ident, .. })
        | syn::Item::Trait(syn::ItemTrait { ident, .. })
        | syn::Item::TraitAlias(syn::ItemTraitAlias { ident, .. })
        | syn::Item::Type(syn::ItemType { ident, .. })
        | syn::Item::Union(syn::ItemUnion { ident, .. }) => ident,
        syn::Item::Macro(syn::ItemMacro { ident: None, .. })
        | syn::Item::ExternCrate(_)
        | syn::Item::ForeignMod(_)
        | syn::Item::Impl(_)
        | syn::Item::Use(_)
        | syn::Item::Verbatim(_)
        | _ => {
            return syn::Error::new_spanned(input, "this item doesn't have ident")
                .into_compile_error()
                .into()
        }
    };
    let ident = match process_lit_name(fn_lit_name) {
        Ok(ident) => ident,
        Err(e) => return e.into(),
    };
    *ident_place = ident;
    input.into_token_stream().into()
}

struct LitNameCall {
    fn_lit_name: Vec<syn::LitStr>,
    args: syn::punctuated::Punctuated<syn::Expr, Token![,]>,
}

impl syn::parse::Parse for LitNameCall {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let fn_lit_name_first: syn::LitStr = input.parse()?;
        let mut fn_lit_name = vec![fn_lit_name_first];
        while input.lookahead1().peek(syn::LitStr) {
            fn_lit_name.push(input.parse()?);
        }
        if input.lookahead1().peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }
        let args = input.parse_terminated(syn::Expr::parse, Token![,])?;
        Ok(LitNameCall { fn_lit_name, args })
    }
}

#[proc_macro]
pub fn call(input: TokenStream) -> TokenStream {
    let LitNameCall { fn_lit_name, args } = parse_macro_input!(input as LitNameCall);
    let ident = match process_lit_name(fn_lit_name) {
        Ok(ident) => ident,
        Err(e) => return e.into(),
    };
    syn::ExprCall {
        attrs: vec![],
        func: Box::new(syn::Expr::Path(syn::ExprPath {
            path: ident.into(),
            attrs: Default::default(),
            qself: Default::default(),
        })),
        paren_token: Default::default(),
        args,
    }
    .to_token_stream()
    .into()
}
