use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse::discouraged::Speculative;
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
        '+' => "_plus_".chars(),
        '-' => "_minus_".chars(),
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
        let mut fn_lit_name: Vec<syn::LitStr> = vec![input.parse()?];
        while !input.is_empty() {
            fn_lit_name.push(input.parse()?);
        }
        if !input.is_empty() {
            return Err(input.lookahead1().error());
        }
        Ok(LitNameItemArgs { fn_lit_name })
    }
}

enum LitNameAppliedItem {
    Item(syn::Item),
    TraitItem(syn::TraitItem),
    ImplItem(syn::ImplItem),
}

impl syn::parse::Parse for LitNameAppliedItem {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let result;
        let fork1 = input.fork();
        if let Some(item) = fork1
            .parse()
            .ok()
            .filter(|t| !matches!(t, syn::Item::Verbatim(_)))
        {
            input.advance_to(&fork1);
            result = LitNameAppliedItem::Item(item);
        } else {
            let fork2 = input.fork();
            if let Some(trait_item) = fork2
                .parse()
                .ok()
                .filter(|t| !matches!(t, syn::TraitItem::Verbatim(_)))
            {
                input.advance_to(&fork2);
                result = LitNameAppliedItem::TraitItem(trait_item);
            } else {
                let fork3 = input.fork();
                if let Some(impl_item) = fork3
                    .parse()
                    .ok()
                    .filter(|t| !matches!(t, syn::ImplItem::Verbatim(_)))
                {
                    input.advance_to(&fork3);
                    result = LitNameAppliedItem::ImplItem(impl_item);
                } else {
                    return Err(input.lookahead1().error());
                }
            }
        }

        if !input.is_empty() {
            return Err(input.lookahead1().error());
        }
        Ok(result)
    }
}

impl ToTokens for LitNameAppliedItem {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            LitNameAppliedItem::Item(item) => item.to_tokens(tokens),
            LitNameAppliedItem::TraitItem(trait_item) => trait_item.to_tokens(tokens),
            LitNameAppliedItem::ImplItem(impl_item) => impl_item.to_tokens(tokens),
        }
    }
}

#[proc_macro_attribute]
pub fn name(attr: TokenStream, input: TokenStream) -> TokenStream {
    let LitNameItemArgs { fn_lit_name } = parse_macro_input!(attr as LitNameItemArgs);
    let mut input = parse_macro_input!(input as LitNameAppliedItem);
    let ident_place = match &mut input {
        LitNameAppliedItem::Item(syn::Item::Const(syn::ItemConst { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Enum(syn::ItemEnum { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Fn(syn::ItemFn {
            sig: syn::Signature { ident, .. },
            ..
        }))
        | LitNameAppliedItem::Item(syn::Item::Macro(syn::ItemMacro {
            ident: Some(ident), ..
        }))
        | LitNameAppliedItem::Item(syn::Item::Mod(syn::ItemMod { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Static(syn::ItemStatic { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Struct(syn::ItemStruct { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Trait(syn::ItemTrait { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::TraitAlias(syn::ItemTraitAlias { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Type(syn::ItemType { ident, .. }))
        | LitNameAppliedItem::Item(syn::Item::Union(syn::ItemUnion { ident, .. }))
        | LitNameAppliedItem::TraitItem(syn::TraitItem::Const(syn::TraitItemConst {
            ident, ..
        }))
        | LitNameAppliedItem::TraitItem(syn::TraitItem::Fn(syn::TraitItemFn {
            sig: syn::Signature { ident, .. },
            ..
        }))
        | LitNameAppliedItem::TraitItem(syn::TraitItem::Type(syn::TraitItemType {
            ident, ..
        }))
        | LitNameAppliedItem::ImplItem(syn::ImplItem::Const(syn::ImplItemConst {
            ident, ..
        }))
        | LitNameAppliedItem::ImplItem(syn::ImplItem::Fn(syn::ImplItemFn {
            sig: syn::Signature { ident, .. },
            ..
        }))
        | LitNameAppliedItem::ImplItem(syn::ImplItem::Type(syn::ImplItemType { ident, .. })) => {
            ident
        }
        LitNameAppliedItem::Item(syn::Item::Macro(syn::ItemMacro { ident: None, .. }))
        | LitNameAppliedItem::Item(syn::Item::ExternCrate(_))
        | LitNameAppliedItem::Item(syn::Item::ForeignMod(_))
        | LitNameAppliedItem::Item(syn::Item::Impl(_))
        | LitNameAppliedItem::Item(syn::Item::Use(_))
        | LitNameAppliedItem::TraitItem(syn::TraitItem::Macro(_))
        | LitNameAppliedItem::ImplItem(syn::ImplItem::Macro(_)) => {
            return syn::Error::new_spanned(input, "this item doesn't have ident")
                .into_compile_error()
                .into()
        }
        LitNameAppliedItem::Item(syn::Item::Verbatim(_))
        | LitNameAppliedItem::TraitItem(syn::TraitItem::Verbatim(_))
        | LitNameAppliedItem::ImplItem(syn::ImplItem::Verbatim(_))
        | LitNameAppliedItem::Item(_)
        | LitNameAppliedItem::TraitItem(_)
        | LitNameAppliedItem::ImplItem(_) => {
            return syn::Error::new_spanned(input, "this item is not successfully parsed")
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

enum LitNameCallPrefixToken {
    Ident(syn::Ident),
    Dot(syn::Token![.]),
    DoubleColon(syn::Token![::]),
}

impl syn::parse::Parse for LitNameCallPrefixToken {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        if input.lookahead1().peek(syn::Ident) {
            Ok(Self::Ident(input.parse()?))
        } else if input.lookahead1().peek(syn::Token![.]) {
            Ok(Self::Dot(input.parse()?))
        } else if input.lookahead1().peek(syn::Token![::]) {
            Ok(Self::DoubleColon(input.parse()?))
        } else {
            Err(input.lookahead1().error())
        }
    }
}

impl quote::ToTokens for LitNameCallPrefixToken {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            LitNameCallPrefixToken::Ident(ident) => ident.to_tokens(tokens),
            LitNameCallPrefixToken::Dot(dot) => dot.to_tokens(tokens),
            LitNameCallPrefixToken::DoubleColon(double_colon) => double_colon.to_tokens(tokens),
        }
    }
}

struct LitNameCall {
    prefix: Vec<LitNameCallPrefixToken>,
    fn_lit_name: Vec<syn::LitStr>,
    args: syn::punctuated::Punctuated<syn::Expr, Token![,]>,
}

impl syn::parse::Parse for LitNameCall {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let mut prefix = vec![];
        loop {
            let fork = input.fork();
            if let Ok(token) = fork.parse() {
                input.advance_to(&fork);
                prefix.push(token);
            } else {
                break;
            }
        }
        let fn_lit_name_first: syn::LitStr = input.parse()?;
        let mut fn_lit_name = vec![fn_lit_name_first];
        while input.lookahead1().peek(syn::LitStr) {
            fn_lit_name.push(input.parse()?);
        }
        if input.lookahead1().peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }
        let args = input.parse_terminated(syn::Expr::parse, Token![,])?;
        Ok(LitNameCall {
            prefix,
            fn_lit_name,
            args,
        })
    }
}

#[proc_macro]
pub fn call(input: TokenStream) -> TokenStream {
    let LitNameCall {
        prefix,
        fn_lit_name,
        args,
    } = parse_macro_input!(input as LitNameCall);
    let ident = match process_lit_name(fn_lit_name) {
        Ok(ident) => ident,
        Err(e) => return e.into(),
    };
    let call_expr = syn::ExprCall {
        attrs: vec![],
        func: Box::new(syn::Expr::Path(syn::ExprPath {
            path: ident.into(),
            attrs: Default::default(),
            qself: Default::default(),
        })),
        paren_token: Default::default(),
        args,
    };
    quote::quote!(
        #(#prefix)* #call_expr
    )
    .into()
}
