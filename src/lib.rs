#![recursion_limit="255"]
#![feature(proc_macro, proc_macro_lib)]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;
#[macro_use] extern crate quote;

#[proc_macro_derive(DieselStruct)]
pub fn diesel_struct(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    let ast = syn::parse_macro_input(&source).unwrap();

    let expanded = expand_diesel_struct(&ast);

    println!("{}", expanded);
    expanded.to_string().parse().unwrap()
}

fn expand_diesel_struct(ast: &syn::MacroInput) -> quote::Tokens {
    use syn::{Body, VariantData, Ident};

    let fields = match ast.body {
        Body::Struct(VariantData::Struct(ref fields)) => fields.clone(),
        _ => panic!("DieselStruct can only be used on non-tuple structs"),
    };

    if !ast.generics.lifetimes.is_empty() || !ast.generics.ty_params.is_empty() {
        panic!("DieselStruct does not support lifetimes or generics");
    }

    let vis = &ast.vis;
    let name = &ast.ident;
    let new_name = Ident::new(format!("New{}", name));
    let new_name_builder = Ident::new(format!("New{}Builder", name));
    let update_name = Ident::new(format!("Update{}", name));
    let model_name = Ident::new(format!("{}Model", name));

    let without = |name| {
        move |x: &&syn::Field| !x.attrs.iter().any(|x| x.value.name() == name)
    };

    let new_fields : Vec<_> = fields.iter()
        .filter(without("idx")) // Remove all #[idx] fields
        .cloned()
        .map(strip_attributes)
        .collect();

    let new_field_types : Vec<_> = new_fields.iter().map(|x| &x.ty)
        .cloned()
        .collect();

    let new_field_types_without_option : Vec<_> = new_fields.iter().map(|x| &x.ty)
        .cloned()
        .map(strip_option)
        .collect();

    let update_fields : Vec<_> = fields.iter()
        .filter(without("idx")) // Remove all #[idx] fields
        .filter(without("fix")) // Remove all #[fix] fields
        .cloned()
        .map(strip_attributes)
        .collect();

    let new_builder_fields : Vec<_> = new_fields.iter()
        .cloned()
        .map(make_option)
        .collect();

    let new_builder_field_names : Vec<_> = new_builder_fields.iter()
        .map(|x| x.ident.as_ref().unwrap().clone())
        .collect();

    let new_builder_field_names2 = new_builder_field_names.clone();

    let new_builder_field_types : Vec<_> = new_builder_fields.iter().map(|x| &x.ty)
        .cloned()
        .collect();

    let new_builder_field_names_setters : Vec<_> = new_builder_fields.iter()
        .map(|x| Ident::new(format!("set_{}", x.ident.as_ref().unwrap())))
        .collect();

    let model_fields : Vec<_> = fields.iter()
        .filter(without("tmp")) // Remove all #[tmp] fields
        .cloned()
        .map(strip_attributes)
        .collect();

    let model_field_getters : Vec<_> = fields.iter()
        .filter(without("tmp")) // Remove all #[tmp] fields
        .map(|x| x.ident.as_ref().unwrap().clone())
        .collect();

    let model_field_names : Vec<_> = model_field_getters.clone();

    let model_field_types : Vec<_> = fields.iter()
        .filter(without("tmp")) // Remove all #[tmp] fields
        .map(|x| x.ty.clone())
        .collect();

    quote! {
        #vis struct #name;

        #vis struct #model_name {
            #( #model_fields ),*
        }

        impl #model_name {
            #(
                #[allow(dead_code)]
                pub fn #model_field_getters (&self) -> &#model_field_types {
                    &self.#model_field_names
                }
            )*
        }

        #vis struct #new_name {
            #( #new_fields ),*
        }

        #vis struct #update_name {
            #( #update_fields ),*
        }

        #vis struct #new_name_builder {
            #( #new_builder_fields ),*
        }

        impl #name {
            pub fn build() -> #new_name_builder {
                #new_name_builder {
                    #( #new_builder_field_names : None ),*
                }
            }
        }

        impl #new_name_builder {
            #(
                #[allow(dead_code)]
                pub fn #new_builder_field_names_setters (&mut self, x: #new_field_types_without_option) -> &mut Self {
                    self.#new_builder_field_names2 = Some(x);
                    self
                }
            )*

            pub fn save(self) -> #model_name {
                unimplemented!()
            }
        }
    }
}

static ATTRIBUTES : &'static [&'static str] = &["idx", "fix", "tmp"];

fn strip_attributes(mut x: syn::Field) -> syn::Field {
    x.attrs.retain(|a| !ATTRIBUTES.contains(&a.value.name()));
    x
}

fn make_option(mut field: syn::Field) -> syn::Field {
    use syn::{Ty, Path, PathSegment, AngleBracketedParameterData, PathParameters, Ident};
    let mut is_option = false;
    if let &Ty::Path(None, ref p) = &field.ty {
        if let Some(ref seg) = p.segments.last() {
            if seg.ident.as_ref() == "Option" {
                is_option = true;
            }
        }
    }
    if is_option {
        field
    } else {
        let ty = field.ty.clone();
        field.ty = Ty::Path(None, Path {
                global: false,
                segments: vec![
                    PathSegment {
                        ident: Ident::new("Option"),
                        parameters: PathParameters::AngleBracketed(
                            AngleBracketedParameterData {
                                lifetimes: vec![],
                                types: vec![ ty ],
                                bindings: vec![]
                            }
                        )
                    }
                ]
            });
        field
    }
}

fn strip_option(ty: syn::Ty) -> syn::Ty {
    use syn::{Ty, AngleBracketedParameterData, PathParameters};
    if let &Ty::Path(None, ref p) = &ty {
        if let Some(ref seg) = p.segments.last() {
            if seg.ident.as_ref() == "Option" {
                if let PathParameters::AngleBracketed(AngleBracketedParameterData{ ref types, ..}) = seg.parameters {
                    if let Some(ty) = types.last() {
                        return ty.clone();
                    }
                }
            }
        }
    }
    ty
}

