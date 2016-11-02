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
    use syn::{Body, VariantData, Ident, Attribute, MetaItem, Lit};

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
    let model_name_iter = vec![model_name.clone()].into_iter().cycle();
    let validations_name = Ident::new(format!("{}Validations", name));

    let table_name : Ident = if let Some(&Attribute { value: MetaItem::NameValue(_, Lit::Str(ref table,_)), ..})
                            = ast.attrs.iter().filter(|x| x.value.name() == "table_name").next()
    {
        table.clone()
    } else {
        format!("{}s", name.to_string().to_lowercase())
    }.into();

    let table_name_iter  = vec![table_name.clone()].into_iter().cycle();
    let table_name_iter2 = vec![table_name.clone()].into_iter().cycle();
    let table_name_iter3 = vec![table_name.clone()].into_iter().cycle();
    let table_name_string = table_name.to_string();

    let without = |name| {
        move |x: &&syn::Field| !x.attrs.iter().any(|x| x.value.name() == name)
    };

    let new_fields : Vec<_> = fields.iter()
        .filter(without("idx")) // Remove all #[idx] fields
        .filter(without("tmp")) // Remove all #[tmp] fields
        .cloned()
        .map(strip_attributes)
        .collect();

    let null_field_names : Vec<_> = new_fields.iter()
        .filter(is_option)
        .map(|x| x.ident.as_ref().unwrap().clone())
        .collect();

    let null_field_names2 = null_field_names.clone();

    let non_null_field_names : Vec<_> = new_fields.iter()
        .filter(|x| !is_option(x))
        .map(|x| x.ident.as_ref().unwrap().clone())
        .collect();

    let non_null_field_names2 = non_null_field_names.clone();
    let non_null_field_names3 = non_null_field_names.clone();
    let non_null_field_names4 = non_null_field_names.clone();

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

    let new_builder_fields : Vec<_> = fields.iter()
        .filter(without("idx")) // Remove all #[idx] fields
        .cloned()
        .map(strip_attributes)
        .map(make_option)
        .collect();

    let new_builder_field_names : Vec<_> = new_builder_fields.iter()
        .map(|x| x.ident.as_ref().unwrap().clone())
        .collect();

    let new_builder_field_names2 = new_builder_field_names.clone();

    let validations_field_names  = new_builder_field_names.clone();
    let validations_field_names2 = new_builder_field_names.clone();
    let validations_field_names3 = new_builder_field_names.clone();

    let new_builder_field_types : Vec<_> = new_builder_fields.iter().map(|x| &x.ty)
        .cloned()
        .collect();

    let new_builder_field_names_setters : Vec<_> = new_builder_fields.iter()
        .map(|x| Ident::new(format!("set_{}", x.ident.as_ref().unwrap())))
        .collect();

    let new_builder_field_types_without_option : Vec<_> = new_builder_fields.iter().map(|x| &x.ty)
        .cloned()
        .map(strip_option)
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

    let find_by_model_names : Vec<_> = model_fields.iter()
        .map(|x| Ident::new(format!("find_by_{}", x.ident.as_ref().unwrap())))
        .collect();

    let find_by_model_names2 = model_field_names.clone();

    let find_by_model_types = model_field_types.clone();


    quote! {
        use schema::#table_name;

        #vis struct #name;

        #[derive(Queryable)]
        #[derive(Clone, Debug)]
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

        #[derive(Insertable)]
        #[table_name=#table_name_string]
        #[derive(Clone, Debug)]
        #vis struct #new_name {
            #( #new_fields ),*
        }

        #[derive(Clone, Debug)]
        #vis struct #update_name {
            #( #update_fields ),*
        }

        #[derive(Clone, Debug)]
        #vis struct #new_name_builder {
            #( #new_builder_fields ),*
        }

        #[derive(Clone, Debug)]
        #vis struct #validations_name {
            pub builder: #new_name_builder,
            #( #validations_field_names : Vec<String>),*
        }

        impl #validations_name {
            pub fn new(builder: &#new_name_builder) -> #validations_name {
                #validations_name {
                    builder: builder.clone(),
                    #( #validations_field_names2 : Vec::new()),*
                }
            }

            pub fn no_errors(&self) -> bool {
                true #(&& self.#validations_field_names3.is_empty() )*
            }

            pub fn verify(self) -> Result<(), #validations_name> {
                if self.no_errors() { Ok(()) } else { Err(self) }
            }
        }

        impl #name {
            pub fn build() -> #new_name_builder {
                #new_name_builder {
                    #( #new_builder_field_names : None ),*
                }
            }

            pub fn find(id: usize, conn: &::diesel::sqlite::SqliteConnection) -> Result<#model_name, ::diesel::result::Error> {
                use diesel::prelude::*;
                use schema::#table_name::dsl::*;

                #table_name.filter(id.eq(id))
                     .get_result(conn)
            }
        }

        impl #new_name_builder {
            #(
                #[allow(dead_code)]
                pub fn #new_builder_field_names_setters (&mut self, x: #new_builder_field_types_without_option) -> &mut Self {
                    self.#new_builder_field_names2 = Some(x);
                    self
                }
            )*

            pub fn verify(self) -> Result<#new_name, #validations_name> {
                try!(self.validate());
                try!(self.verify_null());

                Ok(#new_name {
                    #( #null_field_names: self.#null_field_names2, )*
                    #( #non_null_field_names: self.#non_null_field_names2.unwrap(), )*
                })
            }

            fn verify_null(&self) -> Result<(), #validations_name> {
                let mut err = #validations_name::new(self);
                #(if self.#non_null_field_names3.is_none() {
                    err.#non_null_field_names4.push("Cannot be empty".into());
                })*
                err.verify()
            }
        }

        impl #new_name {
            pub fn save(self, conn: &::diesel::sqlite::SqliteConnection) -> Result<(), ::diesel::result::Error> {
                use schema::#table_name;
                use diesel::prelude::*;

                ::diesel::insert(&self).into(#table_name::table)
                    .execute(conn).map(|_| ())
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

fn is_option(field: &&syn::Field) -> bool {
    use syn::{Ty, AngleBracketedParameterData, PathParameters};
    if let &Ty::Path(None, ref p) = &field.ty {
        if let Some(ref seg) = p.segments.last() {
            if seg.ident.as_ref() == "Option" {
                return true;
            }
        }
    }
    false
}
