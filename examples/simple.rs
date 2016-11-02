#![feature(proc_macro)]
#[macro_use] extern crate diesel_struct;
#[macro_use] extern crate diesel_codegen;
#[macro_use] extern crate diesel;

mod models {
    #[derive(DieselStruct)]
    #[table_name="users"]
    pub struct User {
        /// The ID of the user
        #[idx] id: i32,
        #[fix] email: String,
        name: String,
        lastname: Option<String>,
        password_hash: String,
        #[tmp] password: String,
    }

    impl NewUserBuilder {
        pub fn validate(&self) -> Result<(), UserValidations> {
            let mut errors = UserValidations::new(self);
            if let Some(ref name) = self.lastname {
                if name.len() < 2 {
                    errors.name.push(String::from("Name has to be longer than 2 characters"));
                }
            }

            errors.verify()
        }

        pub fn transform(&mut self) -> Result<(), String>  {
            self.password_hash = self.password.clone();
            Ok(())
        }
    }

    impl UserModel {
        pub fn full_name(&self) -> String {
            format!("{} {}", self.name, self.lastname.as_ref().unwrap_or(&String::new()))
        }
    }
}

mod schema {
    infer_schema!("target/database");
}

fn main() {
    use diesel::sqlite::SqliteConnection;
    use diesel::prelude::*;

    let conn = SqliteConnection::establish("target/database").unwrap();

    let mut user_builder = models::User::build();
    user_builder.set_name(String::from("Luke"));
    user_builder.set_lastname(String::from("Cage"));
    user_builder.set_email(String::from("luke@cage.us"));
    user_builder.set_password(String::from("iamstronk"));

    user_builder.transform().unwrap(); // Do some computation

    user_builder.verify().unwrap().save(&conn).unwrap();
    let user = models::User::find(1, &conn);
    println!("New User: {:#?}", user);
}
