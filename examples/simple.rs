#![feature(proc_macro)]
#[macro_use] extern crate diesel_struct;

mod models {
    #[derive(DieselStruct)]
    pub struct User {
        /// The ID of the user
        #[idx] id: i64,
        #[fix] email: String,
        name: String,
        lastname: Option<String>,
        image: u64,
        password_hash: String,
        #[tmp] password: String,
    }
}

fn main() {
    let mut user_builder = models::User::build();
    user_builder.set_name(String::from("Luke Cage"));
    let user = user_builder.save();
    println!("New User id: {}", user.id());
}
