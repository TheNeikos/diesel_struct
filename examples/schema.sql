CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    email VARCHAR NOT NULL,
    name VARCHAR NOT NULL,
    lastname VARCHAR,
    password_hash VARCHAR NOT NULL
);
