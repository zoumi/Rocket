#![feature(plugin, custom_derive)]
#![plugin(rocket_codegen)]

#[derive(FromForm)]
struct MyForm {
    #[form(field = "blah", field = "bloo")]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm1 {
    #[form]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm2 {
    #[form("blah")]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm3 {
    #[form(123)]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm4 {
    #[form(beep = "bop")]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm5 {
    #[form(field = "blah")]
    #[form(field = "blah")]
    //~^^^ ERROR: more than one
    my_field: String,
}

#[derive(FromForm)]
struct MyForm6 {
    #[form(field = true)]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm7 {
    #[form(field)]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm8 {
    #[form(field = 123)]
    //~^^ ERROR: malformed
    my_field: String,
}

#[derive(FromForm)]
struct MyForm9 {
    #[form(field = "hello")]
    //~^^ ERROR: more than one field
    first: String,
    #[form(field = "hello")]
    other: String,
}

#[derive(FromForm)]
struct MyForm10 {
    first: String,
    #[form(field = "first")]
    //~^^^ ERROR: more than one field
    other: String,
}

#[derive(FromForm)]
struct MyForm11 {
    #[form(field = "hello world")]
    //~^^ ERROR: invalid field name
    first: String,
}

#[derive(FromForm)]
struct MyForm12 {
    #[form(field = "!@#$%^&*()_")]
    //~^^ ERROR: invalid field name
    first: String,
}
