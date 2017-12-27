extern crate serde;
extern crate serde_json;
extern crate glob;

#[cfg(feature = "tera_templates")] mod tera_templates;
#[cfg(feature = "handlebars_templates")] mod handlebars_templates;
mod engine;
mod context;

use self::engine::{Engine, Engines};
use self::context::Context;
use self::serde::Serialize;
use self::serde_json::{Value, to_value};
use self::glob::glob;

use std::borrow::Cow;
use std::path::{Path, PathBuf};

use rocket::State;
use rocket::request::Request;
use rocket::fairing::{Fairing, AdHoc};
use rocket::response::{self, Content, Responder};
use rocket::http::{ContentType, Status};
use rocket::config::ConfigError;

const DEFAULT_TEMPLATE_DIR: &'static str = "templates";

/// The Template type implements generic support for template rendering in
/// Rocket.
///
/// Templating in Rocket works by first discovering all of the templates inside
/// the template directory. The template directory is configurable via the
/// `template_dir` configuration parameter and defaults to `templates/`. The
/// path set in `template_dir` should be relative to the Rocket configuration
/// file. See the [configuration
/// chapter](https://rocket.rs/guide/overview/#configuration) of the guide for
/// more information on configuration.
///
/// Templates are discovered according to their extension. At present, this
/// library supports the following templates and extensions:
///
/// * **Tera**: `.tera`
/// * **Handlebars**: `.hbs`
///
/// Any file that ends with one of these extension will be discovered and
/// rendered with the corresponding templating engine. The name of the template
/// will be the path to the template file relative to `template_dir` minus at
/// most two extensions. The following are examples of template names (on the
/// right) given that the template is at the path on the left.
///
///   * `{template_dir}/index.html.hbs` => index
///   * `{template_dir}/index.tera` => index
///   * `{template_dir}/index.hbs` => index
///   * `{template_dir}/dir/index.hbs` => dir/index
///   * `{template_dir}/dir/index.html.tera` => dir/index
///   * `{template_dir}/index.template.html.hbs` => index.template
///   * `{template_dir}/subdir/index.template.html.hbs` => subdir/index.template
///
/// The recommended naming scheme is to use two extensions: one for the file
/// type, and one for the template extension. This means that template
/// extensions should look like: `.html.hbs`, `.html.tera`, `.xml.hbs`, etc.
///
/// Template discovery is actualized by the template fairing, which itself is
/// created via the
/// [`Template::fairing()`](/rocket_contrib/struct.Template.html#method.fairing)
/// method. In order for _any_ templates to be rendered, the template fairing
/// must be [attached](/rocket/struct.Rocket.html#method.attach) to the running
/// Rocket instance.
///
/// Templates are rendered with the `render` method. The method takes in the
/// name of a template and a context to render the template with. The context
/// can be any type that implements `Serialize` from
/// [Serde](https://github.com/serde-rs/json) and would serialize to an `Object`
/// value.
///
/// # Usage
///
/// To use, add the `handlebars_templates` feature, the `tera_templates`
/// feature, or both, to the `rocket_contrib` dependencies section of your
/// `Cargo.toml`:
///
/// ```toml,ignore
/// [dependencies.rocket_contrib]
/// version = "*"
/// default-features = false
/// features = ["handlebars_templates", "tera_templates"]
/// ```
///
/// Then, ensure that the template [fairing](/rocket/fairing/) is attached to
/// your Rocket application:
///
/// ```rust
/// extern crate rocket;
/// extern crate rocket_contrib;
///
/// use rocket_contrib::Template;
///
/// fn main() {
///     rocket::ignite()
///         // ...
///         .attach(Template::fairing())
///         // ...
///     # ;
/// }
/// ```
///
/// The `Template` type implements Rocket's `Responder` trait, so it can be
/// returned from a request handler directly:
///
/// ```rust,ignore
/// #[get("/")]
/// fn index() -> Template {
///     let context = ...;
///     Template::render("index", &context)
/// }
/// ```
#[derive(Debug)]
pub struct Template {
    name: Cow<'static, str>,
    value: Option<Value>
}

#[derive(Debug)]
pub struct TemplateInfo {
    /// The complete path, including `template_dir`, to this template.
    path: PathBuf,
    /// The extension for the engine of this template.
    extension: String,
    /// The extension before the engine extension in the template, if any.
    data_type: ContentType
}

impl Template {
    /// Returns a fairing that intializes and maintains templating state.
    ///
    /// This fairing _must_ be attached to any `Rocket` instance that wishes to
    /// render templates. Failure to attach this fairing will result in a
    /// "Uninitialized template context: missing fairing." error message when a
    /// template is attempted to be rendered.
    ///
    /// # Example
    ///
    /// To attach this fairing, simple call `attach` on the application's
    /// `Rocket` instance with `Template::fairing()`:
    ///
    /// ```rust
    /// extern crate rocket;
    /// extern crate rocket_contrib;
    ///
    /// use rocket_contrib::Template;
    ///
    /// fn main() {
    ///     rocket::ignite()
    ///         // ...
    ///         .attach(Template::fairing())
    ///         // ...
    ///     # ;
    /// }
    /// ```
    pub fn fairing() -> impl Fairing {
        AdHoc::on_attach(|rocket| {
            let mut template_root = rocket.config()
                .root_relative(DEFAULT_TEMPLATE_DIR);

            match rocket.config().get_str("template_dir") {
                Ok(dir) => template_root = rocket.config().root_relative(dir),
                Err(ConfigError::NotFound) => { /* ignore missing configs */ }
                Err(e) => {
                    e.pretty_print();
                    warn_!("Using default templates directory '{:?}'", template_root);
                }
            };

            match Context::initialize(template_root) {
                Some(ctxt) => Ok(rocket.manage(ctxt)),
                None => Err(rocket)
            }
        })
    }

    /// Render the template named `name` with the context `context`. The
    /// `context` can be of any type that implements `Serialize`. This is
    /// typically a `HashMap` or a custom `struct`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::collections::HashMap;
    /// use rocket_contrib::Template;
    ///
    /// // Create a `context`. Here, just an empty `HashMap`.
    /// let mut context = HashMap::new();
    ///
    /// # context.insert("test", "test");
    /// # #[allow(unused_variables)]
    /// let template = Template::render("index", context);
    #[inline]
    pub fn render<S, C>(name: S, context: C) -> Template
        where S: Into<Cow<'static, str>>, C: Serialize
    {
        Template { name: name.into(), value: to_value(context).ok() }
    }

    /// Render the template named `name` located at the path `root` with the
    /// context `context` into a `String`. This method is _very slow_ and should
    /// **not** be used in any running Rocket application. This method should
    /// only be used during testing to validate `Template` responses. For other
    /// uses, use [`render`](#method.render) instead.
    ///
    /// The `context` can be of any type that implements `Serialize`. This is
    /// typically a `HashMap` or a custom `struct`. The path `root` can be
    /// relative, in which case it is relative to the current working directory,
    /// or absolute.
    ///
    /// Returns `Some` if the template could be rendered. Otherwise, returns
    /// `None`. If rendering fails, error output is printed to the console.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::collections::HashMap;
    /// use rocket_contrib::Template;
    ///
    /// // Create a `context`. Here, just an empty `HashMap`.
    /// let mut context = HashMap::new();
    ///
    /// # context.insert("test", "test");
    /// # #[allow(unused_variables)]
    /// let template = Template::show("templates/", "index", context);
    #[inline]
    pub fn show<P, S, C>(root: P, name: S, context: C) -> Option<String>
        where P: AsRef<Path>, S: Into<Cow<'static, str>>, C: Serialize
    {
        let root = root.as_ref().to_path_buf();
        Context::initialize(root).and_then(|ctxt| {
            Template::render(name, context).finalize(&ctxt).ok().map(|v| v.0)
        })
    }

    #[inline(always)]
    fn finalize(self, ctxt: &Context) -> Result<(String, ContentType), Status> {
        let name = &*self.name;
        let info = ctxt.templates.get(name).ok_or_else(|| {
            let ts: Vec<_> = ctxt.templates.keys().map(|s| s.as_str()).collect();
            error_!("Template '{}' does not exist.", name);
            info_!("Known templates: {}", ts.join(","));
            info_!("Searched in '{:?}'.", ctxt.root);
            Status::InternalServerError
        })?;

        let value = self.value.ok_or_else(|| {
            error_!("The provided template context failed to serialize.");
            Status::InternalServerError
        })?;

        let string = ctxt.engines.render(name, &info, value).ok_or_else(|| {
            error_!("Template '{}' failed to render.", name);
            Status::InternalServerError
        })?;

        Ok((string, info.data_type.clone()))
    }
}

/// Returns a response with the Content-Type derived from the template's
/// extension and a fixed-size body containing the rendered template. If
/// rendering fails, an `Err` of `Status::InternalServerError` is returned.
impl Responder<'static> for Template {
    fn respond_to(self, req: &Request) -> response::Result<'static> {
        let ctxt = req.guard::<State<Context>>().succeeded().ok_or_else(|| {
            error_!("Uninitialized template context: missing fairing.");
            info_!("To use templates, you must attach `Template::fairing()`.");
            info_!("See the `Template` documentation for more information.");
            Status::InternalServerError
        })?;
        #[cfg(debug_assertions)]
        let (render,content_type) = {
            match Context::initialize(&ctxt.root) {
                Some(ctxt) => self.finalize(&ctxt)?;
                None => {
                    error_!("The template context failed to initialize.");
                    return Status::InternalServerError;
                }
            }
        };
        #[cfg(not(debug_assertions))]
        let (render, content_type) = self.finalize(&ctxt)?;
        Content(content_type, render).respond_to(req)
    }
}
