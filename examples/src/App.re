module Route = {
  type t =
    | Signup
    | Login
    | BlogPost;

  let fromUrl = (url: ReasonReactRouter.url) =>
    switch (url.hash) {
    | "signup" => Signup
    | "login" => Login
    | "blog-post" => BlogPost
    | _ => Signup
    };
};

[@react.component]
let make = () => {
  let route = ReasonReactRouter.useUrl()->Route.fromUrl;

  <div className="container">
    <div className="header">
      <h1> "Formality"->React.string </h1>
      <a href="https://github.com/alexfedoseev/re-formality" className="link">
        "Github"->React.string
      </a>
    </div>
    <div className="nav">
      <button
        className={Cn.make([
          "primary",
          "nav-link",
          switch (route) {
          | Signup => "active"
          | _ => ""
          },
        ])}
        onClick={_ => ReasonReactRouter.push("#signup")}>
        "Signup"->React.string
      </button>
      <button
        className={Cn.make([
          "primary",
          "nav-link",
          switch (route) {
          | Login => "active"
          | _ => ""
          },
        ])}
        onClick={_ => ReasonReactRouter.push("#login")}>
        "Login"->React.string
      </button>
      <button
        className={Cn.make([
          "primary",
          "nav-link",
          switch (route) {
          | BlogPost => "active"
          | _ => ""
          },
        ])}
        onClick={_ => ReasonReactRouter.push("#blog-post")}>
        "Blog Post"->React.string
      </button>
    </div>
    {switch (route) {
     | Signup => <SignupForm />
     | Login => <LoginForm />
     | BlogPost => <BlogPostForm />
     }}
  </div>;
};
