type route =
  | Signup
  | Login;

type state = {route};

type action =
  | UpdateRoute(route);

[@react.component]
let make = () => {
  let initialRoute =
    React.useMemo1(
      () => {
        let url = ReasonReactRouter.dangerouslyGetInitialUrl();
        switch (url.hash) {
        | "signup" => Signup
        | "login" => Login
        | _ => Signup
        };
      },
      [||],
    );

  let (state, dispatch) =
    React.useReducer(
      (_, action) =>
        switch (action) {
        | UpdateRoute(nextRoute) => {route: nextRoute}
        },
      {route: initialRoute},
    );

  React.useEffect1(
    () => {
      let watcherID =
        ReasonReactRouter.watchUrl(url =>
          switch (url.hash) {
          | "signup" => Signup->UpdateRoute->dispatch
          | "login" => Login->UpdateRoute->dispatch
          | _ => Signup->UpdateRoute->dispatch
          }
        );
      Some(() => watcherID->ReasonReactRouter.unwatchUrl);
    },
    [||],
  );

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
          "nav-link",
          switch (state.route) {
          | Signup => "active"
          | _ => ""
          },
        ])}
        onClick={_ => ReasonReactRouter.push("#signup")}>
        "Signup"->React.string
      </button>
      <button
        className={Cn.make([
          "nav-link",
          switch (state.route) {
          | Login => "active"
          | _ => ""
          },
        ])}
        onClick={_ => ReasonReactRouter.push("#login")}>
        "Login"->React.string
      </button>
    </div>
    {switch (state.route) {
     | Signup => React.null
     | Login => <LoginForm />
     }}
  </div>;
};
