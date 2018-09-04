[@bs.val] external locationHash: string = "window.location.hash";

type route =
  | Signup
  | Login
  | LoginFormWithRememberMe;

type state = {route};

type action =
  | UpdateRoute(route);

let getInitialRoute = () =>
  switch (locationHash) {
  | "#signup" => Signup
  | "#login" => Login
  | "#login-remember-me" => LoginFormWithRememberMe
  | _ => Signup
  };

let component = ReasonReact.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {route: getInitialRoute()},
  reducer: (action, _) =>
    switch (action) {
    | UpdateRoute(route) => ReasonReact.Update({route: route})
    },
  didMount: ({send, onUnmount}) => {
    let watcherID =
      ReasonReact.Router.watchUrl(url =>
        switch (url.hash) {
        | "signup" => UpdateRoute(Signup) |> send
        | "login" => UpdateRoute(Login) |> send
        | "login-remember-me" => UpdateRoute(LoginFormWithRememberMe) |> send
        | _ => UpdateRoute(Signup) |> send
        }
      );
    onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },
  render: ({state}) =>
    <div className="container">
      <div className="header">
        <h1> {"Formality" |> ReasonReact.string} </h1>
        <a
          href="https://github.com/alexfedoseev/re-formality" className="link">
          {"Github" |> ReasonReact.string}
        </a>
      </div>
      <div className="nav">
        <button
          className={
            Cn.make([
              "nav-link",
              switch (state.route) {
              | Signup => "active"
              | _ => ""
              },
            ])
          }
          onClick={_ => ReasonReact.Router.push("#signup")}>
          {"Signup" |> ReasonReact.string}
        </button>
        <button
          className={
            Cn.make([
              "nav-link",
              switch (state.route) {
              | Login => "active"
              | _ => ""
              },
            ])
          }
          onClick={_ => ReasonReact.Router.push("#login")}>
          {"Login" |> ReasonReact.string}
        </button>
        <button
          className={
            Cn.make([
              "nav-link",
              switch (state.route) {
              | LoginFormWithRememberMe => "active"
              | _ => ""
              },
            ])
          }
          onClick={_ => ReasonReact.Router.push("#login-remember-me")}>
          {"Login With Remember Me" |> ReasonReact.string}
        </button>
      </div>
      {
        switch (state.route) {
        | Signup => <SignupForm />
        | Login => <LoginForm />
        | LoginFormWithRememberMe => <LoginFormWithRememberMeCheckbox />
        }
      }
    </div>,
};
