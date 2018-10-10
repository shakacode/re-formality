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

let component = React.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {route: getInitialRoute()},
  reducer: (action, _) =>
    switch (action) {
    | UpdateRoute(route) => React.Update({route: route})
    },
  didMount: ({send, onUnmount}) => {
    let watcherID =
      React.Router.watchUrl(url =>
        switch (url.hash) {
        | "signup" => Signup->UpdateRoute->send
        | "login" => Login->UpdateRoute->send
        | "login-remember-me" => LoginFormWithRememberMe->UpdateRoute->send
        | _ => Signup->UpdateRoute->send
        }
      );
    onUnmount(() => watcherID->React.Router.unwatchUrl);
  },
  render: ({state}) =>
    <div className="container">
      <div className="header">
        <h1> "Formality"->React.string </h1>
        <a
          href="https://github.com/alexfedoseev/re-formality" className="link">
          "Github"->React.string
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
          onClick={_ => React.Router.push("#signup")}>
          "Signup"->React.string
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
          onClick={_ => React.Router.push("#login")}>
          "Login"->React.string
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
          onClick={_ => React.Router.push("#login-remember-me")}>
          "Login With Remember Me"->React.string
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
