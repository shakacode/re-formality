[@bs.val] external locationHash : string = "window.location.hash";

type route =
  | Signup
  | Login;

type state = {route};

type action =
  | UpdateRoute(route);

let getInitialRoute = () =>
  switch (locationHash) {
  | "#signup" => Signup
  | "#login" => Login
  | _ => Signup
  };

let component = "App" |> ReasonReact.reducerComponent;

let make = (_) => {
  ...component,
  initialState: () => {route: getInitialRoute()},
  reducer: (action, _) =>
    switch (action) {
    | UpdateRoute(route) => ReasonReact.Update({route: route})
    },
  subscriptions: ({send}) => [
    Sub(
      () =>
        ReasonReact.Router.watchUrl(url =>
          switch (url.hash) {
          | "signup" => send(UpdateRoute(Signup))
          | "login" => send(UpdateRoute(Login))
          | _ => send(UpdateRoute(Signup))
          }
        ),
      ReasonReact.Router.unwatchUrl,
    ),
  ],
  render: ({state}) =>
    <div className="container">
      <div className="header">
        <h1> ("Formality" |> ReasonReact.stringToElement) </h1>
        <a
          href="https://github.com/alexfedoseev/re-formality" className="link">
          ("Github" |> ReasonReact.stringToElement)
        </a>
      </div>
      <div className="nav">
        <button
          className=(
            Cn.make([
              "nav-link",
              switch (state.route) {
              | Signup => "active"
              | _ => ""
              },
            ])
          )
          onClick=((_) => ReasonReact.Router.push("#signup"))>
          ("Signup" |> ReasonReact.stringToElement)
        </button>
        <button
          className=(
            Cn.make([
              "nav-link",
              switch (state.route) {
              | Login => "active"
              | _ => ""
              },
            ])
          )
          onClick=((_) => ReasonReact.Router.push("#login"))>
          ("Login" |> ReasonReact.stringToElement)
        </button>
      </div>
      (
        switch (state.route) {
        | Signup => <SignupForm />
        | Login => <LoginForm />
        }
      )
    </div>,
};
