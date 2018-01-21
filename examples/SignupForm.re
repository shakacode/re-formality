let component = ReasonReact.statelessComponent("SignupForm");

let make = (_) => {
  ...component,
  render: (_) => <div> ("Signup Form" |> ReasonReact.stringToElement) </div>
};
