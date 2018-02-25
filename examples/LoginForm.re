module LoginForm = {
  type field =
    | Email
    | Password;
  type state = {
    email: string,
    password: string
  };
  type message = string;
  let get = (field, state) =>
    switch field {
    | Email => state.email
    | Password => state.password
    };
  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    };
  module Validators =
    Formality.MakeValidators(
      {
        type t = field;
      }
    );
  type validators = Validators.t(Formality.validator(field, state, message));
  let validators =
    Formality.(
      Validators.empty
      |> Validators.add(
           Email,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: None,
             validate: (value, _) => {
               let emailRegex = [%bs.re {|/.*@.*\..+/|}];
               switch value {
               | "" => {
                   valid: false,
                   message: Some("Email is required"),
                   meta: None
                 }
               | _ when ! (emailRegex |> Js.Re.test(value)) => {
                   valid: false,
                   message: Some("Email is invalid"),
                   meta: None
                 }
               | _ => {valid: true, message: Some("Nice!"), meta: None}
               };
             }
           }
         )
      |> Validators.add(
           Password,
           {
             strategy: Strategy.OnFirstBlur,
             dependents: None,
             validate: (value, _) =>
               switch value {
               | "" => {
                   valid: false,
                   message: Some("Password is required"),
                   meta: None
                 }
               | _ => {valid: true, message: Some("Nice!"), meta: None}
               }
           }
         )
    );
};

module LoginFormContainer = Formality.Make(LoginForm);

let component = "LoginForm" |> ReasonReact.statelessComponent;

let make = (_) => {
  ...component,
  render: (_) =>
    <LoginFormContainer
      initialState={email: "", password: ""}
      onSubmit=(
        (state, notify) => {
          Js.log2("Called with:", state);
          Js.log2(
            "If api returned error this callback should be called:",
            notify.onFailure
          );
          let _ = Js.Global.setTimeout(notify.onSuccess, 500);
          ();
        }
      )>
      ...(
           form =>
             <form
               className="form"
               onSubmit=(form.submit |> Formality.Dom.preventDefault)>
               <div className="form-messages-area form-messages-area-lg" />
               <div className="form-content">
                 <h2 className="push-lg">
                   ("Login" |> ReasonReact.stringToElement)
                 </h2>
                 <div className="form-row">
                   <label htmlFor="login--email" className="label-lg">
                     ("Email" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="login--email"
                     value=form.state.email
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)
                     onChange=(
                       LoginForm.Email
                       |> form.change
                       |> Formality.Dom.valueOnChange
                     )
                     onBlur=(
                       LoginForm.Email
                       |> form.blur
                       |> Formality.Dom.valueOnBlur
                     )
                   />
                   (
                     switch (LoginForm.Email |> form.results) {
                     | Some({valid: true, message: Some(message)}) =>
                       <div className=(Cn.make(["form-message", "success"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | Some({valid: false, message: Some(message)}) =>
                       <div className=(Cn.make(["form-message", "failure"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | None => ReasonReact.nullElement
                     | _ => raise(Formality.ImpossibleResult)
                     }
                   )
                 </div>
                 <div className="form-row">
                   <label htmlFor="login--password" className="label-lg">
                     ("Password" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="login--password"
                     value=form.state.password
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)
                     onChange=(
                       LoginForm.Password
                       |> form.change
                       |> Formality.Dom.valueOnChange
                     )
                     onBlur=(
                       LoginForm.Password
                       |> form.blur
                       |> Formality.Dom.valueOnBlur
                     )
                   />
                   (
                     switch (LoginForm.Password |> form.results) {
                     | Some({valid: true, message: Some(message), meta}) =>
                       <div
                         className=(
                           Cn.make([
                             "form-message",
                             switch meta {
                             | Some(meta) => meta
                             | None => "success"
                             }
                           ])
                         )>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | Some({valid: false, message: Some(message)}) =>
                       <div className=(Cn.make(["form-message", "failure"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | None => ReasonReact.nullElement
                     | _ => raise(Formality.ImpossibleResult)
                     }
                   )
                 </div>
                 <div className="form-row">
                   <button
                     className="push-lg"
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)>
                     (
                       (form.submitting ? "Submitting..." : "Submit")
                       |> ReasonReact.stringToElement
                     )
                   </button>
                 </div>
               </div>
             </form>
         )
    </LoginFormContainer>
};
