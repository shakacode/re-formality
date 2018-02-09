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
  let strategy = Formality.Strategy.OnFirstSuccessOrFirstBlur;
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
             strategy: None, /* None means global will be used */
             dependents: None,
             validate: (value, _) => {
               let emailRegex = [%bs.re {|/.*@.*\..+/|}];
               switch value {
               | "" =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Email is required"
                 })
               | _ when ! (emailRegex |> Js.Re.test(value)) =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Email is invalid"
                 })
               | _ => ValidityBag({valid: true, tag: None, message: "Nice!"})
               };
             }
           }
         )
      |> Validators.add(
           Password,
           {
             strategy: Some(Strategy.OnFirstBlur),
             dependents: None,
             validate: (value, _) =>
               switch value {
               | "" =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Password is required"
                 })
               | _ => ValidityBag({valid: true, tag: None, message: "Nice!"})
               }
           }
         )
    );
  exception InvalidResult(field);
};

module LoginFormContainer = Formality.Make(LoginForm);

let component = ReasonReact.statelessComponent("LoginForm");

let make = (_) => {
  ...component,
  render: (_) =>
    <LoginFormContainer
      initialState={email: "", password: ""}
      onSubmit=(
        (~notifyOnSuccess, ~notifyOnFailure, state) => {
          Js.log("Called with:");
          Js.log(state);
          Js.log("If api returned error this callback should be called:");
          Js.log(notifyOnFailure);
          let _ = Js.Global.setTimeout(notifyOnSuccess, 500);
          ();
        }
      )>
      ...(
           ({state, results, change, blur, submit, submitting}) =>
             <form className="form" onSubmit=submit>
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
                     value=state.email
                     disabled=(submitting |> Js.Boolean.to_js_boolean)
                     onChange=(change(LoginForm.Email))
                     onBlur=(blur(LoginForm.Email))
                   />
                   (
                     switch (LoginForm.Email |> results) {
                     | Some(result) =>
                       switch result {
                       | Formality.ValidityBag(validity) =>
                         <div
                           className=(
                             Cn.make([
                               "form-message",
                               validity.valid ? "success" : "failure"
                             ])
                           )>
                           (validity.message |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(LoginForm.InvalidResult(LoginForm.Email))
                       }
                     | None => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row">
                   <label htmlFor="login--password" className="label-lg">
                     ("Password" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="login--password"
                     value=state.password
                     disabled=(submitting |> Js.Boolean.to_js_boolean)
                     onChange=(change(LoginForm.Password))
                     onBlur=(blur(LoginForm.Password))
                   />
                   (
                     switch (LoginForm.Password |> results) {
                     | Some(result) =>
                       switch result {
                       | Formality.ValidityBag(validity) =>
                         <div
                           className=(
                             Cn.make([
                               "form-message",
                               validity.valid ?
                                 switch validity.tag {
                                 | Some(tag) => tag
                                 | None => "success"
                                 } :
                                 "failure"
                             ])
                           )>
                           (validity.message |> ReasonReact.stringToElement)
                         </div>
                       | _ =>
                         raise(LoginForm.InvalidResult(LoginForm.Password))
                       }
                     | None => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row">
                   <button
                     className="push-lg"
                     disabled=(submitting |> Js.Boolean.to_js_boolean)>
                     (
                       (submitting ? "Submitting..." : "Submit")
                       |> ReasonReact.stringToElement
                     )
                   </button>
                 </div>
               </div>
             </form>
         )
    </LoginFormContainer>
};
