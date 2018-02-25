module SignupForm = {
  type field =
    | Email
    | Password
    | PasswordConfirmation;
  type state = {
    email: string,
    password: string,
    passwordConfirmation: string
  };
  type message = string;
  let get = (field, state) =>
    switch field {
    | Email => state.email
    | Password => state.password
    | PasswordConfirmation => state.passwordConfirmation
    };
  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    | (PasswordConfirmation, value) => {...state, passwordConfirmation: value}
    };
  let debounceInterval = Formality.debounceInterval;
  module Validators =
    Formality.MakeValidators(
      {
        type t = field;
      }
    );
  type validators =
    Validators.t(Formality.asyncValidator(field, state, message));
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
             },
             validateAsync:
               Some(
                 value =>
                   Js.Promise.(
                     value
                     |> Api.validateEmail
                     |> then_(valid =>
                          valid ?
                            resolve({
                              valid: true,
                              message: Some("Nice!"),
                              meta: None
                            }) :
                            resolve({
                              valid: false,
                              message: Some("Email is already taken"),
                              meta: None
                            })
                        )
                   )
               )
           }
         )
      |> Validators.add(
           Password,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: Some([PasswordConfirmation]),
             validate: (value, _) => {
               let minLength = 4;
               let strongLength = 6;
               switch value {
               | "" => {
                   valid: false,
                   message: Some("Password is required"),
                   meta: None
                 }
               | _ when String.length(value) < minLength => {
                   valid: false,
                   message: Some({j| $(minLength)+ characters, please|j}),
                   meta: None
                 }
               | _ when String.length(value) < strongLength => {
                   valid: true,
                   message: Some("Can be stronger... (still valid tho)"),
                   meta: Some("weak")
                 }
               | _ => {valid: true, message: Some("Nice!"), meta: None}
               };
             },
             validateAsync: None
           }
         )
      |> Validators.add(
           PasswordConfirmation,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: None,
             validate: (value, state) =>
               switch value {
               | "" => {
                   valid: false,
                   message: Some("Confirmation is required"),
                   meta: None
                 }
               | _ when value !== state.password => {
                   valid: false,
                   message: Some("Password doesn't match"),
                   meta: None
                 }
               | _ => {valid: true, message: Some("Match!"), meta: None}
               },
             validateAsync: None
           }
         )
    );
};

module SignupFormContainer =
  Formality.MakeWithAsyncValidationsOnChange(SignupForm);

let component = "SignupForm" |> ReasonReact.statelessComponent;

let make = (_) => {
  ...component,
  render: (_) =>
    <SignupFormContainer
      initialState={email: "", password: "", passwordConfirmation: ""}
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
                   ("Signup" |> ReasonReact.stringToElement)
                 </h2>
                 <div className="form-row">
                   <label htmlFor="signup--email" className="label-lg">
                     ("Email" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="signup--email"
                     value=form.state.email
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)
                     onChange=(
                       SignupForm.Email
                       |> form.change
                       |> Formality.Dom.valueOnChange
                     )
                     onBlur=(
                       SignupForm.Email
                       |> form.blur
                       |> Formality.Dom.valueOnBlur
                     )
                   />
                   (
                     switch (
                       SignupForm.Email |> form.results,
                       SignupForm.Email |> form.validating
                     ) {
                     | (_, true) =>
                       <div className="form-message">
                         ("Checking..." |> ReasonReact.stringToElement)
                       </div>
                     | (Some({valid, message: Some(message)}), false) =>
                       <div
                         className=(
                           Cn.make([
                             "form-message",
                             valid ? "success" : "failure"
                           ])
                         )>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | (Some(_), false) => raise(Formality.ImpossibleResult)
                     | (None, false) => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row form-row-footer">
                   <div className="note push-lg">
                     (
                       "Hint: try `test@taken.email`"
                       |> ReasonReact.stringToElement
                     )
                   </div>
                 </div>
                 <div className="form-row">
                   <label htmlFor="signup--password" className="label-lg">
                     ("Password" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="signup--password"
                     value=form.state.password
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)
                     onChange=(
                       SignupForm.Password
                       |> form.change
                       |> Formality.Dom.valueOnChange
                     )
                     onBlur=(
                       SignupForm.Password
                       |> form.blur
                       |> Formality.Dom.valueOnBlur
                     )
                   />
                   (
                     switch (SignupForm.Password |> form.results) {
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
                     | Some(_) => raise(Formality.ImpossibleResult)
                     | None => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row">
                   <label
                     htmlFor="signup--passwordConfirmation"
                     className="label-lg">
                     ("Confirmation" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="signup--passwordConfirmation"
                     value=form.state.passwordConfirmation
                     disabled=(form.submitting |> Js.Boolean.to_js_boolean)
                     onChange=(
                       SignupForm.PasswordConfirmation
                       |> form.change
                       |> Formality.Dom.valueOnChange
                     )
                     onBlur=(
                       SignupForm.PasswordConfirmation
                       |> form.blur
                       |> Formality.Dom.valueOnBlur
                     )
                   />
                   (
                     switch (SignupForm.PasswordConfirmation |> form.results) {
                     | Some({valid, message: Some(message)}) =>
                       <div
                         className=(
                           Cn.make([
                             "form-message",
                             valid ? "success" : "failure"
                           ])
                         )>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | Some(_) => raise(Formality.ImpossibleResult)
                     | None => ReasonReact.nullElement
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
    </SignupFormContainer>
};
