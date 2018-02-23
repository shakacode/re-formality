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
  let strategy = Formality.Strategy.OnFirstSuccessOrFirstBlur;
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
             },
             validateAsync:
               Some(
                 value =>
                   Js.Promise.(
                     value
                     |> Api.validateEmail
                     |> then_(valid =>
                          valid ?
                            resolve(
                              ValidityBag({
                                valid: true,
                                tag: None,
                                message: "Nice!"
                              })
                            ) :
                            resolve(
                              ValidityBag({
                                valid: false,
                                tag: None,
                                message: "Email is already taken"
                              })
                            )
                        )
                   )
               )
           }
         )
      |> Validators.add(
           Password,
           {
             strategy: None, /* None means global will be used */
             dependents: Some([PasswordConfirmation]),
             validate: (value, _) => {
               let minLength = 4;
               let strongLength = 6;
               switch value {
               | "" =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Password is required"
                 })
               | _ when String.length(value) < minLength =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: {j| $(minLength)+ characters, please|j}
                 })
               | _ when String.length(value) < strongLength =>
                 ValidityBag({
                   valid: true,
                   tag: Some("weak"),
                   message: "Can be stronger... (still valid tho)"
                 })
               | _ => ValidityBag({valid: true, tag: None, message: "Nice!"})
               };
             },
             validateAsync: None
           }
         )
      |> Validators.add(
           PasswordConfirmation,
           {
             strategy: None,
             dependents: None,
             validate: (value, state) =>
               switch value {
               | "" =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Confirmation is required"
                 })
               | _ when value !== state.password =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: "Password doesn't match"
                 })
               | _ => ValidityBag({valid: true, tag: None, message: "Match!"})
               },
             validateAsync: None
           }
         )
    );
  /* exception InvalidResult(field); */
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
          Js.log("Called with:");
          Js.log(state);
          Js.log("If api returned error this callback should be called:");
          Js.log(notify.onFailure);
          let _ = Js.Global.setTimeout(notify.onSuccess, 500);
          ();
        }
      )>
      ...(
           form =>
             <form className="form" onSubmit=form.submit>
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
                     onChange=(SignupForm.Email |> form.change)
                     onBlur=(SignupForm.Email |> form.blur)
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
                     | (Some(result), false) =>
                       switch result {
                       | Formality.ValidityBag(bag) =>
                         <div
                           className=(
                             Cn.make([
                               "form-message",
                               bag.valid ? "success" : "failure"
                             ])
                           )>
                           (bag.message |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(Formality.ImpossibleResult)
                       }
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
                     onChange=(SignupForm.Password |> form.change)
                     onBlur=(SignupForm.Password |> form.blur)
                   />
                   (
                     switch (SignupForm.Password |> form.results) {
                     | Some(result) =>
                       switch result {
                       | Formality.ValidityBag(bag) =>
                         <div
                           className=(
                             Cn.make([
                               "form-message",
                               bag.valid ?
                                 switch bag.tag {
                                 | Some(tag) => tag
                                 | None => "success"
                                 } :
                                 "failure"
                             ])
                           )>
                           (bag.message |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(Formality.ImpossibleResult)
                       }
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
                     onChange=(SignupForm.PasswordConfirmation |> form.change)
                     onBlur=(SignupForm.PasswordConfirmation |> form.blur)
                   />
                   (
                     switch (SignupForm.PasswordConfirmation |> form.results) {
                     | Some(result) =>
                       switch result {
                       | Formality.ValidityBag(bag) =>
                         <div
                           className=(
                             Cn.make([
                               "form-message",
                               bag.valid ? "success" : "failure"
                             ])
                           )>
                           (bag.message |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(Formality.ImpossibleResult)
                       }
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
