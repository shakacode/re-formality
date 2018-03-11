module SignupForm = {
  type field =
    | Email
    | Password
    | PasswordConfirmation;
  type state = {
    email: string,
    password: string,
    passwordConfirmation: string,
  };
  type message = string;
  let get = (field, state) =>
    switch (field) {
    | Email => state.email
    | Password => state.password
    | PasswordConfirmation => state.passwordConfirmation
    };
  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    | (PasswordConfirmation, value) => {
        ...state,
        passwordConfirmation: value,
      }
    };
  let debounceInterval = Formality.debounceInterval;
  module Validators =
    Formality.MakeValidators(
      {
        type t = field;
      },
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
               switch (value) {
               | "" => Invalid("Email is required")
               | _ when ! (emailRegex |> Js.Re.test(value)) =>
                 Invalid("Email is invalid")
               | _ => Valid
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
                            resolve(Valid) :
                            resolve(Invalid("Email is already taken"))
                        )
                   ),
               ),
           },
         )
      |> Validators.add(
           Password,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: Some([PasswordConfirmation]),
             validate: (value, _) => {
               let minLength = 4;
               switch (value) {
               | "" => Invalid("Password is required")
               | _ when String.length(value) < minLength =>
                 Invalid({j| $(minLength)+ characters, please|j})
               | _ => Valid
               };
             },
             validateAsync: None,
           },
         )
      |> Validators.add(
           PasswordConfirmation,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: None,
             validate: (value, state) =>
               switch (value) {
               | "" => Invalid("Confirmation is required")
               | _ when value !== state.password =>
                 Invalid("Password doesn't match")
               | _ => Valid
               },
             validateAsync: None,
           },
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
            notify.onFailure,
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
                       SignupForm.Email |> form.validating,
                     ) {
                     | (_, true) =>
                       <div className="form-message">
                         ("Checking..." |> ReasonReact.stringToElement)
                       </div>
                     | (Some(Invalid(message)), false) =>
                       <div className=(Cn.make(["form-message", "failure"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | (Some(Valid), false) =>
                       <div className=(Cn.make(["form-message", "success"]))>
                         ({j|✓|j} |> ReasonReact.stringToElement)
                       </div>
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
                     | Some(Invalid(message)) =>
                       <div className=(Cn.make(["form-message", "failure"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | Some(Valid) =>
                       <div className=(Cn.make(["form-message", "success"]))>
                         ({j|✓|j} |> ReasonReact.stringToElement)
                       </div>
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
                     | Some(Invalid(message)) =>
                       <div className=(Cn.make(["form-message", "failure"]))>
                         (message |> ReasonReact.stringToElement)
                       </div>
                     | Some(Valid) =>
                       <div className=(Cn.make(["form-message", "success"]))>
                         ({j|✓|j} |> ReasonReact.stringToElement)
                       </div>
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
    </SignupFormContainer>,
};
