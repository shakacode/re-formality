module SignupForm = {
  type field =
    | Email
    | Password;
  type state = {
    email: string,
    password: string
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
  type validators = Validators.t(Formality.validator(state));
  let validators =
    Formality.(
      Validators.empty
      |> Validators.add(
           Email,
           {
             strategy: None, /* None means global will be used */
             validate: (value', state) => {
               let emailRegex = [%bs.re {|/.*@.*\..+/|}];
               let value = value' |> Js.Option.getWithDefault(state.email);
               switch value {
               | "" => ValidityBag({valid: false, tag: None, message: Some("Email is required")})
               | _ when ! (emailRegex |> Js.Re.test(value)) =>
                 ValidityBag({valid: false, tag: None, message: Some("Email is invalid")})
               | _ => ValidityBag({valid: true, tag: None, message: Some("Nice!")})
               }
             }
           }
         )
      |> Validators.add(
           Password,
           {
             strategy: None, /* None means global will be used */
             validate: (value', state) => {
               let minLength = 4;
               let strongLength = 6;
               let value = value' |> Js.Option.getWithDefault(state.password);
               switch value {
               | "" =>
                 ValidityBag({valid: false, tag: None, message: Some("Password is required")})
               | _ when String.length(value) < minLength =>
                 ValidityBag({
                   valid: false,
                   tag: None,
                   message: Some({j| $(minLength)+ characters, please|j})
                 })
               | _ when String.length(value) < strongLength =>
                 ValidityBag({
                   valid: true,
                   tag: Some("weak"),
                   message: Some("Can be stronger... (still valid tho)")
                 })
               | _ => ValidityBag({valid: true, tag: None, message: Some("Nice!")})
               }
             }
           }
         )
    );
  exception InvalidResult(field);
};

module Container = Formality.Make(SignupForm);

let component = ReasonReact.statelessComponent("SignupForm");

let make = (_) => {
  ...component,
  render: (_) =>
    <Container
      initialState={email: "", password: ""}
      onSubmit=(
        (~notifyOnSuccess, ~notifyOnFailure, state) => {
          Js.log("Called with:");
          Js.log(state);
          Js.log("If api returned error this callback should be called:");
          Js.log(notifyOnFailure);
          let _ = Js.Global.setTimeout(notifyOnSuccess, 500);
          ()
        }
      )>
      ...(
           ({state, results, update, blur, submit, submitting}) =>
             <form className="form" onSubmit=submit>
               <div className="form-messages-area form-messages-area-lg" />
               <div className="form-content">
                 <h2 className="push-lg"> ("Signup" |> ReasonReact.stringToElement) </h2>
                 <div className="form-row">
                   <label htmlFor="signup--email" className="label-lg">
                     ("Email" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="signup--email"
                     value=state.email
                     disabled=(submitting |> Js.Boolean.to_js_boolean)
                     onChange=(update(SignupForm.Email))
                     onBlur=(blur(SignupForm.Email))
                   />
                   (
                     switch (SignupForm.Email |> results) {
                     | Some(result) =>
                       switch result {
                       | Formality.ValidityBag(validity) =>
                         <div
                           className=(
                             Cn.make(["form-message", validity.valid ? "success" : "failure"])
                           )>
                           (validity.message |> Js.Option.getExn |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(SignupForm.InvalidResult(SignupForm.Email))
                       }
                     | None => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row">
                   <label htmlFor="signup--password" className="label-lg">
                     ("Password" |> ReasonReact.stringToElement)
                   </label>
                   <input
                     id="signup--password"
                     value=state.password
                     disabled=(submitting |> Js.Boolean.to_js_boolean)
                     onChange=(update(SignupForm.Password))
                     onBlur=(blur(SignupForm.Password))
                   />
                   (
                     switch (SignupForm.Password |> results) {
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
                           (validity.message |> Js.Option.getExn |> ReasonReact.stringToElement)
                         </div>
                       | _ => raise(SignupForm.InvalidResult(SignupForm.Password))
                       }
                     | None => ReasonReact.nullElement
                     }
                   )
                 </div>
                 <div className="form-row">
                   <button className="push-lg" disabled=(submitting |> Js.Boolean.to_js_boolean)>
                     ((submitting ? "Submitting..." : "Submit") |> ReasonReact.stringToElement)
                   </button>
                 </div>
               </div>
             </form>
         )
    </Container>
};
