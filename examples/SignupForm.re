module SignupForm = {
  type value = string;
  type message = string;

  type field =
    | Email
    | Password
    | PasswordConfirmation;

  type state = {
    email: string,
    password: string,
    passwordConfirmation: string,
  };

  let get = (state, field) =>
    switch (field) {
    | Email => state.email
    | Password => state.password
    | PasswordConfirmation => state.passwordConfirmation
    };

  let set = (state, (field, value)) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    | (PasswordConfirmation, value) => {
        ...state,
        passwordConfirmation: value,
      }
    };

  let valueEmpty = Formality.emptyString;
  let debounceInterval = Formality.debounceInterval;

  let validators =
    Formality.[
      {
        field: Email,
        strategy: Strategy.OnFirstSuccessOrFirstBlur,
        dependents: None,
        validate: (value, _state) => {
          let emailRegex = [%bs.re {|/.*@.*\..+/|}];
          switch (value) {
          | "" => Invalid("Email is required")
          | _ when !emailRegex->Js.Re.test(value, _) =>
            Invalid("Email is invalid")
          | _ => Valid
          };
        },
        validateAsync:
          Some(
            value =>
              Js.Promise.(
                value
                ->Api.validateEmail
                ->then_(
                    valid =>
                      valid ?
                        Valid->resolve :
                        Invalid("Email is already taken")->resolve,
                    _,
                  )
              ),
          ),
      },
      {
        field: Password,
        strategy: Strategy.OnFirstSuccessOrFirstBlur,
        dependents: [PasswordConfirmation]->Some,
        validate: (value, _state) => {
          let minLength = 4;
          switch (value) {
          | "" => Invalid("Password is required")
          | _ when value->Js.String.length < minLength =>
            Invalid({j| $(minLength)+ characters, please|j})
          | _ => Valid
          };
        },
        validateAsync: None,
      },
      {
        field: PasswordConfirmation,
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
    ];
};

module SignupFormContainer =
  Formality.MakeWithAsyncValidationsOnChange(SignupForm);

let component = React.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <SignupFormContainer
      initialState={email: "", password: "", passwordConfirmation: ""}
      onSubmit={
        (state, form) => {
          Js.log2("Called with:", state);
          Js.Global.setTimeout(
            () => {
              form.notifyOnSuccess(None);
              form.reset->Js.Global.setTimeout(3000)->ignore;
            },
            500,
          )
          ->ignore;
        }
      }>
      ...{
           form =>
             <form
               className="form"
               onSubmit={form.submit->Formality.Dom.preventDefault}>
               <div className="form-messages-area form-messages-area-lg" />
               <div className="form-content">
                 <h2 className="push-lg"> "Signup"->React.string </h2>
                 <div className="form-row">
                   <label htmlFor="signup--email" className="label-lg">
                     "Email"->React.string
                   </label>
                   <input
                     id="signup--email"
                     type_="text"
                     value={form.state.email}
                     disabled={form.submitting}
                     onChange={
                       event =>
                         event->ReactEvent.Form.target##value
                         ->(form.change(Email))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##value
                         ->(form.blur(Email))
                     }
                   />
                   {
                     switch (Email->(form.results), Email->(form.validating)) {
                     | (_, true) =>
                       <div className="form-message">
                         "Checking..."->React.string
                       </div>
                     | (Some(Invalid(message)), false) =>
                       <div className={Cn.make(["form-message", "failure"])}>
                         message->React.string
                       </div>
                     | (Some(Valid), false) =>
                       <div className={Cn.make(["form-message", "success"])}>
                         {j|✓|j}->React.string
                       </div>
                     | (None, false) => React.null
                     }
                   }
                 </div>
                 <div className="form-row form-row-footer">
                   <div className="note push-lg">
                     "Hint: try `test@taken.email`"->React.string
                   </div>
                 </div>
                 <div className="form-row">
                   <label htmlFor="signup--password" className="label-lg">
                     "Password"->React.string
                   </label>
                   <input
                     id="signup--password"
                     type_="text"
                     value={form.state.password}
                     disabled={form.submitting}
                     onChange={
                       event =>
                         event->ReactEvent.Form.target##value
                         ->(form.change(Password))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##value
                         ->(form.blur(Password))
                     }
                   />
                   {
                     switch (Password->(form.results)) {
                     | Some(Invalid(message)) =>
                       <div className={Cn.make(["form-message", "failure"])}>
                         message->React.string
                       </div>
                     | Some(Valid) =>
                       <div className={Cn.make(["form-message", "success"])}>
                         {j|✓|j}->React.string
                       </div>
                     | None => React.null
                     }
                   }
                 </div>
                 <div className="form-row">
                   <label
                     htmlFor="signup--passwordConfirmation"
                     className="label-lg">
                     "Confirmation"->React.string
                   </label>
                   <input
                     id="signup--passwordConfirmation"
                     type_="text"
                     value={form.state.passwordConfirmation}
                     disabled={form.submitting}
                     onChange={
                       event =>
                         event->ReactEvent.Form.target##value
                         ->(form.change(PasswordConfirmation))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##value
                         ->(form.blur(PasswordConfirmation))
                     }
                   />
                   {
                     switch (PasswordConfirmation->(form.results)) {
                     | Some(Invalid(message)) =>
                       <div className={Cn.make(["form-message", "failure"])}>
                         message->React.string
                       </div>
                     | Some(Valid) =>
                       <div className={Cn.make(["form-message", "success"])}>
                         {j|✓|j}->React.string
                       </div>
                     | None => React.null
                     }
                   }
                 </div>
                 <div className="form-row">
                   <button className="push-lg" disabled={form.submitting}>
                     (form.submitting ? "Submitting..." : "Submit")
                     ->React.string
                   </button>
                   {
                     switch (form.status) {
                     | Submitted =>
                       <div className={Cn.make(["form-status", "success"])}>
                         {j|✓ Signed Up|j}->React.string
                       </div>
                     | _ => React.null
                     }
                   }
                 </div>
               </div>
             </form>
         }
    </SignupFormContainer>,
};
