module LoginForm = {
  type value = string;
  type message = string;

  type field =
    | Email
    | Password;

  type state = {
    email: string,
    password: string,
  };

  let get = (state, field) =>
    switch (field) {
    | Email => state.email
    | Password => state.password
    };

  let set = (state, (field, value)) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    };

  let valueEmpty = Formality.emptyString;

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
      },
      {
        field: Password,
        strategy: Strategy.OnFirstBlur,
        dependents: None,
        validate: (value, _state) =>
          switch (value) {
          | "" => Invalid("Password is required")
          | _ => Valid
          },
      },
    ];
};

module LoginFormContainer = Formality.Make(LoginForm);

let component = React.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <LoginFormContainer
      initialState={email: "", password: ""}
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
                 <h2 className="push-lg"> "Login"->React.string </h2>
                 <div className="form-row">
                   <label htmlFor="login--email" className="label-lg">
                     "Email"->React.string
                   </label>
                   <input
                     id="login--email"
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
                     switch (Email->(form.results)) {
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
                   <label htmlFor="login--password" className="label-lg">
                     "Password"->React.string
                   </label>
                   <input
                     id="login--password"
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
                   <button className="push-lg" disabled={form.submitting}>
                     (form.submitting ? "Submitting..." : "Submit")
                     ->React.string
                   </button>
                   {
                     switch (form.status) {
                     | Submitted =>
                       <div className={Cn.make(["form-status", "success"])}>
                         {j|✓ Logged In|j}->React.string
                       </div>
                     | _ => React.null
                     }
                   }
                 </div>
               </div>
             </form>
         }
    </LoginFormContainer>,
};
