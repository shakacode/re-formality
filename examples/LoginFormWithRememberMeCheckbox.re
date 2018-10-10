module LoginForm = {
  type value =
    | String(string)
    | Bool(bool);

  type message = string;

  type field =
    | Email
    | Password
    | Remember;

  type state = {
    email: string,
    password: string,
    remember: bool,
  };

  let get = (state, field) =>
    switch (field) {
    | Email => String(state.email)
    | Password => String(state.password)
    | Remember => Bool(state.remember)
    };

  let set = (state, (field, value)) =>
    switch (field, value) {
    | (Email, String(value)) => {...state, email: value}
    | (Password, String(value)) => {...state, password: value}
    | (Remember, Bool(value)) => {...state, remember: value}
    /* TODO: Sadly, we can catch it only at runtime. Something to improve. */
    | _ => failwith("Config.set function received bad input")
    };

  let valueEmpty = value =>
    switch (value) {
    | String(value) when value === "" => true
    | String(_)
    | Bool(_) => false
    };

  let validators =
    Formality.[
      {
        field: Email,
        strategy: Strategy.OnFirstSuccessOrFirstBlur,
        dependents: None,
        validate: (value, _state) => {
          let emailRegex = [%bs.re {|/.*@.*\..+/|}];
          switch (value) {
          | String("") => Invalid("Email is required")
          | String(value) when !emailRegex->Js.Re.test(value, _) =>
            Invalid("Email is invalid")
          | String(_) => Valid
          | _ => failwith("Email validator received bad input") /* sadly, as well */
          };
        },
      },
      {
        field: Password,
        strategy: Strategy.OnFirstBlur,
        dependents: None,
        validate: (value, _state) =>
          switch (value) {
          | String("") => Invalid("Password is required")
          | String(_) => Valid
          | _ => failwith("Password validator received bad input") /* sadly, as well */
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
      initialState={email: "", password: "", remember: false}
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
                         ->String
                         ->(form.change(Email))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##value
                         ->String
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
                         ->String
                         ->(form.change(Password))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##value
                         ->String
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
                   <input
                     id="login--remember"
                     type_="checkbox"
                     checked={form.state.remember}
                     disabled={form.submitting}
                     className="push-lg"
                     onChange={
                       event =>
                         event->ReactEvent.Form.target##checked
                         ->Bool
                         ->(form.change(Remember))
                     }
                     onBlur={
                       event =>
                         event->ReactEvent.Focus.target##checked
                         ->Bool
                         ->(form.blur(Remember))
                     }
                   />
                   <label htmlFor="login--remember">
                     "Remember me"->React.string
                   </label>
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
