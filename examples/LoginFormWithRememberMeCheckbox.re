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

  let get = (field, state) =>
    switch (field) {
    | Email => String(state.email)
    | Password => String(state.password)
    | Remember => Bool(state.remember)
    };

  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, String(value)) => {...state, email: value}
    | (Password, String(value)) => {...state, password: value}
    | (Remember, Bool(value)) => {...state, remember: value}
    /* TODO: Sadly, we can catch it only at runtime. Something to improve. */
    | _ => failwith("Config.update function received bad input")
    };

  let valueEmpty = value =>
    switch (value) {
    | String(value) when value === "" => true
    | String(_)
    | Bool(_) => false
    };

  module Validators =
    Formality.MakeValidators({
      type t = field;
    });

  type validators =
    Validators.t(Formality.validator(field, value, state, message));

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
               | String("") => Invalid("Email is required")
               | String(value) when !(emailRegex |> Js.Re.test(value)) =>
                 Invalid("Email is invalid")
               | String(_) => Valid
               | _ => failwith("Email validator received bad input") /* sadly, as well */
               };
             },
           },
         )
      |> Validators.add(
           Password,
           {
             strategy: Strategy.OnFirstBlur,
             dependents: None,
             validate: (value, _) =>
               switch (value) {
               | String("") => Invalid("Password is required")
               | String(_) => Valid
               | _ => failwith("Password validator received bad input") /* sadly, as well */
               },
           },
         )
    );
};

module LoginFormContainer = Formality.Make(LoginForm);

let component = ReasonReact.statelessComponent(__MODULE__);

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
              Js.Global.setTimeout(form.reset, 3000) |> ignore;
            },
            500,
          )
          |> ignore;
        }
      }>
      ...{
           form =>
             <form
               className="form"
               onSubmit={form.submit |> Formality.Dom.preventDefault}>
               <div className="form-messages-area form-messages-area-lg" />
               <div className="form-content">
                 <h2 className="push-lg"> {"Login" |> ReasonReact.string} </h2>
                 <div className="form-row">
                   <label htmlFor="login--email" className="label-lg">
                     {"Email" |> ReasonReact.string}
                   </label>
                   <input
                     id="login--email"
                     type_="text"
                     value={form.state.email}
                     disabled={form.submitting}
                     onChange={
                       event =>
                         event->Formality.Dom.toValueOnChange->LoginForm.String
                         |> form.change(LoginForm.Email)
                     }
                     onBlur={
                       event =>
                         event->Formality.Dom.toValueOnBlur->LoginForm.String
                         |> form.blur(LoginForm.Email)
                     }
                   />
                   {
                     switch (LoginForm.Email |> form.results) {
                     | Some(Invalid(message)) =>
                       <div className={Cn.make(["form-message", "failure"])}>
                         {message |> ReasonReact.string}
                       </div>
                     | Some(Valid) =>
                       <div className={Cn.make(["form-message", "success"])}>
                         {{j|✓|j} |> ReasonReact.string}
                       </div>
                     | None => ReasonReact.null
                     }
                   }
                 </div>
                 <div className="form-row">
                   <label htmlFor="login--password" className="label-lg">
                     {"Password" |> ReasonReact.string}
                   </label>
                   <input
                     id="login--password"
                     type_="text"
                     value={form.state.password}
                     disabled={form.submitting}
                     onChange={
                       event =>
                         event->Formality.Dom.toValueOnChange->LoginForm.String
                         |> form.change(LoginForm.Password)
                     }
                     onBlur={
                       event =>
                         event->Formality.Dom.toValueOnBlur->LoginForm.String
                         |> form.blur(LoginForm.Password)
                     }
                   />
                   {
                     switch (LoginForm.Password |> form.results) {
                     | Some(Invalid(message)) =>
                       <div className={Cn.make(["form-message", "failure"])}>
                         {message |> ReasonReact.string}
                       </div>
                     | Some(Valid) =>
                       <div className={Cn.make(["form-message", "success"])}>
                         {{j|✓|j} |> ReasonReact.string}
                       </div>
                     | None => ReasonReact.null
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
                         event->Formality.Dom.toCheckedOnChange->LoginForm.Bool
                         |> form.change(LoginForm.Remember)
                     }
                     onBlur={
                       event =>
                         event->Formality.Dom.toCheckedOnBlur->LoginForm.Bool
                         |> form.blur(LoginForm.Remember)
                     }
                   />
                   <label htmlFor="login--remember">
                     {"Remember me" |> ReasonReact.string}
                   </label>
                 </div>
                 <div className="form-row">
                   <button className="push-lg" disabled={form.submitting}>
                     {
                       (form.submitting ? "Submitting..." : "Submit")
                       |> ReasonReact.string
                     }
                   </button>
                   {
                     switch (form.status) {
                     | Formality.FormStatus.Submitted =>
                       <div className={Cn.make(["form-status", "success"])}>
                         {{j|✓ Logged In|j} |> ReasonReact.string}
                       </div>
                     | _ => ReasonReact.null
                     }
                   }
                 </div>
               </div>
             </form>
         }
    </LoginFormContainer>,
};
