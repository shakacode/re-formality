module SignupForm = [%form
  type input = {
    email: [@field.async] string,
    password: [@field.deps passwordConfirmation] string,
    passwordConfirmation: string,
  };
  let validators = {
    email: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({email}) => {
        let emailRegex = [%bs.re {|/.*@.*\..+/|}];
        switch (email) {
        | "" => Error("Email is required")
        | _ as value when !emailRegex->Js.Re.test_(value) =>
          Error("Email is invalid")
        | _ => Ok(email)
        };
      },
      validateAsync: email =>
        Js.Promise.(
          email
          ->Api.validateEmail
          ->then_(
              valid =>
                valid
                  ? Ok(email)->resolve
                  : Error("Email is already taken")->resolve,
              _,
            )
        ),
    },
    password: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({password}) => {
        let minLength = 4;
        switch (password) {
        | "" => Error("Password is required")
        | _ when password->Js.String.length < minLength =>
          Error({j| $(minLength)+ characters, please|j})
        | _ => Ok(password)
        };
      },
    },
    passwordConfirmation: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({password, passwordConfirmation}) =>
        switch (passwordConfirmation) {
        | "" => Error("Confirmation is required")
        | _ when passwordConfirmation !== password =>
          Error("Password doesn't match")
        | _ => Ok(passwordConfirmation)
        },
    },
  }
];

let initialInput: SignupForm.input = {
  email: "",
  password: "",
  passwordConfirmation: "",
};

[@react.component]
let make = () => {
  let form =
    SignupForm.useForm(
      ~initialInput,
      ~onSubmit=(output, form) => {
        Js.log2("Submitted with:", output);
        Js.Global.setTimeout(
          () => {
            form.notifyOnSuccess(None);
            form.reset->Js.Global.setTimeout(3000)->ignore;
          },
          500,
        )
        ->ignore;
      },
    );

  <Form className="form" onSubmit={form.submit}>
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
          value={form.input.email}
          disabled={form.submitting}
          onBlur={_ => form.blurEmail()}
          onChange={event =>
            form.updateEmail({
              ...form.input,
              email: event->ReactEvent.Form.target##value,
            })
          }
        />
        {switch (form.emailResult()) {
         | Some(Validating(_)) =>
           <div className="form-message"> "Checking..."->React.string </div>
         | Some(Result(Error(message))) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Result(Ok(_))) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | None => React.null
         }}
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
          value={form.input.password}
          disabled={form.submitting}
          onBlur={_ => form.blurPassword()}
          onChange={event =>
            form.updatePassword({
              ...form.input,
              password: event->ReactEvent.Form.target##value,
            })
          }
        />
        {switch (form.passwordResult()) {
         | Some(Error(message)) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Ok(_)) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | None => React.null
         }}
      </div>
      <div className="form-row">
        <label htmlFor="signup--passwordConfirmation" className="label-lg">
          "Confirmation"->React.string
        </label>
        <input
          id="signup--passwordConfirmation"
          type_="text"
          value={form.input.passwordConfirmation}
          disabled={form.submitting}
          onBlur={_ => form.blurPasswordConfirmation()}
          onChange={event =>
            form.updatePasswordConfirmation({
              ...form.input,
              passwordConfirmation: event->ReactEvent.Form.target##value,
            })
          }
        />
        {switch (form.passwordConfirmationResult()) {
         | Some(Error(message)) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Ok(_)) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | None => React.null
         }}
      </div>
      <div className="form-row">
        <button className="push-lg" disabled={form.submitting}>
          (form.submitting ? "Submitting..." : "Submit")->React.string
        </button>
        {switch (form.status) {
         | Submitted =>
           <div className={Cn.make(["form-status", "success"])}>
             {j|✓ Signed Up|j}->React.string
           </div>
         | _ => React.null
         }}
      </div>
    </div>
  </Form>;
};
