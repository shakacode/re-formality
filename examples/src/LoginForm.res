module LoginForm = %form(
  type input = {
    email: string,
    password: string,
    rememberMe: bool,
  }
  let validators = {
    email: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({email}) => {
        let emailRegex = %re(`/.*@.*\..+/`)
        switch email {
        | "" => Error("Email is required")
        | _ as value if !(emailRegex->Js.Re.test_(value)) => Error("Email is invalid")
        | _ => Ok(email)
        }
      },
    },
    password: {
      strategy: OnFirstBlur,
      validate: ({password}) =>
        switch password {
        | "" => Error("Password is required")
        | _ => Ok(password)
        },
    },
    rememberMe: None,
  }
)

let initialInput: LoginForm.input = {
  email: "",
  password: "",
  rememberMe: false,
}

@react.component
let make = () => {
  let form = LoginForm.useForm(~initialInput, ~onSubmit=(output, form) => {
    Js.log2("Submitted with:", output)
    Js.Global.setTimeout(() => {
      form.notifyOnSuccess(None)
      form.reset->Js.Global.setTimeout(3000)->ignore
    }, 500)->ignore
  })

  <Form className="form" onSubmit=form.submit>
    <div className="form-messages-area form-messages-area-lg" />
    <div className="form-content">
      <h2 className="push-lg"> {"Login"->React.string} </h2>
      <div className="form-row">
        <label htmlFor="login--email" className="label-lg"> {"Email"->React.string} </label>
        <input
          id="login--email"
          type_="text"
          value=form.input.email
          disabled=form.submitting
          onBlur={_ => form.blurEmail()}
          onChange={event =>
            form.updateEmail(
              (input, value) => {...input, email: value},
              (event->ReactEvent.Form.target)["value"],
            )}
        />
        {switch form.emailResult {
        | Some(Error(message)) =>
          <div className={cx(["form-message", "form-message-for-field", "failure"])}>
            {message->React.string}
          </div>
        | Some(Ok(_)) =>
          <div className={cx(["form-message", "form-message-for-field", "success"])}>
            {"✓"->React.string}
          </div>
        | None => React.null
        }}
      </div>
      <div className="form-row">
        <label htmlFor="login--password" className="label-lg"> {"Password"->React.string} </label>
        <input
          id="login--password"
          type_="text"
          value=form.input.password
          disabled=form.submitting
          onBlur={_ => form.blurPassword()}
          onChange={event =>
            form.updatePassword(
              (input, value) => {...input, password: value},
              (event->ReactEvent.Form.target)["value"],
            )}
        />
        {switch form.passwordResult {
        | Some(Error(message)) =>
          <div className={cx(["form-message", "form-message-for-field", "failure"])}>
            {message->React.string}
          </div>
        | Some(Ok(_)) =>
          <div className={cx(["form-message", "form-message-for-field", "success"])}>
            {"✓"->React.string}
          </div>
        | None => React.null
        }}
      </div>
      <div className="form-row">
        <input
          id="login--remember"
          type_="checkbox"
          checked=form.input.rememberMe
          disabled=form.submitting
          className="push-lg"
          onBlur={_ => form.blurRememberMe()}
          onChange={event =>
            form.updateRememberMe(
              (input, value) => {...input, rememberMe: value},
              (event->ReactEvent.Form.target)["checked"],
            )}
        />
        <label htmlFor="login--remember"> {"Remember me"->React.string} </label>
      </div>
      <div className="form-row">
        <button className={cx(["primary", "push-lg"])} disabled=form.submitting>
          {(form.submitting ? "Submitting..." : "Submit")->React.string}
        </button>
        {switch form.status {
        | Submitted =>
          <div className={cx(["form-status", "success"])}> {"✓ Logged In"->React.string} </div>
        | _ => React.null
        }}
      </div>
    </div>
  </Form>
}
