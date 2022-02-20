module Form = %form(
  type input = {name: string}
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) =>
        switch name {
        | "" => Error("Name is required")
        | _ => Ok(name)
        },
    },
  }
)

let initialInput: Form.input = {name: ""}

@react.component
let make = () => {
  let form = Form.useForm(~initialInput, ~onSubmit=(_, _) => ())

  <form
    onSubmit={event => {
      event->ReactEvent.Form.preventDefault
      form.submit()
    }}>
    <input
      id="field--name--input"
      value=form.input.name
      disabled=form.submitting
      onBlur={_ => form.blurName()}
      onChange={event =>
        form.updateName((_input, value) => {name: value}, (event->ReactEvent.Form.target)["value"])}
    />
    {switch form.nameResult {
    | Some(Error(message)) =>
      <div id="field--name--error" className="error"> {message->React.string} </div>
    | Some(Ok(_)) => <div id="field--name--ok" className="ok"> {"OK"->React.string} </div>
    | None => React.null
    }}
    <button type_="submit" id="button--submit" disabled=form.submitting>
      {(form.submitting ? "Submitting..." : "Submit")->React.string}
    </button>
  </form>
}
