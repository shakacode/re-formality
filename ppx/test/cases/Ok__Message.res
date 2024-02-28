module I18n = {
  type t
}

module Form = %form(
  type input = {name: string}
  type message = I18n.t
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
    },
  }
)
