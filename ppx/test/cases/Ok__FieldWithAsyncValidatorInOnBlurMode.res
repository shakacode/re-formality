module Form = %form(
  type input = {name: @field.async({mode: OnBlur}) string}
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
      validateAsync: name => Js.Promise.resolve(Ok(name)),
    },
  }
)
