module Form = %form(
  type input = {name: @field.async({mode: OnBlur}) string}
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
      validateAsync: name => Promise.resolve(Ok(name)),
    },
  }
)
