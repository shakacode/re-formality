module Form = %form(
  type input = {
    name: @field.async({mode: OnBlur}) string,
    age: int,
  }
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name, _}) => Ok(name),
      validateAsync: name => Js.Promise.resolve(Ok(name)),
    },
    age: {
      strategy: OnSubmit,
      validate: ({age, _}) => Ok(age),
    },
  }
)
