module Form = %form(
  type input = {
    name: @field.async({mode: OnBlur}) string,
    age: @field.async({mode: OnBlur}) int,
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
      validateAsync: age => Js.Promise.resolve(Ok(age)),
    },
  }
)
