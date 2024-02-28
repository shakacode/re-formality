module Form = %form(
  type input = {
    name: @field.async({mode: OnBlur}) string,
    email: string,
    age: int,
  }
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name, _}) => Ok(name),
      validateAsync: name => Js.Promise.resolve(Ok(name)),
    },
    email: {
      strategy: OnSubmit,
      validate: ({email, _}) => Ok(email),
    },
    age: {
      strategy: OnSubmit,
      validate: ({age, _}) => Ok(age),
    },
  }
)
