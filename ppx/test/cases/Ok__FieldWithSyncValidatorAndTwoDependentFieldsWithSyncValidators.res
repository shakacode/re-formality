module Form = %form(
  type input = {
    a: @field.deps((b, c)) string,
    b: string,
    c: string,
  }
  let validators = {
    a: {
      strategy: OnSubmit,
      validate: ({a, _}) => Ok(a),
    },
    b: {
      strategy: OnSubmit,
      validate: ({b, _}) => Ok(b),
    },
    c: {
      strategy: OnSubmit,
      validate: ({c, _}) => Ok(c),
    },
  }
)
