module Form = %form(
  type input = {name: string}
  type submissionError =
    | A
    | B
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
    },
  }
)
