module Form = %form(
  type submissionError = CouldNotSubmit
  type input = {
    booleanField1: bool,
    booleanField2: bool,
    booleanField3: bool,
    dateField1: Js.Date.t,
    dateField2: Js.Date.t,
    intField1: string,
    intField2: string,
    intField3: string,
    intField4: string,
    intField5: string,
    optionalStringField1: string,
    optionalStringField2: string,
    readOnlyField1: string,
    readOnlyField2: string,
    stringField1: string,
    stringField2: string,
    stringField3: string,
    stringField4: string,
  }
  type output = {
    booleanField1: bool,
    booleanField2: bool,
    booleanField3: bool,
    dateField1: Js.Date.t,
    dateField2: Js.Date.t,
    intField1: int,
    intField2: int,
    intField3: int,
    intField4: int,
    intField5: int,
    optionalStringField1: option<string>,
    optionalStringField2: option<string>,
    readOnlyField1: unit,
    readOnlyField2: unit,
    stringField1: string,
    stringField2: string,
    stringField3: string,
    stringField4: string,
  }
  let validators = {
    booleanField1: None,
    booleanField2: None,
    booleanField3: {
      strategy: OnFirstBlur,
      validate: form => Ok(form.booleanField3),
    },
    dateField1: {
      strategy: OnFirstChange,
      validate: form => Ok(form.dateField1),
    },
    dateField2: {
      strategy: OnFirstChange,
      validate: form => Ok(form.dateField2),
    },
    intField1: {
      strategy: OnFirstBlur,
      validate: form =>
        switch form.intField1 |> int_of_string_opt {
        | Some(x) => Ok(x)
        | None => Error("Invalid number")
        },
    },
    intField2: {
      strategy: OnFirstBlur,
      validate: form =>
        switch form.intField2 |> float_of_string_opt {
        | Some(x) => Ok(x |> int_of_float)
        | None => Error("Invalid number")
        },
    },
    intField3: {
      strategy: OnFirstBlur,
      validate: form =>
        switch form.intField3 |> int_of_string_opt {
        | Some(x) => Ok(x)
        | None => Error("Invalid number")
        },
    },
    intField4: {
      strategy: OnFirstBlur,
      validate: form =>
        switch form.intField4 |> float_of_string_opt {
        | Some(x) => Ok(x |> int_of_float)
        | None => Error("Invalid number")
        },
    },
    intField5: {
      strategy: OnFirstBlur,
      validate: foo =>
        switch foo.intField5 |> float_of_string_opt {
        | Some(x) => Ok(x |> int_of_float)
        | None => Error("Invalid number")
        },
    },
    optionalStringField1: {
      strategy: OnFirstBlur,
      validate: form => Ok(Some(form.optionalStringField1)),
    },
    optionalStringField2: {
      strategy: OnFirstBlur,
      validate: form => Ok(Some(form.optionalStringField2)),
    },
    readOnlyField1: {
      strategy: OnFirstBlur,
      validate: _ => Ok(),
    },
    readOnlyField2: {
      strategy: OnFirstBlur,
      validate: _ => Ok(),
    },
    stringField1: None,
    stringField2: {
      strategy: OnFirstBlur,
      validate: form => Ok(form.stringField2),
    },
    stringField3: {
      strategy: OnFirstBlur,
      validate: form => Ok(form.stringField3),
    },
    stringField4: {
      strategy: OnFirstBlur,
      validate: form => Ok(form.stringField4),
    },
  }
)
