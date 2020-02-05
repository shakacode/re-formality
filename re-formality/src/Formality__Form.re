module FormStatus = Formality__FormStatus;
module Validation = Formality__Validation;

let validateFieldOnChangeWithValidator:
  type o.
    (
      ~input: 'input,
      ~status: Validation.status(o, 'message),
      ~submission: FormStatus.submission,
      ~validator: Validation.SingleValue.validator('input, o, 'message),
      ~setStatus: Validation.status(o, 'message) => 'statuses
    ) =>
    'statuses =
  (~input, ~status, ~submission, ~validator, ~setStatus) => {
    let Validation.SingleValue.{strategy, validate} = validator;
    switch (strategy, status, submission) {
    | (_, Dirty(_, Shown), _)
    | (_, _, AttemptedToSubmit)
    | (OnFirstChange, _, NeverSubmitted) =>
      Dirty(input->validate, Shown)->setStatus
    | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, NeverSubmitted) =>
      switch (input->validate) {
      | Ok(_) as result => Dirty(result, Shown)->setStatus
      | Error(_) as result => Dirty(result, Hidden)->setStatus
      }
    | (OnFirstBlur | OnSubmit, _, NeverSubmitted) =>
      Dirty(input->validate, Hidden)->setStatus
    };
  };

let validateFieldOnChangeWithoutValidator:
  type o.
    (
      ~fieldInput: o,
      ~setStatus: Validation.status(o, 'message) => 'statuses
    ) =>
    'statuses =
  (~fieldInput, ~setStatus) => {
    Dirty(Ok(fieldInput), Hidden)->setStatus;
  };

let validateFieldDependencyOnChange:
  type o.
    (
      ~input: 'input,
      ~status: Validation.status(o, 'message),
      ~validator: Validation.SingleValue.validator('input, o, 'message),
      ~setStatus: Validation.status(o, 'message) => 'statuses
    ) =>
    option('statuses) =
  (~input, ~status, ~validator, ~setStatus) => {
    let Validation.SingleValue.{validate} = validator;
    switch (status) {
    | Pristine
    | Dirty(_, Hidden) => None
    | Dirty(_, Shown) => Some(Dirty(input->validate, Shown)->setStatus)
    };
  };

let validateFieldOnBlurWithValidator:
  type o.
    (
      ~input: 'input,
      ~status: Validation.status(o, 'message),
      ~validator: Validation.SingleValue.validator('input, o, 'message),
      ~setStatus: Validation.status(o, 'message) => 'statuses
    ) =>
    option('statuses) =
  (~input, ~status, ~validator, ~setStatus) => {
    switch (status) {
    | Dirty(_, Shown) => None
    | Pristine
    | Dirty(_, Hidden) =>
      let Validation.SingleValue.{strategy, validate} = validator;
      switch (strategy) {
      | OnFirstChange
      | OnFirstSuccess
      | OnSubmit => Some(Dirty(input->validate, Hidden)->setStatus)
      | OnFirstBlur
      | OnFirstSuccessOrFirstBlur =>
        Some(Dirty(input->validate, Shown)->setStatus)
      };
    };
  };

let validateFieldOnBlurWithoutValidator:
  type o.
    (
      ~fieldInput: o,
      ~status: Validation.status(o, 'message),
      ~setStatus: Validation.status(o, 'message) => 'statuses
    ) =>
    option('statuses) =
  (~fieldInput, ~status, ~setStatus) =>
    switch (status) {
    | Dirty(_, Shown | Hidden) => None
    | Pristine => Some(Dirty(Ok(fieldInput), Hidden)->setStatus)
    };

let exposeFieldResult:
  type o. Validation.status(o, 'message) => option(result(o, 'message)) =
  status =>
    switch (status) {
    | Pristine
    | Dirty(_, Hidden) => None
    | Dirty(result, Shown) => Some(result)
    };
