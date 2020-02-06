module ReactUpdate = Formality__ReactUpdate;

type strategy =
  | OnFirstBlur
  | OnFirstChange
  | OnFirstSuccess
  | OnFirstSuccessOrFirstBlur
  | OnSubmit;

type visibility =
  | Shown
  | Hidden;

type fieldStatus('outputValue, 'message) =
  | Pristine
  | Dirty(result('outputValue, 'message), visibility);

type asyncFieldStatus('outputValue, 'message) =
  | Pristine
  | Dirty(result('outputValue, 'message), visibility)
  | Validating;

type formStatus('submissionError) =
  | Editing
  | Submitting(option('submissionError))
  | Submitted
  | SubmissionFailed('submissionError);

type submissionStatus =
  | NeverSubmitted
  | AttemptedToSubmit;

type index = int;

type singleValueValidator('input, 'outputValue, 'message) = {
  strategy,
  validate: 'input => result('outputValue, 'message),
};

type singleValueAsyncValidator('input, 'outputValue, 'message) = {
  strategy,
  validate: 'input => result('outputValue, 'message),
  validateAsync: 'input => Js.Promise.t(result('outputValue, 'message)),
};

type collectionValidator('input, 'message, 'fieldsValidators) = {
  collection: option('input => result(unit, 'message)),
  fields: 'fieldsValidators,
};

type valueOfCollectionValidator('input, 'outputValue, 'message) = {
  strategy,
  validate: ('input, ~at: index) => result('outputValue, 'message),
};

type valueOfCollectionAsyncValidator('input, 'outputValue, 'message) = {
  strategy,
  validate: ('input, ~at: index) => result('outputValue, 'message),
  validateAsync:
    ('input, ~at: index) => Js.Promise.t(result('outputValue, 'message)),
};

type formValidationResult('output, 'fieldsStatuses) =
  | Ok({
      output: 'output,
      fieldsStatuses: 'fieldsStatuses,
    })
  | Error({fieldsStatuses: 'fieldsStatuses});

type submissionCallbacks('input, 'submissionError) = {
  notifyOnSuccess: option('input) => unit,
  notifyOnFailure: 'submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
};

let validateFieldOnChangeWithValidator =
    (
      ~input: 'input,
      ~fieldStatus: fieldStatus('outputValue, 'message),
      ~submissionStatus: submissionStatus,
      ~validator: singleValueValidator('input, 'outputValue, 'message),
      ~setStatus: fieldStatus('outputValue, 'message) => 'statuses,
    )
    : 'statuses => {
  switch (validator.strategy, fieldStatus, submissionStatus) {
  | (_, Dirty(_, Shown), _)
  | (_, _, AttemptedToSubmit)
  | (OnFirstChange, _, NeverSubmitted) =>
    Dirty(validator.validate(input), Shown)->setStatus
  | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, NeverSubmitted) =>
    switch (validator.validate(input)) {
    | Ok(_) as result => Dirty(result, Shown)->setStatus
    | Error(_) as result => Dirty(result, Hidden)->setStatus
    }
  | (OnFirstBlur | OnSubmit, _, NeverSubmitted) =>
    Dirty(validator.validate(input), Hidden)->setStatus
  };
};

let validateFieldOnChangeWithoutValidator =
    (
      ~fieldInput: 'outputValue,
      ~setStatus: fieldStatus('outputValue, 'message) => 'statuses,
    )
    : 'statuses => {
  Dirty(Ok(fieldInput), Hidden)->setStatus;
};

let validateFieldDependencyOnChange =
    (
      ~input: 'input,
      ~fieldStatus: fieldStatus('outputValue, 'message),
      ~validator: singleValueValidator('input, 'outputValue, 'message),
      ~setStatus: fieldStatus('outputValue, 'message) => 'statuses,
    )
    : option('statuses) => {
  switch (fieldStatus) {
  | Pristine
  | Dirty(_, Hidden) => None
  | Dirty(_, Shown) =>
    Some(Dirty(validator.validate(input), Shown)->setStatus)
  };
};

let validateFieldOnBlurWithValidator =
    (
      ~input: 'input,
      ~fieldStatus: fieldStatus('outputValue, 'message),
      ~validator: singleValueValidator('input, 'outputValue, 'message),
      ~setStatus: fieldStatus('outputValue, 'message) => 'statuses,
    )
    : option('statuses) => {
  switch (fieldStatus) {
  | Dirty(_, Shown) => None
  | Pristine
  | Dirty(_, Hidden) =>
    switch (validator.strategy) {
    | OnFirstChange
    | OnFirstSuccess
    | OnSubmit => Some(Dirty(validator.validate(input), Hidden)->setStatus)
    | OnFirstBlur
    | OnFirstSuccessOrFirstBlur =>
      Some(Dirty(validator.validate(input), Shown)->setStatus)
    }
  };
};

let validateFieldOnBlurWithoutValidator =
    (
      ~fieldInput: 'outputValue,
      ~fieldStatus: fieldStatus('outputValue, 'message),
      ~setStatus: fieldStatus('outputValue, 'message) => 'statuses,
    )
    : option('statuses) =>
  switch (fieldStatus) {
  | Dirty(_, Shown | Hidden) => None
  | Pristine => Some(Dirty(Ok(fieldInput), Hidden)->setStatus)
  };

let exposeFieldResult =
    (fieldStatus: fieldStatus('outputValue, 'message))
    : option(result('outputValue, 'message)) =>
  switch (fieldStatus) {
  | Pristine
  | Dirty(_, Hidden) => None
  | Dirty(result, Shown) => Some(result)
  };
