type ok =
  | Valid
  | NoValue;

type result('message) = Result.t(ok, 'message);

type status('message) =
  | Pristine
  | Dirty(result('message));

type validate('state, 'message) = 'state => result('message);

type validateAsync('state, 'message) =
  'state => Js.Promise.t(result('message));

type checkEquality('state) = ('state, 'state) => bool;

type validator('field, 'state, 'message) = {
  field: 'field,
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message),
};

type asyncValidator('field, 'state, 'message) = {
  field: 'field,
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message),
  validateAsync:
    option((validateAsync('state, 'message), checkEquality('state))),
};

type submissionCallbacks('field, 'state, 'message) = {
  notifyOnSuccess: option('state) => unit,
  notifyOnFailure: (list(('field, 'message)), option('message)) => unit,
  reset: unit => unit,
};
