type validationResult('message) =
  | Valid
  | Invalid('message);

type validate('value, 'state, 'message) =
  ('value, 'state) => validationResult('message);

type validateAsync('value, 'message) =
  'value => Js.Promise.t(validationResult('message));

type validator('field, 'value, 'state, 'message) = {
  field: 'field,
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('value, 'state, 'message),
};

type asyncValidator('field, 'value, 'state, 'message) = {
  field: 'field,
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('value, 'state, 'message),
  validateAsync: option(validateAsync('value, 'message)),
};

type submissionCallbacks('field, 'state, 'message) = {
  notifyOnSuccess: option('state) => unit,
  notifyOnFailure: (list(('field, 'message)), option('message)) => unit,
  reset: unit => unit,
};
