module MakeValidators = Formality__Validation.MakeValidators;
module Dom = Formality__PublicHelpers.Dom;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;
module Make = Formality__Form.Make;
module MakeWithAsyncValidationsOnChange = Formality__FormAsyncOnChange.Make;
module MakeWithAsyncValidationsOnBlur = Formality__FormAsyncOnBlur.Make;

type validationResult('message) =
  Formality__Validation.validationResult('message) =
    | Valid | Invalid('message);

type validate('value, 'state, 'message) =
  ('value, 'state) => validationResult('message);

type validateAsync('value, 'message) =
  'value => Js.Promise.t(validationResult('message));

type validator('field, 'value, 'state, 'message) =
  Formality__Validation.validator('field, 'value, 'state, 'message) = {
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: validate('value, 'state, 'message),
  };

type asyncValidator('field, 'value, 'state, 'message) =
  Formality__Validation.asyncValidator('field, 'value, 'state, 'message) = {
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: validate('value, 'state, 'message),
    validateAsync: option(validateAsync('value, 'message)),
  };

let debounceInterval: int;

let emptyString: string => bool;
