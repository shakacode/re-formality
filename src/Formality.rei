module Dom = Formality__PublicHelpers.Dom;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module Make = Formality__Form.Make;

module Async: {
  module Make = Formality__FormAsyncOnChange.Make;
  module MakeOnBlur = Formality__FormAsyncOnBlur.Make;
  let debounceInterval: int;
};

type result('message) =
  Formality__Validation.result('message) =
    | Valid | Invalid('message) | Optional;

type validate('state, 'message) = 'state => result('message);

type validateAsync('state, 'message) =
  'state => Js.Promise.t(result('message));

type checkEquality('state) = ('state, 'state) => bool;

type validator('field, 'state, 'message) =
  Formality__Validation.validator('field, 'state, 'message) = {
    field: 'field,
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: validate('state, 'message),
  };

type asyncValidator('field, 'state, 'message) =
  Formality__Validation.asyncValidator('field, 'state, 'message) = {
    field: 'field,
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: validate('state, 'message),
    validateAsync:
      option((validateAsync('state, 'message), checkEquality('state))),
  };
