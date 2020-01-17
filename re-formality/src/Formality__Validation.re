module Strategy = Formality__Strategy;

module Index = {
  type t = int;
};

module Visibility = {
  type t =
    | Shown
    | Hidden;
};

module Sync = {
  type status('output, 'message) =
    | Pristine
    | Dirty(Result.t('output, 'message), Visibility.t);

  type result('output, 'fieldsStatuses) =
    | Ok({
        output: 'output,
        fieldsStatuses: 'fieldsStatuses,
      })
    | Error({fieldsStatuses: 'fieldsStatuses});

  module SingleValue = {
    type validator('input, 'output, 'message) = {
      strategy: Strategy.t,
      validate: 'input => Result.t('output, 'message),
    };
  };

  module Collection = {
    type collectionValidator('input, 'message, 'fieldsValidators) = {
      collection: option('input => Result.t(unit, 'message)),
      fields: 'fieldsValidators,
    };

    type valueValidator('input, 'output, 'message) = {
      strategy: Strategy.t,
      validate: ('input, ~at: Index.t) => Result.t('output, 'message),
    };
  };
};

module Async = {
  module Result = {
    type t('message) = Belt.Result.t(ok, 'message)
    and ok =
      | Valid
      | NoValue;
  };

  type status('message) =
    | Pristine
    | Dirty(Result.t('message), Visibility.t)
    | Validating;

  type singleValueValidatorFn('input, 'message) =
    'input => Result.t('message);
  type valueOfCollectionValidatorFn('input, 'message) =
    ('input, ~at: Index.t) => Result.t('message);

  type validate('state, 'message) =
    'state => Js.Promise.t(Result.t('message));

  type equalityChecker('state) = ('state, 'state) => bool;

  type validator('field, 'state, 'message) = {
    field: 'field,
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: singleValueValidatorFn('state, 'message),
    validateAsync:
      option((validate('state, 'message), equalityChecker('state))),
  };
};

include Sync;

type visibility = Visibility.t;

type submissionCallbacks('state, 'submissionError) = {
  notifyOnSuccess: option('state) => unit,
  notifyOnFailure: 'submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
};
