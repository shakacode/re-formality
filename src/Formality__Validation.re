/* TODO: Make variant? */
type value = string;

type result('message) = {
  valid: bool,
  message: option('message),
  meta: option(string)
};

type validate('state, 'message) = (value, 'state) => result('message);

type validateAsync('message) = value => Js.Promise.t(result('message));

type validator('field, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message)
};

type asyncValidator('field, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message),
  validateAsync: option(validateAsync('message))
};

module type ValidatorsConfig = {type t;};

module MakeValidators = (Config: ValidatorsConfig) =>
  Map.Make(
    {
      type t = Config.t;
      let compare = Formality__Utils.comparator;
    }
  );

type notifiers = {
  onSuccess: unit => unit,
  onFailure: unit => unit /* TODO: notifiers.onFailure should accept errors */
};

let ifResult = (~valid, ~invalid, result) =>
  switch result {
  | {valid: true} => result |> valid
  | {valid: false} => result |> invalid
  };
