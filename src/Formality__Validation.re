/* TODO: Make variant? */
type value = string;

type validityBag('message) = {
  valid: bool,
  tag: option(string),
  message: 'message
};

type result('message) =
  | Valid(bool)
  | ValidityBag(validityBag('message));

type validate('state, 'message) = (value, 'state) => result('message);

type validateAsync('message) = value => Js.Promise.t(result('message));

type validator('field, 'state, 'message) = {
  strategy: option(Formality__Strategy.t),
  dependents: option(list('field)),
  validate: validate('state, 'message)
};

type asyncValidator('field, 'state, 'message) = {
  strategy: option(Formality__Strategy.t),
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
