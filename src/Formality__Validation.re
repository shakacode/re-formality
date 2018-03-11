/* TODO: Make variant? */
type value = string;

type validationResult('message) =
  | Valid
  | Invalid('message);

type validate('state, 'message) =
  (value, 'state) => validationResult('message);

type validateAsync('message) =
  value => Js.Promise.t(validationResult('message));

type validator('field, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message),
};

type asyncValidator('field, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('state, 'message),
  validateAsync: option(validateAsync('message)),
};

module type ValidatorsConfig = {type t;};

module MakeValidators = (Config: ValidatorsConfig) =>
  Map.Make(
    {
      type t = Config.t;
      let compare = Formality__Utils.comparator;
    },
  );

type notifiers = {
  onSuccess: unit => unit,
  onFailure: unit => unit /* TODO: notifiers.onFailure should accept errors */
};

let ifResult = (~valid, ~invalid, result) =>
  switch (result) {
  | Valid => result |> valid
  | Invalid(_) => result |> invalid
  };
