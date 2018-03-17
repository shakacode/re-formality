type validationResult('message) =
  | Valid
  | Invalid('message);

type validate('value, 'state, 'message) =
  ('value, 'state) => validationResult('message);

type validateAsync('value, 'message) =
  'value => Js.Promise.t(validationResult('message));

type validator('field, 'value, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('value, 'state, 'message),
};

type asyncValidator('field, 'value, 'state, 'message) = {
  strategy: Formality__Strategy.t,
  dependents: option(list('field)),
  validate: validate('value, 'state, 'message),
  validateAsync: option(validateAsync('value, 'message)),
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
