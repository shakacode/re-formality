module Strategy = {
  type t =
    | OnFirstBlur
    | OnFirstChange
    | OnFirstSuccess
    | OnFirstSuccessOrFirstBlur
    | OnSubmit;
};

/* TODO: Make variant? */
type value = string;

/* TODO: Define type in config as `message` might be anything, e.g. i18n object */
type validityBag = {
  valid: bool,
  tag: option(string),
  message: option(string)
};

type result =
  | Valid(bool)
  | ValidityBag(validityBag);

type validate('state) = (value, 'state) => result;

type validateAsync = value => Js.Promise.t(result);

type validator('field, 'state) = {
  strategy: option(Strategy.t),
  dependents: option(list('field)),
  validate: validate('state)
};

type asyncValidator('field, 'state) = {
  strategy: option(Strategy.t),
  dependents: option(list('field)),
  validate: validate('state),
  validateAsync: option(validateAsync)
};
