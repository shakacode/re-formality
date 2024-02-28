module Dom = FormalityCompat__PublicHelpers.Dom
module Strategy = FormalityCompat__Strategy
module FormStatus = FormalityCompat__FormStatus

module Make = FormalityCompat__Form.Make
module MakeWithId = FormalityCompat__FormWithId.Make

type ok = FormalityCompat__Validation.Result.ok = Valid | NoValue

type result<'message> = Belt.Result.t<ok, 'message>

type status<'message> = FormalityCompat__Validation.Sync.status<'message> =
  | Pristine
  | Dirty(
      FormalityCompat__Validation.Result.result<'message>,
      FormalityCompat__Validation.Visibility.t,
    )

type validate<'state, 'message> = 'state => FormalityCompat__Validation.Result.result<'message>

type validator<'field, 'state, 'message> = FormalityCompat__Validation.Sync.validator<
  'field,
  'state,
  'message,
> = {
  field: 'field,
  strategy: FormalityCompat__Strategy.t,
  dependents: option<list<'field>>,
  validate: validate<'state, 'message>,
}

module Async: {
  module Make = FormalityCompat__FormAsyncOnChange.Make
  module MakeWithId = FormalityCompat__FormAsyncOnChangeWithId.Make

  module MakeOnBlur = FormalityCompat__FormAsyncOnBlur.Make
  module MakeOnBlurWithId = FormalityCompat__FormAsyncOnBlurWithId.Make

  let debounceInterval: int

  type status<'message> = FormalityCompat__Validation.Async.status<'message> =
    | Pristine
    | Dirty(
        FormalityCompat__Validation.Result.result<'message>,
        FormalityCompat__Validation.Visibility.t,
      )
    | Validating

  type validate<'state, 'message> = 'state => Js.Promise.t<
    FormalityCompat__Validation.Result.result<'message>,
  >

  type equalityChecker<'state> = ('state, 'state) => bool

  type validator<'field, 'state, 'message> = FormalityCompat__Validation.Async.validator<
    'field,
    'state,
    'message,
  > = {
    field: 'field,
    strategy: FormalityCompat__Strategy.t,
    dependents: option<list<'field>>,
    validate: FormalityCompat__Validation.Sync.validate<'state, 'message>,
    validateAsync: option<(validate<'state, 'message>, equalityChecker<'state>)>,
  }
}
