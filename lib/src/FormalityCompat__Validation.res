module Startegy = FormalityCompat__Strategy

module Result = {
  type ok =
    | Valid
    | NoValue

  type result<'message> = Belt.Result.t<ok, 'message>
}

module Visibility = {
  type t =
    | Shown
    | Hidden
}

module Sync = {
  type status<'message> =
    | Pristine
    | Dirty(Result.result<'message>, Visibility.t)

  type validate<'state, 'message> = 'state => Result.result<'message>

  type validator<'field, 'state, 'message> = {
    field: 'field,
    strategy: Startegy.t,
    dependents: option<list<'field>>,
    validate: validate<'state, 'message>,
  }
}

include Sync

module Async = {
  type status<'message> =
    | Pristine
    | Dirty(Result.result<'message>, Visibility.t)
    | Validating

  type validate<'state, 'message> = 'state => Js.Promise.t<Result.result<'message>>

  type equalityChecker<'state> = ('state, 'state) => bool

  type validator<'field, 'state, 'message> = {
    field: 'field,
    strategy: Startegy.t,
    dependents: option<list<'field>>,
    validate: Sync.validate<'state, 'message>,
    validateAsync: option<(validate<'state, 'message>, equalityChecker<'state>)>,
  }
}

type submissionCallbacks<'state, 'submissionError> = {
  notifyOnSuccess: option<'state> => unit,
  notifyOnFailure: 'submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
}
