module Validation = FormalityCompat__FormAsyncOnChangeWithId.Validation

let defaultDebounceInterval = FormalityCompat__FormAsyncOnChangeWithId.defaultDebounceInterval

module type Form = {
  type field
  type state
  type message
  type submissionError
  let debounceInterval: int
  let validators: list<Validation.Async.validator<field, state, message>>
}

module Make = (Form: Form) => FormalityCompat__FormAsyncOnChangeWithId.Make({
  include Form
  module FieldId = Id.MakeComparable({
    type t = Form.field
    let cmp = Pervasives.compare
  })
})
