module Validation = FormalityCompat__FormWithId.Validation

module type Form = {
  type field
  type state
  type message
  type submissionError
  let validators: list<Validation.validator<field, state, message>>
}

module Make = (Form: Form) => FormalityCompat__FormWithId.Make({
  include Form
  module FieldId = Id.MakeComparable({
    type t = Form.field
    let cmp = Pervasives.compare
  })
})
