module Validation = Formality__FormAsyncOnChangeId.Validation;

let defaultDebounceInterval = Formality__FormAsyncOnChangeId.defaultDebounceInterval;

module type Form = {
  type field;
  type state;
  type message;
  type submissionError;
  let debounceInterval: int;
  let validators: list(Validation.Async.validator(field, state, message));
};

module Make = (Form: Form) =>
  Formality__FormAsyncOnChangeId.Make({
    include Form;
    module FieldId =
      Id.MakeComparable({
        type t = Form.field;
        let cmp = Pervasives.compare;
      });
  });
