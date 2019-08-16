module Validation = Formality__FormAsyncOnChangeWithId.Validation;

let defaultDebounceInterval = Formality__FormAsyncOnChangeWithId.defaultDebounceInterval;

module type Form = {
  type field;
  type state;
  type message;
  type submissionError;
  let debounceInterval: int;
  let validators: list(Validation.Async.validator(field, state, message));
};

module Make = (Form: Form) =>
  Formality__FormAsyncOnChangeWithId.Make({
    include Form;
    module FieldId =
      Id.MakeComparable({
        type t = Form.field;
        let cmp = Pervasives.compare;
      });
  });
