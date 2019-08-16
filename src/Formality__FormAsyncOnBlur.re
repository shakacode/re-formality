module Validation = Formality__FormAsyncOnBlurWithId.Validation;

module type Form = {
  type field;
  type state;
  type message;
  type submissionError;
  let validators: list(Validation.Async.validator(field, state, message));
};

module Make = (Form: Form) =>
  Formality__FormAsyncOnBlurWithId.Make({
    include Form;
    module FieldId =
      Id.MakeComparable({
        type t = Form.field;
        let cmp = Pervasives.compare;
      });
  });
