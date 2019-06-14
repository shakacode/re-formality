include Formality__Validation.Result;
include Formality__Validation.Sync;
include Formality__PublicHelpers;

module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module Make = FormalityCompat__Form.Make;

module Async = {
  include Formality__Validation.Async;
  module Make = FormalityCompat__FormAsyncOnChange.Make;
  module MakeOnBlur = FormalityCompat__FormAsyncOnBlur.Make;
  let debounceInterval = FormalityCompat__FormAsyncOnChange.defaultDebounceInterval;
};
