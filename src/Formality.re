include Formality__Validation.Result;
include Formality__Validation.Sync;
include Formality__PublicHelpers;

module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module Make = Formality__Form.Make;
module MakeWithId = Formality__FormWithId.Make;

module Async = {
  include Formality__Validation.Async;

  module Make = Formality__FormAsyncOnChange.Make;
  module MakeWithId = Formality__FormAsyncOnChangeWithId.Make;

  module MakeOnBlur = Formality__FormAsyncOnBlur.Make;
  module MakeOnBlurWithId = Formality__FormAsyncOnBlurWithId.Make;

  let debounceInterval = Formality__FormAsyncOnChangeWithId.defaultDebounceInterval;
};
