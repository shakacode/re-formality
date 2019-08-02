include Formality__Validation.Result;
include Formality__Validation.Sync;
include Formality__PublicHelpers;

module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module Make = Formality__Form.Make;
module MakeId = Formality__FormId.Make;

module Async = {
  include Formality__Validation.Async;

  module Make = Formality__FormAsyncOnChange.Make;
  module MakeId = Formality__FormAsyncOnChangeId.Make;

  module MakeOnBlur = Formality__FormAsyncOnBlur.Make;
  module MakeOnBlurId = Formality__FormAsyncOnBlurId.Make;

  let debounceInterval = Formality__FormAsyncOnChangeId.defaultDebounceInterval;
};
