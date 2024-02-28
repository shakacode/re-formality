include FormalityCompat__Validation.Result
include FormalityCompat__Validation.Sync
include FormalityCompat__PublicHelpers

module Strategy = FormalityCompat__Strategy
module FormStatus = FormalityCompat__FormStatus

module Make = FormalityCompat__Form.Make
module MakeWithId = FormalityCompat__FormWithId.Make

module Async = {
  include FormalityCompat__Validation.Async

  module Make = FormalityCompat__FormAsyncOnChange.Make
  module MakeWithId = FormalityCompat__FormAsyncOnChangeWithId.Make

  module MakeOnBlur = FormalityCompat__FormAsyncOnBlur.Make
  module MakeOnBlurWithId = FormalityCompat__FormAsyncOnBlurWithId.Make

  let debounceInterval = FormalityCompat__FormAsyncOnChangeWithId.defaultDebounceInterval
}
