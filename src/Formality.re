include Formality__Validation;

include Formality__PublicHelpers;

module Strategy = Formality__Strategy;

module Make = Formality__Form.Make;

module MakeWithAsyncValidationsOnChange = Formality__FormAsyncOnChange.Make;

module MakeWithAsyncValidationsOnBlur = Formality__FormAsyncOnBlur.Make;

let debounceInterval = Formality__FormAsyncOnChange.defaultDebounceInterval;

exception ImpossibleResult;
