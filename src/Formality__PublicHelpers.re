let ifResult =
    (~valid, ~validWithBag, ~invalid, ~invalidWithBag, ~none, result) =>
  switch result {
  | Some(result) =>
    switch result {
    | Formality__Validation.Valid(true) => valid()
    | Formality__Validation.ValidityBag(bag) when bag.valid =>
      bag |> validWithBag
    | Formality__Validation.Valid(false) => invalid()
    | Formality__Validation.ValidityBag(bag) => bag |> invalidWithBag
    }
  | None => none()
  };

let ifResultJust = (~valid, ~invalid, ~none, result) =>
  switch result {
  | Some(result) =>
    switch result {
    | Formality__Validation.Valid(true) => None |> valid
    | Formality__Validation.ValidityBag(bag) when bag.valid =>
      Some(bag) |> valid
    | Formality__Validation.Valid(false) => None |> invalid
    | Formality__Validation.ValidityBag(bag) => Some(bag) |> invalid
    }
  | None => none()
  };

module Dom = {
  let valueOnChange = (handle, event) =>
    event |> ReactEventRe.Form.target |> Formality__Utils.targetValue |> handle;
  let valueOnBlur = (handle, event) =>
    event
    |> ReactEventRe.Focus.target
    |> Formality__Utils.targetValue
    |> handle;
  let preventDefault = (submit, event) => {
    if (! (event |> ReactEventRe.Form.defaultPrevented)) {
      event |> ReactEventRe.Form.preventDefault;
    };
    submit();
  };
};
