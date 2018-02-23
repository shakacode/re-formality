module Validation = Formality__Validation;

let ifResult =
    (~valid, ~validWithBag, ~invalid, ~invalidWithBag, ~none, result) =>
  switch result {
  | Some(result) =>
    switch result {
    | Validation.Valid(true) => valid()
    | Validation.ValidityBag(bag) when bag.valid => bag |> validWithBag
    | Validation.Valid(false) => invalid()
    | Validation.ValidityBag(bag) => bag |> invalidWithBag
    }
  | None => none()
  };

let ifResultJust = (~valid, ~invalid, ~none, result) =>
  switch result {
  | Some(result) =>
    switch result {
    | Validation.Valid(true) => None |> valid
    | Validation.ValidityBag(bag) when bag.valid => Some(bag) |> valid
    | Validation.Valid(false) => None |> invalid
    | Validation.ValidityBag(bag) => Some(bag) |> invalid
    }
  | None => none()
  };
