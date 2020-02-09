// TODO: [Public Api] Change `updateField` siganture so it accepts `input => input` instead of `input`
// TODO: [Meta] In some cases (records?) order of items is reversed.
// TODO: [Collections] Collections: `items: [@field.collection] array(item)`
// TODO: [Collections] Whole collection validation
// TODO: [Collections] Add/remove items from collections
// TODO: [Collections] Reorder items in collections

"formality" |> Ppxlib.Driver.register_transformation(~extensions=[Form.ext]);
