// TODO: Async validation: `item: [@field.async] string`
// TODO: Collections: `items: [@field.collection] array(item)`
// TODO: Whole collection validation
// TODO: Field with deps validation
// TODO: Add/remove items from collections
// TODO: Reorder items in collections
// TODO: Strip attributes from input types

"formality" |> Ppxlib.Driver.register_transformation(~extensions=[Form.ext]);
