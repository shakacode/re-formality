// TODO: [Public Api] Change `updateField` siganture so it accepts `input => input` instead of `input`
// TODO: [Public Api] Change `form.fieldResult()` from function to just value, since all these functions are called anyway
// TODO: [Meta] In some cases (records?) order of items is reversed.
// TODO: [Collections] Reorder items in collections
// TODO: [General] Prolly makes sense to move functions from Formality module to own modules to reduce bundle size
//                 I.e. if user doesn't use async stuff or collections, those wouldn't get into the final bundle (unless tree shaking works these days)

"formality" |> Ppxlib.Driver.register_transformation(~extensions=[Form.ext]);
