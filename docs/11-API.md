# API

- [Configuration](#configuration)
  - [`type input`](#type-input)
    - [`[@field.async]`](#fieldasync)
    - [`[@field.collection]`](#fieldcollection)
    - [`[@field.deps]`](#fielddeps)
  - [`type output`](#type-output)
  - [`type message`](#type-message)
  - [`type submissionError`](#type-submissionerror)
  - [`let debounceInterval`](#let-debounceinterval)
  - [`let validators`](#let-validators)
- [Rendering](#rendering)
  - [`useForm`](#useform)
    - [`submissionCallbacks`](#submissioncallbacks)
  - [`interface`](#interface)
    - general field handlers
    - async field handlers
    - field of collection handlers
    - collection handlers
    - `input`
    - `status`
    - `submitting`
    - `dirty`
    - `submit`
    - `dismissSubmissionError`
    - `dismissSubmissionResult`
    - `mapSubmissionError`
    - `reset`
    - `valid`

## Configuration
Form configuration module can be created using `[%form]` extension:

```reason
module MyForm = [%form
  type input;
  type output;
  ...
];
```

### `type input`
**Requirements:** Required. Must be a record.

Form input data.

```reason
type input = {email: string};
```

See **[IO](./03-IO.md)** & **[Basic Usage](./04-BasicUsage.md)**.
<br>
<br>

#### `input` field attributes
##### `[@field.async]`
**Requirements:** Optional.

Attribute for a field with async validation.

```reason
type input = {email: [@field.async] string};

// OnChange mode
// Default, use it if you want to be explicit.
type input = {email: [@field.async {mode: OnChange}] string};

// OnBlur mode
type input = {email: [@field.async {mode: OnBlur}] string};
```

See **[Async Validation](./05-AsyncValidation.md)**.
<br>
<br>

##### `[@field.collection]`
**Requirements:** Optional. If provided, must be an array.

Attribute for a collection field.

```reason
type input = {
  authors: [@field.collection] array(author),
}
and author = {name: string};
```

See **[Collections](./06-Collections.md)**.
<br>
<br>

##### `[@field.deps]`
**Requirements:** Optional.

Dependent fields attribute.

```reason
type input = {
  a: [@field.deps b] string,
  b: string,
};
```

See **[Dependent Fields](./07-DependentFields.md)**.
<br>
<br>

### `type output`
**Requirements:** Optional. If provided, must be a record with the same set of fields as in `input`.<br>
**Default:** `input`

Form output data.

```reason
type output = {email: Email.t};
```

See **[IO](./03-IO.md)** & **[Basic Usage](./04-BasicUsage.md)**.
<br>
<br>

### `type message`
**Requirements:** Optional.<br>
**Default:** `string`

Type of error messages.

```reason
type message = I18n.t;
```

See **[Basic Usage](./04-BasicUsage.md)** & **[I18n](./09-I18n.md)**.
<br>
<br>

### `type submissionError`
**Requirements:** Optional.<br>
**Default:** `unit`

Type of submission error.

```reason
type submissionError =
  | UserNotFound
  | UnknownError;
```

See **[Form Submission](./08-FormSubmission.md)**.
<br>
<br>

### `let debounceInterval`
**Requirements:** Optional `int`.<br>
**Default:** `700`

Debounce interval in ms for async validators in `OnChange` mode.

```reason
let debounceInterval = 1000;
```

See **[Async Validation](./05-AsyncValidation.md)**.
<br>
<br>

### `let validators`
**Requirements:** Required. Must be a record with the same set of fields as in `input`/`output`.<br>

Validators record.

```reason
// Pseudo code
let validators = {
  // General field
  field: {
    strategy: Strategy.t,
    validate: input => result('outputValue, message),
  },

  // Field of collection
  fieldOfCollection: {
    collection: input => result(unit, message), // or None
    fields: {
      strategy: Strategy.t,
      validate: (input, ~at: int) => result('outputValue, message),
    },
  },

  // Async field
  asyncField: {
    strategy: Strategy.t,
    validate: input => result('outputValue, message),
    validateAsync: 'outputValue => Js.Promise.t(result('outputValue, message)),
  },

  // Async field of collection
  asyncFieldOfCollection: {
    strategy: Strategy.t,
    validate: (input, ~at: int) => result('outputValue, message),
    validateAsync: 'outputValue => Js.Promise.t(result('outputValue, message)),
  },

  // Async field with eq function
  asyncFieldWithEq: {
    strategy: Strategy.t,
    validate: input => result('outputValue, message),
    validateAsync: 'outputValue => Js.Promise.t(result('outputValue, message)),
    eq: ('outputValue, 'outputValue) => bool,
  },

  // Field without validator
  fieldWithoutValidator: None,
};
```

See **[Validation Strategies](./02-ValidationStrategies.md)**, **[Basic Usage](./04-BasicUsage.md)**, **[Async Validation](./05-AsyncValidation.md)** & **[Collections](./06-Collections.md)**.
<br>
<br>

## Rendering
Module created via `[%form]` extension exposes `useForm` React hook.

### `useForm`
React hook.

```reason
MyForm.useForm(
  ~initialInput: MyForm.input,
  ~onSubmit: (output: MyForm.output, cb: Formality.submissionCallbacks) => unit,
) => MyForm.interface;
```

#### `submissionCallbacks`
Callbacks passed to `onSubmit` handler.

```reason
type submissionCallbacks = {
  notifyOnSuccess: option(input) => unit,
  notifyOnFailure: submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
};
```

See **[Form Submission](./08-FormSubmission.md)**.
<br>
<br>

### `interface`
Interface to the hook.

```reason
type interface = {
  input: input,
  status: Formality.formStatus(submissionError),
  submitting: bool,
  dirty: unit => bool,
  submit: unit => unit,
  dismissSubmissionError: unit => unit,
  dismissSubmissionResult: unit => unit,
  mapSubmissionError: (submissionError => submissionError) => unit,
  reset: unit => unit,

  // General form
  valid: unit => bool,
  // Form with async fields
  valid: unit => option(bool),

  // General field
  update[Field]: (input => input) => unit,
  blur[Field]: unit => unit,
  [field]Result: option(result('outputValue, message)),

  // Async field
  update[Field]: (input => input) => unit,
  blur[Field]: unit => unit,
  [field]Result: option(Formality.Async.exposedFieldStatus('outputValue, message)),

  // Field of collection
  update[CollectionEntry][Field]: (input => input, ~at: index) => unit,
  blur[CollectionEntry][Field]: (~at: index) => unit,
  [collectionEntry][Field]Result: (~at: index) => option(result(string, message)),

  // Collection
  add[CollectionEntry]: author => unit,
  remove[CollectionEntry]: (~at: index) => unit,
};
```
