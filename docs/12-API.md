# API

- [Configuration](#configuration)
  - [`type input`](#type-input)
    - [`[@field.async]`](#fieldasync)
    - [`[@field.collection]`](#fieldcollection)
    - [`[@field.deps]`](#fielddeps)
  - [`type output`](#type-output)
  - [`type message`](#type-message)
  - [`type metadata`](#metadata)
  - [`type submissionError`](#type-submissionerror)
  - [`let debounceInterval`](#let-debounceinterval)
  - [`let validators`](#let-validators)
- [Rendering](#rendering)
  - [`useForm`](#useform)
    - [`submissionCallbacks`](#submissioncallbacks)
  - [`interface`](#interface)
    - [Update handlers](#update-handlers)
    - [Blur handlers](#blur-handlers)
    - [Field result](#field-result)
    - [Collection handlers](#collection-handlers)
    - [`input`](#input)
    - [`status`](#status)
    - [`submitting`](#submitting)
    - [`dirty`](#dirty)
    - [`submit`](#submit)
    - [`dismissSubmissionResult`](#dismisssubmissionresult)
    - [`dismissSubmissionError`](#dismisssubmissionerror)
    - [`mapSubmissionError`](#mapsubmissionerror)
    - [`reset`](#reset)
    - [`valid`](#valid)

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

See **[Basic Usage](./04-BasicUsage.md)** & **[I18n](./10-I18n.md)**.
<br>
<br>

### `type metadata`
**Requirements:** Optional.<br>

Additional metadata.

```reason
type metadata = { items: array(Item.t) };
```

See **[Metadata](./08-Metadata.md)**.
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

See **[Form Submission](./09-FormSubmission.md)**.
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

  // Validator with `metadata`
  field: {
    strategy: Strategy.t,
    validate: (input, metadata) => result('outputValue, message),
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

  // Async field with metadata
  asyncFieldWithEq: {
    strategy: Strategy.t,
    validate: (input, metadata) => result('outputValue, message),
    validateAsync: ('outputValue, metadata) => Js.Promise.t(result('outputValue, message)),
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
// General
MyForm.useForm(
  ~initialInput: MyForm.input,
  ~onSubmit: (output: MyForm.output, cb: Formality.submissionCallbacks) => unit,
) => MyForm.interface;

// With `metadata`
MyForm.useForm(
  ~initialInput: MyForm.input,
  ~metadata: MyForm.metadata,
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

See **[Form Submission](./09-FormSubmission.md)**.
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
  dismissSubmissionResult: unit => unit,
  dismissSubmissionError: unit => unit,
  mapSubmissionError: (submissionError => submissionError) => unit,
  reset: unit => unit,

  // General form
  valid: unit => bool,
  // Form with async fields
  valid: unit => option(bool),

  // General field
  update[Field]: ((input, 'inputValue) => input, 'inputValue) => unit,
  blur[Field]: unit => unit,
  [field]Result: option(result('outputValue, message)),

  // Async field
  update[Field]: ((input, 'inputValue) => input, 'inputValue) => unit,
  blur[Field]: unit => unit,
  [field]Result: option(Formality.Async.exposedFieldStatus('outputValue, message)),

  // Field of collection
  update[CollectionEntry][Field]: (~at: index, (input, 'inputValue) => input, 'inputValue) => unit,
  blur[CollectionEntry][Field]: (~at: index) => unit,
  [collectionEntry][Field]Result: (~at: index) => option(result('outputValue, message)),

  // Collection
  add[CollectionEntry]: 'collectionEntryInput => unit,
  remove[CollectionEntry]: (~at: index) => unit,
};
```
<br>

#### Update handlers
Used to update form input for a specific field.

```reason
// Field
update[Field]: ((input, 'inputValue) => input, 'inputValue) => unit

// Field of collection
update[CollectionEntry][Field]: (~at: index, (input, 'inputValue) => input, 'inputValue) => unit,
```
<br>

#### Blur handlers
Used to notify hook on blur event for a specific field.

```reason
// Field
blur[Field]: unit => unit

// Field of collection
blur[CollectionEntry][Field]: (~at: index) => unit,
```
<br>

#### Field result
Used to display validation result for a specific field.

```reason
// Field
[field]Result: option(result('outputValue, message)),

// Async field
type asyncResult =
  | Validating('outputValue)
  | Result(result('outputValue, message));

[field]Result: option(asyncResult),

// Field of collection
[collectionEntry][Field]Result: (~at: index) => option(result('outputValue, message)),
```
<br>

#### Collection handlers
Used to update collection contents.

```reason
// Add entry
add[CollectionEntry]: 'collectionEntryInput => unit,

// Remove entry
remove[CollectionEntry]: (~at: index) => unit,
```
<br>

#### `input`
Current form input. Use it to set values of form fields.

```reason
input: MyForm.input
```
<br>

#### `status`
Current form status.

```reason
type formStatus =
  | Editing
  | Submitting(option(MyForm.submissionError))
  | Submitted
  | SubmissionFailed(MyForm.submissionError);

status: formStatus
```
<br>

#### `submitting`
`bool` value, passed for convenience as it gets derived from the `form.status`. Set to `true` when `form.status` is `Submitting`, `false` otherwise.

```reason
submitting: bool
```
<br>

#### `dirty`
The function would return `true` if any form field was touched, `false` otherwise.

```reason
dirty: unit => bool
```
<br>

#### `submit`
Triggers form submission.

```reason
submit: unit => unit
```
<br>

#### `dismissSubmissionResult`
Use it when you want to let a user dismiss alerts with errors from a server or success message without resetting a form. Under the hood, it changes `Submitted` or `SubmissionFailed` form statuses back to `Editing`.

```reason
dismissSubmissionResult: unit => unit
```
<br>

#### `dismissSubmissionError`
Dismisses submission error only.

```reason
dismissSubmissionError: unit => unit
```
<br>

#### `mapSubmissionError`
Maps over submission error. Useful for animating errors.

```reason
mapSubmissionError: (submissionError => submissionError) => unit
```
<br>

#### `reset`
Resets the whole form to its initial state.

```reason
reset: unit => unit
```
<br>

#### `valid`
The function that would report the overall validity of the form.

In forms with async fields, it would return `option(bool)`. `None` is possible when one of the async fields in `Validating` state, i.e. we don't know yet if it's valid or not. Also, keep in mind, if one of the async fields wasn't touched yet, it would perform only local validation.

Use this function with caution since it might introduce noticeable overhead on large forms.

```reason
// General form
valid: unit => bool

// Form with async fields
valid: unit => option(bool)
```
