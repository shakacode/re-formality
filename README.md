# Formality

[![npm version](https://img.shields.io/npm/v/re-formality.svg?style=flat-square)](https://www.npmjs.com/package/re-formality)
[![build status](https://img.shields.io/travis/alexfedoseev/re-formality/master.svg?style=flat-square)](https://travis-ci.org/alexfedoseev/re-formality)
[![license](https://img.shields.io/npm/l/re-formality.svg?style=flat-square)](https://www.npmjs.com/package/re-formality)

Reasonable form validation tool for [`reason-react`](https://reasonml.github.io/reason-react/).

## Features
* Validation strategies
* Async validations (debounced on change / on blur)
* I18n compatible

## TOC
- [Installation](#installation)
- [Examples](#examples)
- [Concepts](#concepts)
  - [Strategies](#strategies)
- [Usage](#usage)
  - [Form config](#form-config)
  - [Form hook](#form-hook)
  - [Rendering](#rendering)
  - [Async validations](#async-validations)
  - [Custom field comparator](#custom-field-comparator)
  - [I18n](#i18n)
- [Alternatives](#alternatives)
- [License](#license)

## Installation

```shell
# yarn
yarn add re-formality
# or npm
npm install --save re-formality
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "re-formality"
]
```

## Examples
* [Live demo](https://re-formality.now.sh)
* [Live @ minima.app](https://minima.app)
* [Sources](./examples)

## Concepts
The main purpose of this library is to provide great form validation UX. To achieve this, `Formality` follows the following principle:

<p align="center">
<strong>Validation feedback should be provided as soon as possible but not too soon.</strong>
</p>

The hardest part is to figure out the right moment when first validation results should be emitted in UI.

Let's break down a case with credit card field. A user opens a form and focuses on a field. When the first character is typed, the field is in an invalid state but it's not polite to show an error immediately: we should let user a chance to finish what he's doing. While the user is typing and field is in invalid state, we don't provide any feedback in UI. If after some character validator reported valid result, it's a proper moment to indicate a success (e.g. show credit card type). But if the user left the field in an invalid state (e.g. moved to another field) we have all rights to emit an error. After the first result is emitted, we update validation state in UI on every change.

Sadly, form fields are different and credit card scenario is not universal. This is where strategies kick in.

### Strategies
We can't have a single scenario for all the cases but we can spot the most common ones, describe a logic of each and apply proper scenarios to specific form fields. To understand a behavior of each strategy, add the following prefix to its name: _"Start providing feedback in UI on..."_

```reason
module Strategy = {
  type t =
    | OnFirstBlur
    | OnFirstChange
    | OnFirstSuccess
    | OnFirstSuccessOrFirstBlur
    | OnSubmit;
};
```

#### `OnFirstBlur`
Results are emitted on the first blur. After first results are emitted, a user receives feedback on every change in this field.

#### `OnFirstChange`
Results are emitted on the first change in a field (basically, as a user types).

#### `OnFirstSuccess`
Results are emitted on first successful validation. After first results are emitted, a user receives feedback on every change in this field.

#### `OnFirstSuccessOrFirstBlur` ✨
Results are emitted on first successful validation or on the first blur. After first results are emitted, a user receives feedback on every change in this field.

#### `OnSubmit`
Results are emitted only after the first submission attempt. After this, results for each field are emitted on every change until the form is reset.


## Usage
It takes 3 steps to implement a form:

1. Define form config.
2. Create form hook.
3. Render form hook and form UI.

> Code > 1000 words. Quick example for you:

<details>
<summary>Spoiler</summary>

```reason
module LoginForm = {
  open Formality;

  type field =
    | Email
    | Password;

  type state = {
    email: string,
    password: string,
  };

  type message = string;
  type submissionError = unit;

  module EmailField = {
    let update = (state, value) => {...state, email: value};

    let validator = {
      field: Email,
      strategy: Strategy.OnFirstSuccessOrFirstBlur,
      dependents: None,
      validate: state =>
        switch (state.email) {
        | "" => Error("Uh oh error")
        | _ => Ok(Valid)
        },
    };
  };

  module PasswordField = {
    let update = (state, value) => {...state, password: value};

    let validator = {
      field: Password,
      strategy: Strategy.OnFirstBlur,
      dependents: None,
      validate: state =>
        switch (state.password) {
        | "" => Error("Uh oh error")
        | _ => Ok(Valid)
        },
    };
  };

  let validators = [
    EmailField.validator,
    PasswordField.validator,
  ];
};

module LoginFormHook = Formality.Make(LoginForm);

[@react.component]
let make = () => {
  let form =
    LoginFormHook.useForm(
      ~initialState=LoginForm.{email: "", password: ""},
      ~onSubmit=(state, form) => {
        // Submit form and use callbacks to update form container
      },
    );

  <form onSubmit={form.submit->Formality.Dom.preventDefault}>
    <input
      value={form.state.email}
      disabled={form.submitting}
      onBlur={_ => form.blur(Email)}
      onChange={event =>
        form.change(
          Email,
          LoginForm.EmailField.update(
            form.state,
            event->ReactEvent.Form.target##value,
          ),
        )
      }
    />
    {switch (Email->form.result) {
     | Some(Error(message)) =>
       <div className={Cn.make(["form-message", "failure"])}>
         message->React.string
       </div>
     | Some(Ok(Valid | NoValue))
     | None => React.null
     }}
    <input
      value={form.state.password}
      disabled={form.submitting}
      onBlur={_ => form.blur(Password)}
      onChange={event =>
        form.change(
          Password,
          LoginForm.PasswordField.update(
            form.state,
            event->ReactEvent.Form.target##value,
          ),
        )
      }
    />
    {switch (Password->form.result) {
     | Some(Error(message)) =>
       <div className={Cn.make(["form-message", "failure"])}>
         message->React.string
       </div>
     | Some(Ok(Valid | NoValue))
     | None => React.null
     }}
    <button disabled={form.submitting}>
      (form.submitting ? "Submitting..." : "Submit")->React.string
    </button>
  </form>;
};
```
</details>

### Form config
Form config is a module:

```reason
module MyForm = {
  type field;
  type state;
  type message;
  type submissionError;
  let validators: list(validator);
};
```

To make things happen, you must provide few types and list of validators. Depending on whether you need async validations or not, your config will require (or not) additional data. But most of the things are common for all types of forms. Let's start with the simplest case without async validations—this is what's required for all types of forms—and then async differences will be outlined in [Async validations](#async-validations) section.

#### `type field`
A variant where tags are form fields.

```reason
type field =
  | Email
  | Password;
```

#### `type state`
`state` is a record that defines a shape of a form state.

```reason
type state = {
  email: string,
  password: string,
};
```

#### `type message`
The type of error messages that will be rendered in UI. Feel free to set it to whatever you need.

The most common scenario is:

```reason
type message = string;
```

If you build i18n'ized app then it's going to be something like this:

```reason
type message = I18n.t;
```

#### `type submissionError`
When you submit a form submission might fail, for various reasons. It might be a bad password on login attempt (expected error) or server crash (unexpected error), anything. This kind of error is specific to a form and its type describes what might go wrong on form submission.

```reason
type submissionError =
  | UserNotFound
  | BadPassword
  | UnexpectedServerError;
```

Later on, on failed form submission, you will be able to pass this error to form container and provide appropriate feedback to users in UI.

#### `let validators: list(validator)`
Field validators.

```reason
module EmailField = {
  let validator = {
    field: Email,
    strategy: Strategy.OnFirstSuccessOrFirstBlur,
    dependents: None,
    validate: state =>
      switch (state.email) {
      | "" => Error("Uh oh error")
      | _ => Ok(Valid)
    },
  };
};

let validators = [
  EmailField.validator,
  ...
];
```

##### `validator`
It's a record of 4 items:

```reason
type validator('field, 'state, 'message) = {
  field: 'field,
  strategy: Formality.Strategy.t,
  dependents: option(list('field)),
  validate: 'state => Result.t(ok, 'message),
};
```

###### `strategy`
See [Strategies](#strategies).

###### `dependents`
Optional list of fields that must be revalidated on a change in the current field. E.g. `PasswordConfirmation` must be revalidated on a change in `Password` field:

```reason
field: Password,
dependents: [PasswordConfirmation]->Some
```

###### `validate`
A function that takes `state` and returns Belt's `Result.t`:

```reason
type ok =
  | Valid
  | NoValue;

type validate('state, 'message) = 'state => Result.t(ok, 'message);
```

Most of the time you need `Ok(Valid)` or `Error('message)`. You want to return `Ok(NoValue)` when optional field receives no value (e.g. `value == ""`). `Valid` and `NoValue` are explicitly differentiated since there's no reason to show success message/icon in UI when no value is provided.

### Form hook
To create form hook simply do the following:

```reason
module MyFormHook = Formality.Make(MyForm);
```

It creates React `useForm` hook for general form.

If you render forms with async validations, use:

```reason
/* Async validations on change (debounced) */
module MyAsyncFormHook = Formality.Async.Make(MyForm);

/* Async validations on blur */
module MyAsyncFormHook = Formality.Async.MakeOnBlur(MyForm);
```

### Rendering
Form hook accepts 2 arguments and returns record with everything you need to render your UI:

```reason
let form =
  MyFormHook.useForm(
    ~initialState={email: "", password: ""},
    ~onSubmit=(state, form) => {
      // Submit form and use callbacks to update form container
    },
  );

// Use `form` to render your UI...
```

#### `initialState`
It's `state` record with initial values for each form field.

#### `onSubmit`
This handler will be triggered on form submission (only when all validators returned `Ok(_)`).

It accepts two arguments:
1. `state`: current state of a form
2. `submissionCallbacks`: record with 4 callbacks

```reason
type submissionCallbacks('state, 'submissionError) = {
  notifyOnSuccess: option('state) => unit,
  notifyOnFailure: 'submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
};
```

##### `notifyOnSuccess`
Trigger this callback when server responded with success. It accepts optional state argument: if it's provided, this state will be set as a next form state.

##### `notifyOnFailure`
Trigger this callback when server responded with an error. It accepts 1 argument of type `MyForm.submissionError` (defined in form config).

You can access this data in render via `form.status` (see [`form.status`](#form-status)).

##### `reset`
Simply, resets a form container state.

##### `form.dismissSubmissionResult`
Use it when you want to dismiss alerts with errors from server or success message without resetting a form. See [`form.status`](#form-status) for more details.

#### `form` record
`form` record, returned from the hook, contains everything you need to render UI:

```reason
type form = {
  state: Form.state,
  status: FormStatus.t,
  result: Form.field => option(result),
  dirty: unit => bool,
  valid: unit => bool, /* not available in async forms */
  submitting: bool,
  change: (Form.field, Form.state) => unit,
  blur: Form.field => unit,
  submit: unit => unit,
  reset: unit => unit,
  mapSubmissionError: ('submissionError => 'submissionError) => unit,
  dismissSubmissionError: unit => unit,
  dismissSubmissionResult: unit => unit,
};
```

##### `form.state`
Form state, obviously. Use it to set `values` of the form fields.

##### `form.status`
Form status is a variant:

```reason
module FormStatus = {
  type t('error) =
    | Editing
    | Submitting
    | Submitted
    | SubmissionFailed('error);
};
```

You can use it to show a spinner while a form is `Submitting`, or success message on `Submitted`, or display server errors using data that you passed to `notifyOnFailure` callback (it's available in `SubmissionFailed` tag payload).

##### `form.submitting`
This prop is passed for convenience (as you will need it to disable form inputs and button while a form is `Submitting`). Basically, this is `true` when `form.status` is `Submitting`, `false` otherwise.

##### `form.result`
Use this function to get validation result for a field.

```reason
switch (Email->form.result) {
| Some(Error(message)) =>
  <div className="failure">
    message->React.string
  </div>
| Some(Ok(Valid | NoValue))
| None => React.null
}
```

##### `form.dirty`
This function will return `true` if any form field was touched, `false` otherwise.

##### `form.valid`
This function will return `true` if all form field are valid, `false` otherwise. Not available in forms with async validations.

##### `form.change`

```reason
type change = (Form.field, Form.state) => unit;
```

This handler must be triggered `onChange`. It accepts `field` and the next form `state`.

```reason
module MyForm = {
  module EmailField = {
    let update = (state, value) => {...state, email: value};
  };
};

<input
  value={form.state.email}
  disabled={form.submitting}
  onBlur={_ => form.blur(Email)}
  onChange={
    event =>
      form.change(
        Email,
        MyForm.EmailField.update(
          form.state,
          event->ReactEvent.Form.target##value,
        ),
      )
  }
/>
```

##### `form.blur`
This handler must be triggered `onBlur`. It accepts only one `field` argument.

```reason
<input
  value={form.state.email}
  disabled={form.submitting}
  onBlur={_ => form.blur(Email)}
/>
```

##### `form.submit`
Use it as `onSubmit` handler of a `<form />` element:

```reason
<form onSubmit={form.submit->Formality.Dom.preventDefault} />
```

##### `form.reset`
Resets form state.

##### `form.dismissSubmissionResult`
Use it when you want to let user dismissing alerts with errors from server or success message without resetting a form. Under the hood, it changes `FormStatus.Submitted` & `FormStatus.SubmissionFailed` statuses back to `FormStatus.Editing`.

##### `form.mapSubmissionError`
Maps over submission error. Useful for animating errors.

##### `form.dismissSubmissionError`
Dismissing submission error only.

### Async validations
Some validations can't be performed locally, e.g. on signup, you want to validate if user's email is available or it's already taken.

There are 2 common ways to provide async feedback: send a request to a server on every change or only on blur event. The first way is better in terms of UX but creates a significant load, so your client might become slow or a server might feel bad. The blur way doesn't have this problem (at least not that much) but UX is definitely not the best b/c user have to blur away from a field to receive a feedback.

What can we do about it to have the best of both worlds? An answer is to debounce async validations on change. What does it mean and how does it work: when a user types something in in a form field, no external requests are triggered. Instead, it's put on hold. While the user is typing, we're waiting. Once he stopped and there was no activity in the certain period of time—async request is triggered.

#### Debounced async validations on change
To implement debounced async validations you need to make some additions to common form config.

##### `let debounceInterval: int`
Configure amount of time (in ms) that `Formality` should wait since last user activity before invoking debounced async validation.

```reason
/* Default interval: 700 */
let debounceInterval = Formality.debounceInterval;

/* Or any other custom int */
let debounceInterval = 1000;
```

##### `validator`
In addition to `field`, `strategy`, `dependents` & `validate`, provide optional async validator.

```reason
type asyncValidator('field, 'state, 'message) = {
  field: 'field,
  strategy: Formality.Strategy.t,
  dependents: option(list('field)),
  validate: 'state => Result.t(ok, 'message),
  validateAsync: option(
    (
      'state => Js.Promise.t(Result.t(ok, 'message)),
      ('state, 'state) => bool,
    ),
  ),
};
```

`validateAsync` is a tuple that consists of 2 functions:

```reason
(
  /* 1. async validator */
  'state => Js.Promise.t(Result.t(ok, 'message)),

  /* 2. value equality checker */
  ('state, 'state) => bool,
)
```

1. Validator itself takes `state` and returns `Js.Promise.t(Result.t)`.
2. Value equality checker receives 2 arguments: form state when validation was invoked and form state when response was resolved. Why it's required: by the time when server responded with some result, local value might be already changed so before setting received result `Formality` checks if value of the field is the same that was validated. And if it's not it simply ignores this result.

**Please, make sure you check equality of field values, not state instances!**

Example:

```reason
validateAsync: Some(
  (
    state =>
      Js.Promise.(
        state.email
        ->Api.validateEmail
        ->then_(
            valid =>
              valid ?
                Ok(Valid)->resolve :
                Error("Email is already taken")->resolve,
            _,
          )
      ),
    (prevState, nextState) => prevState.email == nextState.email,
  )
),
```

To create form hook pass config to `Formality.Async.Make` functor:

```reason
module MyAsyncFormHook = Formality.Async.Make(MyForm);
```

#### Async validations on blur
If you still want to use on blur validations just add `validateAsync` props to `validators` and use `Formality.Async.MakeOnBlur` to create form hook.

#### Note on defining async validators
When you define async validator you need to local open `Async` module like this:

```reason
module SignupForm = {
  open Formality;

  ...

  module EmailField = {
    let validator = Async.{ field: Email, ... };
  };
};
```

You will probably get `Warning 45` from compiler. You can address it either by:
1. Disabling this warning in `bsconfig.json`.

```json
"warnings": {
  "number": "-45"
}
```

2. Or change local open to this:

```reason
let validator = {
  Async.field: Email,
  strategy: OnFirstSuccessOrFirstBlur,
  ...
};
```
### Custom field comparator

By default Formality uses `Pervasives.compare` to compare fields internally. This works when forms are simple, but can cause troubles when fields contain uncomparable values, like functions. In such case `Pervasives.compare` will throw exception.
You can create form with your own comparator
```reason
module LoginForm = {
  type field =
    | Email
    | Password
    | RememberMe;

  type state = {
    email: string,
    password: string,
    rememberMe: bool,
  };

  type message = string;
  type submissionError = unit;

  let validators = [];

  module FieldId =
    Belt.Id.MakeComparable({
      type t = field;
      let cmp = Pervasives.compare;
    });
};
module LoginForm = Formality.MakeId(LoginForm);
```
Each Formality form functor has such helper-functor that requires FieldId

```reason
Formality.MakeId
Formality.Async.MakeId
Formality.Async.MakeOnBlurId
```

[Custom comparator usage examples](/examples/FormsWithFieldId.re)

### I18n
If you build i18n'ized app then set `message` type in form config to your `I18n.t` type. E.g.:

```reason
type message = I18n.t;
```

## Alternatives

- [ReForm](https://github.com/Astrocoders/reform)<br>
  Alternative form state management solution for ReasonReact apps.

## License
MIT.
