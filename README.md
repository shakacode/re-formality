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
  - [Form container](#form-container)
  - [Rendering](#rendering)
  - [Async validations](#async-validations)
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
* Signup form: [live](https://re-formality.now.sh/#signup) &middot; [src](./examples/SignupForm.re)
* Login form: [live](https://re-formality.now.sh/#login) &middot; [src](./examples/LoginForm.re)

## Concepts
The main purpose of this library is to provide great form validation UX. To achieve this, `Formality` follows the following principle:

**Validation feedback should be provided as soon as possible but not too soon.**

The hardest part is to figure out the right moment when first validation results should be emitted in UI.

Let's break down the case with credit card field. A user opens a form and focuses on the field. When the first character is typed, the field is in an invalid state but it's not really polite to show an error at this point: we should let user a chance to finish what he's doing. While the user is typing, we wait. If after some character, validator reported valid result, it's a proper moment to indicate success in UI (e.g. show credit card type). But if the user left the field in an invalid state (e.g. moved to another field) we have all rights to emit an error. After the first result is emitted, we update validation state in UI on every change.

Sadly, form fields are different and credit card scenario is not universal. This is where strategies kick in.

### Strategies
We can't have a single scenario for all the cases but we can spot the most common ones, implement the logic of each and apply proper scenarios to specific form fields. To understand the behavior of each strategy, add the following prefix to its name: _"Start providing feedback in UI on..."_

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
It takes 3 steps to implement the form:

1. Define form config.
2. Create form container.
3. Render form container and form UI.

> If you want to jump straight into the code here's the example for you:

<details>
<summary>Spoiler</summary>

```reason
module MyForm = {
  type field =
    | Email
    | Password;
  type value = string;
  type state = {
    email: string,
    password: string
  };
  type message = string;
  let get = (field, state) =>
    switch field {
    | Email => state.email
    | Password => state.password
    };
  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Password, value) => {...state, password: value}
    };
  let valueEmpty = value => value === "";
  let strategy = Formality.Strategy.OnFirstSuccessOrFirstBlur;
  module Validators = Formality.MakeValidators({type t = field;});
  type validators = Validators.t(Formality.validator(field, value, state, message));
  let validators = Formality.(
    Validators.empty
    |> Validators.add(Email, {
         strategy,
         dependents: None,
         validate: (value, _state) =>
           switch value {
           | "" => Invalid("Uh oh error")
           | _ => Valid
           }
       })
    |> Validators.add(Password, {
         strategy,
         dependents: None,
         validate: (value, _state) =>
           switch value {
           | "" => Invalid("Uh oh error")
           | _ => Valid
           }
       })
    );
};

module FormContainer = Formality.Make(MyForm);

let component = "MyForm" |> ReasonReact.statelessComponent;

let make = (_) => {
  ...component,
  render: (_) =>
    <FormContainer
      initialState={email: "", password: ""}
      onSubmit=((state, {notifyOnSuccess, notifyOnFailure, reset}) => {
        /* Submit form and either notifyOnSuccess / notifyOnFailure / reset */
      })
    >
      ...(
            form =>
              <form
                className="form"
                onSubmit=(form.submit |> Formality.Dom.preventDefault)>
                <input
                  value=form.state.email
                  disabled=form.submitting
                  onChange=(
                    event =>
                      event
                      |> Formality.Dom.toValueOnChange
                      |> form.change(MyForm.Email)
                  )
                  onBlur=(
                    event =>
                      event
                      |> Formality.Dom.toValueOnBlur
                      |> form.blur(MyForm.Email)
                  )
                />
                (
                  switch (MyForm.Email |> form.results) {
                  | Some(Invalid(message)) =>
                    <div className=(Cn.make(["form-message", "failure"]))>
                      (message |> ReasonReact.stringToElement)
                    </div>
                  | Some(Valid)
                  | None => ReasonReact.nullElement
                  }
                )
                <input
                  value=form.state.password
                  disabled=form.submitting
                  onChange=(
                    event =>
                      event
                      |> Formality.Dom.toValueOnChange
                      |> form.change(MyForm.Password)
                  )
                  onBlur=(
                    event =>
                      event
                      |> Formality.Dom.toValueOnBlur
                      |> form.blur(MyForm.Password)
                  )
                />
                (
                  switch (MyForm.Password |> form.results) {
                  | Some(Invalid(message)) =>
                    <div className=(Cn.make(["form-message", "failure"]))>
                      (message |> ReasonReact.stringToElement)
                    </div>
                  | Some(Valid)
                  | None => ReasonReact.nullElement
                  }
                )
                <button disabled=form.submitting>
                  ((form.submitting ? "Submitting..." : "Submit") |> ReasonReact.stringToElement)
                </button>
              </form>
          )
    </FormContainer>
};
```
</details>

### Form config
Form config is a module:

```reason
module MyForm = {
  /* ... */
};
```

To make things happen, you must provide few types, values & functions. Depending on whether you need async validations or not, your config will require (or not) additional data. But most of the things are common for all types of forms. Let's start with the simplest case without async validations—this is what's required for all types of forms—and then async differences will be outlined in [Async validations](#async-validations) section.

#### `type field`
A variant where tags are form fields.

```reason
type field =
  | Email
  | Password;
```

#### `type value`
The type of the `value`. Feel free to set it to whatever you need.

The most common scenario is:

```reason
type value = string;
```

But if you want to store something else rather than only `string` you can define it as a variant like this:

```reason
type value =
  | String(string)
  | Int(int)
  | Date(Js.Date.t);
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
The type of the error messages that will be rendered in UI. Feel free to set it to whatever you need.

The most common scenario is:

```reason
type message = string;
```

If you build i18n'ized app then it's going to be something like this:

```reason
type message = I18n.t;
```

#### `let get: (field, state) => value`
Getter of the field value from state.

```reason
let get = (field, state) =>
  switch field {
  | Email => state.email
  | Password => state.password
  };
```

#### `let update: ((field, value), state) => state`
Updater of the value.

```reason
let update = ((field, value), state) =>
  switch (field, value) {
  | (Email, value) => {...state, email: value}
  | (Password, value) => {...state, password: value}
  };
```

#### `let valueEmpty: value => bool`
This function supposed to answers the question whether a provided `value` is _empty_ or not.

```reason
let valueEmpty = value => value === "";
```

This is required for the optional field case: `Formality` won't emit success if `value` in the optional field is empty. Since `value` type is abstraction defined in userland, `Formality` can't figure it out on its own.

For convenience, `Formality` exposes helper for the most common case:

```reason
let valueEmpty = Formality.emptyString;
```

#### `module Validators`
This is a module that will be populated with fields validators. Under the hood, this is `Map` where a key is of `field` type and a value is of `validator` type (see details below).

Same boilerplate for all forms:

```reason
module Validators = Formality.MakeValidators({
  type t = field;
});
```

#### `type validators`
Type of the validators container.

Same boilerplate for all forms:

```reason
type validators = Validators.t(
  Formality.validator(field, value, state, message)
);
```

#### `let validators: validators`
Finally, fields validators.

```reason
let validators = Formality.(
  Validators.empty
  |> Validators.add(Email, {
       strategy: Strategy.OnFirstSuccessOrFirstBlur,
       dependents: None,
       validate: (value, _state) =>
         switch value {
         | "" => Invalid("Uh oh error")
         | _ => Valid
         }
     })
);
```

As shown above, you have to create an empty map and then add validators for form fields:

```reason
Validators.empty
|> Validators.add(field1, validatorForField1)
|> Validators.add(field2, validatorForField2)
|> ...
```

##### `validator`
It's a record of 3 items:

```reason
type validationResult('message) =
  | Valid
  | Invalid('message);

type validator('field, 'value, 'state, 'message) = {
  strategy: Formality.Strategy.t,
  dependents: option(list('field)),
  validate: ('value, 'state) => validationResult('message),
};
```

###### `strategy`
See [Strategies](#strategies).
###### `dependents`
Optional list of fields that must be revalidated on a change in the current field. E.g. `PasswordConfirmation` must be revalidated on a change in `Password` field:

```reason
dependents: Some([PasswordConfirmation])
```

###### `validate`
A function that takes `value` and `state` and returns either `Valid` or `Invalid('message)`.

### Form container

To create form container simply do the following:

```reason
module MyFormContainer = Formality.Make(MyForm);
```

It creates renderable React component for general form.

If you render async forms, use:

```reason
/* Async validations on change (debounced) */
module MyAsyncFormContainer = Formality.MakeWithAsyncValidationsOnChange(MyForm);

/* Async validations on blur */
module MyAsyncFormContainer = Formality.MakeWithAsyncValidationsOnBlur(MyForm);
```

### Rendering

Form container accepts 4 props:

```reason
render: (_) =>
  <MyFormContainer
    initialState={email: "", password: ""}
    enableReinitialize=true
    onSubmit=((state, {notifyOnSuccess, notifyOnFailure, reset}) => {
      /* Submit form and either notifyOnSuccess / notifyOnFailure / reset */
    })
  >
    ...(form => /* UI */)
  </MyFormContainer>
```

#### `initialState`
It's `state` record with initial values for each form field.

#### `enableReinitialize`
Defaults to `false`. Controls whether reset the form if `initialState` changes (using deep equality).

#### `onSubmit`
This handler will be triggered on form submission (only when all validators returned `Valid`).

It accepts two arguments:
1. `state`: current state of the form
2. `submissionCallbacks`: record with 3 callbacks

```reason
type submissionCallbacks('field, 'state, 'message) = {
  notifyOnSuccess: option('state) => unit,
  notifyOnFailure: (list(('field, 'message)), option('message)) => unit,
  reset: unit => unit,
};
```

##### `notifyOnSuccess`
Trigger this callback when server responded with success. It accepts optional state argument: if it's provided, this state will be set as a next form state.

##### `notifyOnFailure`
Trigger this callback when server responded with an error. It accepts 2 arguments:
1. list of field-level errors
2. optional `message`: some information not directly related to any particular field

You can access this data in render via `form.status` (see below).

##### `reset`
Simply reset the form container state.

#### `form => UI`
Form container accepts children as a function.

`form` argument is a record that contains everything you need to render UI:

```reason
type form = {
  state: state,
  status: FormStatus.t,
  submitting: bool,
  results: field => option(validationResult),
  change: (field, value) => unit,
  blur: (field, value) => unit,
  submit: unit => unit,
  dismissSubmissionResult: unit => unit,
};
```

##### `form.state`
Form state, obviously. Use it to set `values` of the form fields.

##### `form.status`
Form status is variant:

```reason
module FormStatus = {
  type t('field, 'message) =
    | Editing
    | Submitting
    | Submitted
    | SubmissionFailed(list(('field, 'message)), option('message));
};
```

You can use it to show a spinner while a form is `Submitting`, or success message on `Submitted`, or display server errors using data that you passed to `notifyOnFailure` callback (it's available in `SubmissionFailed` tag payload).

##### `form.submitting`
This prop is passed for convenience (as you will need it to disable form inputs and button while a form is submitting). This is `true` when `form.status === Submitting`, `false` otherwise.

##### `form.results`
Use this function to get validation results for the field.

```reason
switch (MyForm.Email |> form.results) {
| Some(Invalid(message)) =>
  <div className="failure">
    (message |> ReasonReact.stringToElement)
  </div>
| Some(Valid)
| None => ReasonReact.nullElement
}
```

##### `form.change` & `form.blur`
These are handlers that must be triggered `onChange` and `onBlur`. Each accepts `field` and `value`.

When you deal with DOM you can use exposed helpers to extract string value from `event`:

```reason
<input
  value=form.state.email
  disabled=form.submitting
  onChange=(
    event =>
      event
      |> Formality.Dom.toValueOnChange
      |> form.change(MyForm.Email)
  )
  onBlur=(
    event =>
      event
      |> Formality.Dom.toValueOnBlur
      |> form.blur(MyForm.Email)
  )
/>
```

##### `form.submit`
Use it as `onSubmit` handler of the `<form />` element:

```reason
<form onSubmit=(form.submit |> Formality.Dom.preventDefault) />
```

##### `form.dismissSubmissionResult`
Use it when you want to let user dismissing alerts with errors from server or success message without resetting the form. Under the hood, it changes `FormStatus.Submitted` & `FormStatus.SubmissionFailed` statuses back to `FormStatus.Editing`.

### Async validations
Some validations can't be performed locally, e.g. on signup, you want to validate if user's email is available or it's already taken.

There are 2 common ways to provide async feedback: send a request to a server on every change or only on blur event. The first way is better in terms of UX but creates a significant load, so your client might become slow or the server might feel bad. The blur way doesn't have this problem (at least not that much) but UX is definitely not the best b/c user have to blur away from the field to receive the feedback.

What can we do about it to have the best of both worlds? The answer is to debounce async validations on change. What does it mean and how does it work: when a user types something in in the form field, no external requests are triggered. Instead, it's put on hold. While user types, we wait. Once he stopped and there was no activity in the certain period—async request is triggered.

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
In addition to `strategy`, `dependents` & `validate`, provide optional async validator. It takes `value` and returns `Js.Promise.t(validationResult)`:

```reason
type asyncValidator('field, 'value, 'state, 'message) = {
  strategy: Formality.Strategy.t,
  dependents: option(list('field)),
  validate: ('value, 'state) => validationResult('message),
  validateAsync: option('value => Js.Promise.t(validationResult('message))),
};
```

For example:

```reason
validateAsync: Some(
  value =>
    Js.Promise.(
      value
      |> Api.validateEmail
      |> then_(valid =>
           valid ?
             resolve(Valid) :
             resolve(Invalid("Email is already taken"))
         )
    ),
),
```

To create form container pass config to `Formality.MakeWithAsyncValidationsOnChange` functor:

```reason
module MyAsyncFormContainer = Formality.MakeWithAsyncValidationsOnChange(MyForm);
```

#### Async validations on blur
If you still want to use on blur validations just add `validateAsync` props to `validators` and use `Formality.MakeWithAsyncValidationsOnBlur` to create form container.


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
