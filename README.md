# Formality

Reasonable form validation tool for [`reason-react`](https://reasonml.github.io/reason-react/).

## Why

The main goal of the library is to simplify an implementation of forms validation preserving an excellent UX. It offers set of predefined strategies to handle different kinds of validation flows (incl. async validations).

## Examples

* Signup form [ [live](https://formality.now.sh/#signup) &middot; [source](examples/SignupForm.re) ]
* Login form [ [live](https://formality.now.sh/#login) &middot; [source](examples/LoginForm.re) ]

## 🚧 WIP 🚧

* [x] Base API
* [x] Validation strategies
* [x] Dependant fields validation
* [x] Async validations
* [ ] I18n compat
* [ ] Convert [test suit](https://github.com/shakacode/react-validation-layer/tree/master/__tests__)

## Installation

```shell
# yarn / npm
yarn add re-formality
npm install --save re-formality
```

## Usage

Docs are WIP. Here's the quick example. See [`examples`](examples/) for real-world cases.

> Also, you can read more on `strategies` [here](https://github.com/shakacode/react-validation-layer#propsstrategy)

```reason
module MyForm = {
  type field =
    | Email
    | Password;
  type state = {
    email: string,
    password: string
  };
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
  let strategy = Formality.Strategy.OnFirstSuccessOrFirstBlur;
  module Validators = Formality.MakeValidators({type t = field;});
  type validators = Validators.t(Formality.validator(field, state));
  let validators = Formality.(
    Validators.empty
    |> Validators.add(Email, {
         strategy: None, /* None means global strategy will be used, you can override it w/ Some(Formality.Strategy.t) */
         dependents: None, /* You can define fields which must be revalidated on change of this field's value */
         validate: (value, _state) => {
           switch value {
           | "" => Valid(false)
           | _ => Valid(true)
           }
         }
       })
    |> Validators.add(Password, {
         strategy: None, /* None means global strategy will be used, you can override it w/ Some(Formality.Strategy.t) */
         dependents: None, /* You can define fields which must be revalidated on change of this field's value */
         validate: (value, _state) => {
           switch value {
           | "" => Valid(false)
           | _ => Valid(true)
           }
         }
       })
    );
};

module FormContainer = Formality.Make(MyForm);

let component = ReasonReact.statelessComponent("MyForm");

let make = (_) => {
  ...component,
  render: (_) =>
    <FormContainer
      initialState={email: "", password: ""}
      onSubmit=((~notifyOnSuccess, ~notifyOnFailure, state) => {/* Submit form... */})>
      ...(
           ({state, results, change, blur, submit, submitting}) =>
             <form className="form" onSubmit=submit>
               <input
                 value=state.email
                 disabled=(submitting |> Js.Boolean.to_js_boolean)
                 onChange=(change(MyForm.Email))
                 onBlur=(blur(MyForm.Email))
               />
               (
                 switch (MyForm.Email |> results) {
                 | Some(Formality.Valid(valid)) =>
                   <div className=(Cn.make(["form-message", valid ? "success" : "failure"]))>
                     ((valid ? "Nice!" : "Uh oh error") |> ReasonReact.stringToElement)
                   </div>

                 | None => ReasonReact.nullElement
                 }
               )
               <input
                 value=state.password
                 disabled=(submitting |> Js.Boolean.to_js_boolean)
                 onChange=(change(MyForm.Password))
                 onBlur=(blur(MyForm.Password))
               />
               (
                 switch (MyForm.Password |> results) {
                 | Some(Formality.Valid(valid)) =>
                   <div className=(Cn.make(["form-message", valid ? "success" : "failure"]))>
                     ((valid ? "Nice!" : "Uh oh error") |> ReasonReact.stringToElement)
                   </div>
                 | None => ReasonReact.nullElement
                 }
               )
               <button disabled=(submitting |> Js.Boolean.to_js_boolean)>
                 ((submitting ? "Submitting..." : "Submit") |> ReasonReact.stringToElement)
               </button>
             </form>
         )
    </FormContainer>
};
```
