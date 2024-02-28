# History

## 4.0.0-beta.21
** This is the last release before library rename: `re-formality → rescript-formality` (unless there are bugs)**

* **[ BREAKING ]** Update to ReScript v11. All the functions are now uncurried. We don't offer curried option, sorry.
* **[ BREAKING ]** Remove Linux `arm64` binary.

Internal changes:
* PPX source code is converted from Reason to OCaml.
* ReScript library source code is converted from Reason to ReScript.

## 4.0.0-beta.20
* Fix Linux `arm64` binary name.

## 4.0.0-beta.19
* Dependencies update.

## 4.0.0-beta.18
* Add native Linux `arm64` support [#116](https://github.com/shakacode/re-formality/pull/116).

## 4.0.0-beta.17
* Add native macOS `arm64` support [#111](https://github.com/shakacode/re-formality/pull/111).

## 4.0.0-beta.16
* Add support for m1 through rosetta [#106](https://github.com/shakacode/re-formality/pull/106).

## 4.0.0-beta.15
* Fix linux build [#104](https://github.com/MinimaHQ/re-formality/pull/104).

## 4.0.0-beta.13
* Fix multiple dependent fields [#101](https://github.com/MinimaHQ/re-formality/pull/101).

## 4.0.0-beta.11
* **[ BREAKING ]** Switch to `@rescript/react`.
* **[ BREAKING ]** Update `bs-platform`.

## 4.0.0-beta.10
* Add `metadata`.

## 4.0.0-beta.9
* Fix Windows build.

## 4.0.0-beta.8
* Fix stack overflow in large forms.

## 4.0.0-beta.7
* Re-enable `dirty` function in interface.

## 4.0.0-beta.6
* Make all List calls in PPX tail-recursive.
* Temporary disable `dirty` function in interface due to [the BuckleScript issue](https://github.com/BuckleScript/bucklescript/issues/4327).

## 4.0.0-beta.5
* Make all fold calls in PPX tail-recursive.

## 4.0.0-beta.4
* Removed targets and changed update handlers API. See: [#68](https://github.com/MinimaHQ/re-formality/issues/68) & [#78](https://github.com/MinimaHQ/re-formality/pull/78).

## 4.0.0-beta.3
* Added `ReactDom` & `ReactNative` targets and changed update handlers API. See: [#68](https://github.com/MinimaHQ/re-formality/issues/68) & [#72](https://github.com/MinimaHQ/re-formality/pull/72).

## 4.0.0-beta.2
* Fixed `submissionError` type bug.

## 4.0.0-beta.1
**[ BREAKING ]** `Formality` got a new API and is implemented as PPX now. Even though update shouldn't be hard for most of the cases, it's better to read through [new docs](./docs) as we introduced some new concepts and features. Old implementation is still around and available via `FormalityCompat` interface.

## 3.2.0
* `bs-platform` updated to `v7`.

## 3.1.0
* Add `*WithId` functors to support custom comparator in configs (details: [#52](https://github.com/minima-app/re-formality/issues/52)).

## 3.0.0
* **[ BREAKING ]** React hooks support (`reason-react@0.7.0`).
* Add `FormalityCompat` to support `JSX@2`.

## 2.0.0
* **[ BREAKING ]** `bs-platform` updated to `5.0.0`.
* **[ BREAKING ]** Generalize payload of `FormStatus.SubmissionFailed` constructor ([#49](https://github.com/alexfedoseev/re-formality/pull/49)).
* Add `dismissSubmissionError`.
* Add `mapSubmissionError`.
* Keep submission error on a screen (if any) while re-submitting.
* Reset validation state to `Pristine` on successful submission ([#46](https://github.com/alexfedoseev/re-formality/pull/46) by [@steinararnason](https://github.com/steinararnason))

## 1.2.0
* Add `dismissSubmissionResult` to `submissionCallbacks` record.
* (Chore) Update `bs-platform`.

## 1.1.1
* (Chore) Update `re-debouncer`.

## 1.1.0
Add `form.reset()` function which resets the form to its initial state.

## 1.0.0
No changes.

## 1.0.0-beta.4
* Add `form.dirty()` function which returns `true` if any form field was touched, `false` otherwise.
* Add `form.valid()` function which returns `true` if all form fields are valid, `false` otherwise. Not available for forms w/ async validations.
* Namespace async types. You might need to local open `Async` module for async validators in form config. E.g.

```reason
let validator = Async.{ field: Email, ... };
```

## 1.0.0-beta.3
* Un-expose `React` module. I might accidentally broke some apps which have internal `React.re` module. Sorry!

## 1.0.0-beta.2
* **[ BREAKING ]** Validation `result` type is `Belt.Result.t(ok, message)`. Where `type ok = | Valid | NoValue`. `NoValue` tag should be used when optional field's value is empty.

## 1.0.0-beta.1
### Major
There are a number of big changes in public API. Higher level changes are outlined below. Please, see updated documentation, inspect [`src/Formality.rei`](./src/Formality.rei) and follow compiler warnings to update your forms. Also, see updated [examples](./examples).

* **[ BREAKING ]** Fast-pipe & data-first style.
* **[ BREAKING ]** `value` type is removed from config. It 100% decouples forms from particular value type.
* **[ BREAKING ]** Validation `result` type received new constructor: `Optional`. It should be used when optional field's value is empty. You can safely remove confusing `valueEmpty` function from configs. Make sure that all validators of optional fields are updated.
* **[ BREAKING ]** Validators are simply `list(validators)` now (instead of `Map`) and due to `value` removal each validator receives single argument: `state`.
* Switch to `Belt`.

### Deprecations
* `Formality.Dom.toValue*` & `Formality.Dom.toChecked*` helpers are deprecated in favor of common `ReasonReact` getters.

### Chore
* `bs-platform` updated to `4.0.6`.
* Use `re-debouncer`.

## 0.10.0
### Chore
* `bs-platform` updated to `4.0.5` & `reason-react` to `0.5.3`. Thanks **[@jihchi](https://github.com/jihchi)**!

## 0.9.1
### Improvements
* Added `Formality.Dom.toCheckedOnChange` & `Formality.Dom.toCheckedOnBlur` helpers.

## 0.9.0
### Improvements
* Added `form.dismissSubmissionResult` to dismiss server errors without resetting the form. Under the hood, it changes `FormStatus.Submitted` & `FormStatus.SubmissionFailed` statuses to `FormStatus.Editing`.

## 0.8.1
### Fixes
* Fixed emitting of invalid result when value is empty.

## 0.8.0
### Chore
* `bs-platform` updated to `3.0.0`. Thanks **[@jihchi](https://github.com/jihchi)**!

## 0.7.2
### Fixes
* Fixed Map comparator. Thanks **[@jihchi](https://github.com/jihchi)**!
* Fixed equality check in empty string helper. Thanks **[@rauanmayemir](https://github.com/rauanmayemir)**!

## 0.7.1
### Improvements
* Added interface file.
* Added docs.

## 0.7.0
### Features
* Form `status` is added.

```reason
type t('field, 'message) =
  | Editing
  | Submitting
  | Submitted
  | SubmissionFailed(list(('field, 'message)), option('message));
```

You can access this type via `Formality.FormStatus` module.

Current `status` is exposed via `form` argument of the `children` function. `form.submitting` is kept for convenience.

* **[ BREAKING ]** `onSubmit` handler is changed.

Submission callbacks:

```diff
- type notifiers = {
-   onSuccess: unit => unit,
-   onFailure: unit => unit,
- };

+ type submissionCallbacks('field, 'state, 'message) = {
+   notifyOnSuccess: option('state) => unit,
+   notifyOnFailure: (list(('field, 'message)), option('message)) => unit,
+   reset: unit => unit,
+ };
```

`onSubmit` prop:

```diff
- onSubmit=((state, {onSuccess, onFailure}) => ...)
+ onSubmit=((state, {notifyOnSuccess, notifyOnFailure, reset}) => ...)
```

Previously, if `onSuccess` was called, form was reset. Now, each callback sets appropriate form `status`, or you can explicitly `reset` a form. Also with this change, you can store errors returned from a server in form status `SubmissionFailed(list(('field, 'message)), option('message))` and render them in UI.

### Chore
* `bs-platform` is updated to `2.2.3`.

## 0.6.0
### Chore
* `bs-platform@^2.2.2` is added to `peerDependencies`.

## 0.5.0
### Features
* **[ BREAKING ]** `value` is user-defined type (was `string`).

In form config:

```diff
+ type value = string;
+ let valueEmpty = value => value === "";
/* or */
+ let valueEmpty = Formality.emptyString;
```

* **[ BREAKING ]** `Formality.Dom.valueOnChange` replaced with `Formality.Dom.toValueOnChange` and `Formality.Dom.valueOnBlur` replaced with `Formality.Dom.toValueOnBlur` to make DOM helpers more composable with non-string `value`. Also, subjectively, handlers are more transparent this way.

```diff
- onChange=(
-   LoginForm.Email
-   |> form.change
-   |> Formality.Dom.valueOnChange
- )

+ onChange=(
+   event =>
+     event
+     |> Formality.Dom.toValueOnChange
+     |> form.change(LoginForm.Email)
+ )
```

### Fixes
* Fix regressions related to empty values validation on form submission

### Chore
* `bs-platform` updated to `2.2.2`.

## 0.4.1
### Improvements
* In case of optional field (e.g. `"" => Valid`) if value is empty string container will always emit `None` (instead of `Some(Valid)`).

## 0.4.0
### Improvements
* **[ BREAKING ]** `validationResult` type is set back to variant. `meta` is removed.

```diff
- type validationResult('message) = {
-   valid: bool,
-   message: option('message),
-   meta: option(string)
- };

+ type validationResult('message) =
+ | Valid
+ | Invalid('message);
```

Validate function looks like this:

```reason
validate: (value, _state) =>
  switch value {
  | "" => Invalid("Uh oh error"),
  | _ => Valid
  }
```

* **[ BREAKING ]** `exception ImpossibleResult` is removed as with the change above we don't get into impossible state anymore! 🎉🎉🎉

## 0.3.1
### Improvements
* Validation `result` type is renamed to `validationResult` to avoid possible conflicts with Pervasive's `result`.

## 0.3.0
### Improvements
* **[ BREAKING ]** Validation `result` type is simplified. Now it's just record.

```diff
- type result('message) =
- | Valid(bool)
- | ValidityBag(validityBag('message));

+ type result('message) = {
+   valid: bool,
+   message: option('message),
+   meta: option(string)
+ };
```

Validate function looks like this:

```reason
validate: (value, _state) =>
  switch value {
  | "" => {
      valid: false,
      message: Some("Uh oh error"),
      meta: None
    }
  | _ => {
      valid: true,
      message: Some("Nice!"),
      meta: None
    }
  }
```

Thanks **[@thangngoc89](https://github.com/thangngoc89)** for suggestion!

## 0.2.0
### Improvements
* **[ BREAKING ]** Global form `strategy` type is removed. Now strategy is defined via field validator. It means `strategy` field is not `option` anymore.

```diff
- let strategy = Formality.Strategy.OnFirstSuccessOrFirstBlur;
```

```diff
- strategy: Some(Strategy.OnFirstSuccessOrFirstBlur)
+ strategy: Strategy.OnFirstSuccessOrFirstBlur

- strategy: None
+ strategy: Strategy.OnFirstSuccessOrFirstBlur
```

* **[ BREAKING ]** Signatures of `form.change` & `form.blur` handlers are changed. Now both accept `value` instead of events. You can use exposed helpers to get value from event.

```diff
- onChange=(Form.Field |> form.change)
- onBlur=(Form.Field |> form.blur)

+ onChange=(Form.Field |> form.change |> Formality.Dom.valueOnChange)
+ onBlur=(Form.Field |> form.blur |> Formality.Dom.valueOnBlur)
```

* **[ BREAKING ]** Signature of `form.submit` handler is changed. Now it accepts `unit` instead of event. You can use exposed helper to prevent default.

```diff
- <form onSubmit=form.submit>
+ <form onSubmit=(form.submit |> Formality.Dom.preventDefault)>
```

* **[ BREAKING ]** Formality doesn't trigger `event.preventDefault` on form submission anymore. Handle it via exposed helper `Formality.Dom.preventDefault` or however you like.

Thanks **[@grsabreu](https://github.com/grsabreu)** & **[@wokalski](https://github.com/wokalski)** for suggestions!

## 0.1.0
Initial release.
