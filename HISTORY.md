# 0.3.0
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

# 0.2.0
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

# 0.1.0
Initial release.
