# Async Validation
Some validations can't be performed locally. E.g. on signup, you want to validate if a user's email is available or it's already taken.

There are 2 common ways to provide async feedback: send a request to a server on every change or only on blur event. The first way is better in terms of UX but creates a significant load, so your client might become slow or a server might feel bad. The blur way doesn't have this problem (at least not that much) but UX is definitely not the best because a user has to left a field to receive feedback.

What can we do about it to have the best of both worlds? An answer is to debounce async validations on change. What does it mean and how does it work: when a user types something in in a form field, no external requests are triggered. Instead, it's put on hold. While the user is typing, we're waiting. When the user stopped and there was no activity in a certain period of time—async request gets triggered.

## Debounced async validations on change
To implement debounced async validations, you need to annotate your input field:

```reason
type input = {
  email: [@field.async] string,
};
```

And update the validator: in addition to the `strategy` and `validate` entries, add `validateAsync` function which takes value of `output` type of the field and returns `Js.Promise.t(result([OUTPUT_TYPE_OF_FIELD], message))`. In case of the `email` field, it would be `Js.Promise.t(result(Email.t, message))`.

```reason
type input = {
  email: [@field.async] string,
};

type output = {
  email: Email.t,
};

let validators = {
  email: {
    strategy: OnFirstSuccessOrFirstBlur,
    validate: input => input.email->Email.validate,
    validateAsync: email =>
      Js.Promise.(
        Api.validateEmail(email)
        ->then_(
            valid =>
              valid
                ? Ok(email)->resolve
                : Error("Email is already taken")->resolve,
            _,
          )
      ),
  },
};
```

On the rendering side of things, there is only one change. The type of field result is a bit different:

```reason
type asyncFieldStatus('outputValue, 'message) =
  | Validating('outputValue)
  | Result(result('outputValue, 'message));
```

So in UI it would look like this:

```reason
{switch (form.emailResult) {
 | Some(Validating(_)) => <Spinner />
 | Some(Result(Error(message))) =>
   <div className="error"> message->React.string </div>
 | Some(Result(Ok(_)))
 | None => React.null
 }}
```

### Additional configuration options
#### Debounce interval
You can configure the amount of time (in ms) that `Formality` should wait since last user activity before invoking debounced async validation. By default, it's set to `700` bu you can change this by providing your own value in the config like this:

```reason
type input = ...;
type output = ...;

let debounceInterval = 1000;

let validators = ...;
```

#### Value equality
One more thing that you might want to configure is the value equality function.

It takes some time to get a response from serve after async validation is triggered. By the time when server responded with some result, the local value might be already changed so before setting received result `Formality` checks if the value of the field is the same that was validated. And if it's not it ignores this result. To perform such check, it uses `validator.eq` function which is by default set to `(==)`.

When you would want to change it? Consider the `Email.t` type being a record under the hood:

```reason
module Email = {
  type t = {
    user: string,
    domain: string,
  };
};
```

Efficiently, 2 emails are equal when their `user` and `domain` fields are equal. So such type can implement own equality function which would be more efficient performance-wise than the standard `(==)`:

```reason
module Email = {
  type t = {
    user: string,
    domain: string,
  };

  let (==) = (x1, x2) => x1.user == x2.user && x1.domain == x2.domain;
};

// And then provide it to validators
let validators = {
  email: {
    strategy: OnFirstSuccessOrFirstBlur,
    validate: input => ...,
    validateAsync: email => ...,
    eq: Email.(==),
  },
};
```

## Async validations on blur
If you want to trigger async validations on blur, define mode explicitly:

```reason
type input = {
  email: [@field.async {mode: OnBlur}] string,
};
```

---

Next: **[Collections →](./06-Collections.md)**
