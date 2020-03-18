# Basic Usage
It takes 2 steps to implement a form:

1. Create a form hook.
2. Render a form UI.

## Creating a form hook
Form hook can be created using `[%form]` ppx extension. It requires at least 2 things:

- `input` type which must be a record
- `validators` record

Let's start with the `input`:

```reason
module LoginForm = [%form
  type input = {
    email: string,
    password: string,
  };
];
```

As mentioned in [**IO**](./03-IO.md) section, there should be an `output` type defined somewhere. If it's not provided, then under the hood it gets aliased to an `input` type. So the generated code would look like this:

```reason
module LoginForm = [%form
  type input = {
    email: string,
    password: string,
  };
  type output = input;
];
```

But since we want to deserialize form input into type-safe representation, we will provide our own `output` type with the `email` field set to `Email.t` type.

```reason
module LoginForm = [%form
  type input = {
    email: string,
    password: string,
  };
  type output = {
    email: Email.t,
    password: string,
  };
];
```

Worth mentioning, fields in the `output` type must be the same as in `input` type. Otherwise, it would be a compile-time error.

Another important detail regarding the `output` type is that you can't use external types as a value of this type. It must be a record defined in this module. I.e. this wouldn't work:

```reason
type output = LoginData.t;
```

One more optional type that is involved here is `message`—the type of error messages that would be displayed in UI. If an app doesn't implement internalization, you can skip this type and it would be set to `string` (this is what we're going to do in the current example). Otherwise, feel free to use your own type here. See **[I18n](./10-I18n.md)** section for more details.

The next thing to implement is a `validators`: a record with the same set of fields as in `input`/`output`, each holds instructions on how to validate a field. Let's implement one for `email` field, assuming that somewhere in the app there is an `Email` module that defines `Email.t` type and `Email.validate` function which takes `string` and returns `result(Email.t, string)`.

```reason
// Email.validate: string => result(Email.t, string)
let validators = {
  email: {
    strategy: OnFirstSuccessOrFirstBlur,
    validate: input => input.email->Email.validate,
  },
};
```

First of all, you don't need to define a type for `validators`. It's already done by the ppx. In the simplest possible case, field validator record has 2 entries:
1. `strategy`: as described in **[Validation Strategies](./02-ValidationStrategies.md)** section
2. `validate`: function that takes `input` as argument and returns `result([OUTPUT_TYPE_OF_FIELD], message)`. In the `email` case, it's `result(Email.t, message)`.

Pretty much the same applies to the `password` field:

```reason
let validators = {
  password: {
    strategy: OnFirstBlur,
    validate: input =>
      switch (input.password) {
      | "" => Error("Password is required")
      | _ => Ok(input.password)
      },
  },
};
```

Looks like we're done with the first step:

```reason
module LoginForm = [%form
  type input = {
    email: string,
    password: string,
  };

  type output = {
    email: Email.t,
    password: string,
  };

  let validators = {
    email: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: input => input.email->Email.validate,
    },
    password: {
      strategy: OnFirstBlur,
      validate: input =>
        switch (input.password) {
        | "" => Error("Password is required")
        | _ => Ok(input.password)
        },
    },
  };
];
```

## Rendering UI
The resulting module exposes the `useForm` hook that we are going to use for rendering form UI.

### `useForm` hook
```reason
[@react.component]
let make = () => {
  let form =
    LoginForm.useForm(
      ~initialInput={email: "", password: ""},
      ~onSubmit=(output, cb) => {
        // Skipping this for now...
      },
    );
};
```

`useForm` hook takes 2 arguments:
1. `initialInput`: a record of `input` type with initial field values
2. `onSubmit` function that takes `output` record and one more argument with a set of callbacks. We will get back to this a bit later.

As a result, we get a `form` record that holds everything we need to render UI.

### `<form />` tag
Let's start with the `<form />` tag:

```reason
[@react.component]
let make = () => {
  let form = LoginForm.useForm(...);

  <form onSubmit={_ => form.submit()}>
    ...
  </form>
};
```

To trigger submission, you need to call `form.submit` function. The best place to do this is `onSubmit` prop of a `<form />` tag. For clarity's sake, we skipped some DOM-related steps, but you can inspect [`Form`](../examples/src/Form.re) component from examples which includes those.

### Text input field
Next thing to render is a text input for `email` field:

```reason
<input
  value={form.input.email}
  disabled={form.submitting}
  onBlur={_ => form.blurEmail()}
  onChange={event => {
    let value = event->ReactEvent.Form.target##value;
    form.updateEmail(input => {...input, email: value});
  }}
/>
```

The value of the field is exposed via `form.input` record. For extra safety, we disable all inputs during form submission using `form.submitting` property which is of boolean type. The next 2 functions are very important:
1. `form.blurEmail: unit => unit`: must be triggered from `onBlur` handler of an input field
2. `form.updateEmail: (input => input) => unit`: must be triggered from `onChange` handler of an input field. It takes a function as an argument which takes the current form `input` and must return updated `input` record.

Please, make sure you don't capture the whole `event` in this callback. Otherwise, it would result in a runtime error.

```reason
// Bad, don't do this!
onChange={event => {
  form.updateEmail(input => {
    ...input,
    email: event->ReactEvent.Form.target##value,
  });
}}

// Good: extract value from event before passing it to the callback
onChange={event => {
  let value = event->ReactEvent.Form.target##value;
  form.updateEmail(input => {...input, email: value});
}}
```

This runtime error happens due to [React's `SyntheticEvent` being pooled](https://reactjs.org/docs/events.html#event-pooling). Since callback gets triggered asynchronously, by the time it gets called, the event is already null'ed by React.

### Messages
To display feedback in UI, we can use `form.emailResult` value. It's exactly what email validator returns but wrapped in `option` type:

```reason
{switch (form.emailResult) {
 | Some(Error(message)) =>
   <div className="error"> message->React.string </div>
 | Some(Ok(_))
 | None => React.null
 }}
```

When its value is `None`, it means it's not yet a good time to display any feedback to a user, according to the chosen strategy.

The same steps should be done for the `password` field.

### Submit button
Nothing special here:

```reason
<button disabled={form.submitting}>
  "Submit"->React.string
</button>
```

### Form submission
One more thing that needs to be handled is the submission of the form. When a user hits submit and the data is valid, hook triggers `onSubmit` function that was passed to it.

The implementation of this handler is always app-specific. When `onSubmit` handler gets triggered it receives 2 arguments: `output` of the form and set of callbacks that you might want to trigger in specific circumstances or just ignore them and do your own thing according to the requirements of your app.

In general, you would want to take the output and send it to your server asynchronously. When a response is received, there might be many scenarios:
- on success, redirect a user to another screen
- on success, reset the form
- on error, show errors from the server, etc.

In this example, we would stick with the simplest one. And elaborate on more advanced scenarios in **[Form Submission](./09-FormSubmission.md)** section.

So, the scenario is:
- on success, store a user in the app state and redirect the user to another screen
- on failure, display a generic error message

Assuming, there is `Api.loginUser` function in the app:

```reason
let form =
  LoginForm.useForm(
    ~initialInput={email: "", password: ""},
    ~onSubmit=(output, cb) => {
      output->Api.loginUser(res => switch (res) {
        | Ok(user) => user->AppShell.loginAndRedirect
        | Error() => cb.notifyOnFailure()
      });
    },
  );
```

When submission succeeded, the user gets redirected to another screen and form gets unmounted. At this point, we don't really care about its state and just fire `AppShell.loginAndRedirect` handler provided by the app (it's not specific to `Formality`).

But when submission fails, we need to display an error message in UI. So we need to let form hook know about failed submission by triggering `cb.notifyOnFailure()` handler passed in the second argument. What happens next?

Here, we need to mention `form.status`. Form hook tracks the status of the whole form which can be in the following states:

```reason
type formStatus('submissionError) =
  | Editing
  | Submitting(option('submissionError))
  | Submitted
  | SubmissionFailed('submissionError);
```

When `notifyOnFailure()` is triggered, form gets switched to the `SubmissionFailed()` status. So you can react on this change in the UI:

```reason
switch (form.status) {
| Editing
| Submitting(_)
| Submitted => React.null
| SubmissionFailed() =>
  <div className="error">
    "Not logged in"->React.string
  </div>
}
```

### Wrapping up
The whole implementation:

```reason
module LoginForm = [%form
  type input = {
    email: string,
    password: string,
  };

  type output = {
    email: Email.t,
    password: string,
  };

  let validators = {
    email: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: input => input.email->Email.validate,
    },
    password: {
      strategy: OnFirstBlur,
      validate: input =>
        switch (input.password) {
        | "" => Error("Password is required")
        | _ => Ok(input.password)
        },
    },
  };
];

[@react.component]
let make = () => {
  let form =
    LoginForm.useForm(
      ~initialInput={email: "", password: ""},
      ~onSubmit=(output, cb) => {
        output->Api.loginUser(res => switch (res) {
          | Ok(user) => user->AppShell.loginAndRedirect
          | Error() => cb.notifyOnFailure()
        });
      },
    );

  <form onSubmit={_ => form.submit()}>
    <input
      value={form.input.email}
      disabled={form.submitting}
      onBlur={_ => form.blurEmail()}
      onChange={event => {
        let value = event->ReactEvent.Form.target##value;
        form.updateEmail(input => {...input, email: value});
      }}
    />
    {switch (form.emailResult) {
     | Some(Error(message)) =>
       <div className="error"> message->React.string </div>
     | Some(Ok(_))
     | None => React.null
     }}
    <input
      value={form.input.password}
      disabled={form.submitting}
      onBlur={_ => form.blurPassword()}
      onChange={event => {
        let value = event->ReactEvent.Form.target##value;
        form.updatePassword(input => {...input, password: value});
      }}
    />
    {switch (form.passwordResult) {
     | Some(Error(message)) =>
       <div className="error"> message->React.string </div>
     | Some(Ok(_))
     | None => React.null
     }}
     <button disabled={form.submitting}>
       "Submit"->React.string
     </button>
     {switch (form.status) {
     | Editing
     | Submitting(_)
     | Submitted => React.null
     | SubmissionFailed() =>
       <div className="error">
        "Not logged in"->React.string
       </div>
     }}
  </form>
};
```

This is the most basic example which shows only a subset of use-cases. To find out more about advanced features, proceed to the next sections.

---

Next: **[Async Validation →](./05-AsyncValidation.md)**
