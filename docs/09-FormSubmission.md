# Form Submission
`Formality` keeps track of the whole form status which can be in the following states:

```reason
type formStatus<'submissionError> =
  | Editing
  | Submitting(option<'submissionError>)
  | Submitted
  | SubmissionFailed('submissionError);
```

As it's been shown in **[Basic Usage](./04-BasicUsage.md)** section, to trigger form submission you need to call `form.submit` function. After that, `Formality` validates the whole form and if it's valid, it triggers `onSubmit` handler that's been passed to `useForm` hook. Before `onSubmit` gets triggered, `Formality` switches its status to `Submitting` and passes control to the application. Since it's not aware when the application is done with the submission, the latter should notify abstraction about the result so it would switch form status to either `Editing`, `Submitted` or `SubmissionFailed`.

## Submission callbacks
`onSubmit` handler takes 2 arguments: `output` data and callbacks. Let's look into the latter.

```reason
type submissionCallbacks<'input, 'submissionError> = {
  notifyOnSuccess: option<'input> => unit,
  notifyOnFailure: 'submissionError => unit,
  reset: unit => unit,
  dismissSubmissionResult: unit => unit,
};
```

### `notifyOnSuccess: option<'input> => unit`
This callback should be triggered after successful submission if you don't want to completely reset the form but set it to `Submitted` state preserving all internal statuses. Optionally, you can pass the next `input` that would replace the current one.

### `notifyOnFailure: 'submissionError => unit`
If submission fails, use this callback to notify hook about it. It takes one argument: `submissionError`.

When a form gets submitted, submission might fail for various reasons. It might be a bad password on a login attempt (expected error) or server crash (unexpected error), anything. This kind of error is specific to a form and you can provide an exact reason why submission failed via `notifyOnFailure` callback.

To be able to do this, you need to declare `submissionError` type in the config. E.g.:

```reason
type input = ...;
type output = ...;

type submissionError =
  | UserNotFound
  | BadPassword
  | UnexpectedServerError;
```

Then, when a response from the server is received, you can pass one of those constructors to the `notifyOnFailure` and it will be available via `form.status` variant in `SubmissionFailed<submissionError>` constructor.

If you don't need to parse submission errors in your form, skip this type and it would be set to `unit` under the hood:

```reason
type submissionError = unit;
```

### `reset: unit => unit`
This callback simply resets the form: its status gets set back to `Editing`, fields statuses get reset as well.

### `dismissSubmissionResult: unit => unit`
Use it when you want to dismiss alerts with errors from a server or success message without resetting a form. Under the hood, it changes a form status from `Submitted` or `SubmissionFailed` to `Editing`.

---

Next: **[I18n →](./10-I18n.md)**
