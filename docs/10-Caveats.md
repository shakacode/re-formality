# Caveats

- [Runtime error during input update](runtime-error-during-input-update)
- [Warning 42 during compilation](warning-42-during-compilation)

## Runtime error during input update
After implementing a form, you might face app crash when you type something in input field with this error in a browser console:

```
Warning: This synthetic event is reused for performance reasons. If you're seeing this, you're accessing the property `target` on a released/nullified synthetic event. This is set to null. If you must keep the original synthetic event around, use event.persist(). See https://fb.me/react-event-pooling for more information.

Uncaught TypeError: Cannot read property 'value' of null
```

This runtime error happens due to [React's `SyntheticEvent` being pooled](https://reactjs.org/docs/events.html#event-pooling).

To fix this, make sure you don't capture the whole `event` in updater callback:

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

Since callback gets triggered asynchronously, by the time it gets called, the event is already null'ed by React.

## Warning 42 during compilation
This warning is disabled by default but if you explicitly enabled it, you might see the following message during a compilation:

```
Warning 42: this use of [field] relies on type-directed disambiguation, it will not compile with OCaml 4.00 or earlier.
```

To get rid of it, consider to disable it either globally via `bsconfig.json` or locally in form modules:

```reason
[@ocaml.warning "-42"];
```

---

Next: **[API →](./11-API.md)**
