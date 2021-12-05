# Dependent Fields
If the validity of one field depends on the value of another, use `[@field.deps]` attribute to mark the relation.

```reason
type input = {
  a: [@field.deps b] string,
  b: string,
};
```

 In the case above, it states: "If the value of field `a` has changed, please, re-validate field `b` as well".

A real-world use-case is a form that updates a password:

```reason
type input = {
  oldPassword string,
  newPassword: [@field.deps newPasswordConfirmation] string,
  newPasswordConfirmation: string,
};
```

When `newPassword` field is updated, `newPasswordConfirmation` field should be revalidated as well since its validity depends on the `newPassword` value.

If you need to re-validate multiple fields, provide a tuple:

```reason
type input = {
  a: [@field.deps (b, c)] string,
  b: string,
  c: int,
};
```

If one of the dependent fields is a field of collection, define it like this:

```reason
type input = {
  title: [@field.deps author.name] string,
  authors: [@field.collection] array(author),
}
and author = {name: string}
```

---

Next: **[Metadata →](./08-Metadata.md)**
