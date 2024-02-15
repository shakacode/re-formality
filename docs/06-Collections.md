# Collections
Collection is an array that contains sets of fields that can be added or removed from a form.

## Configuration
To define a collection, you need to annotate a field in the `input` record with `@field.collection` attribute:

```reason
type rec input = {
  authors: @field.collection array<author>,
}
and author = {name: string};
```

Such field has few requirements:
1. It must be of `array<entry>` type
2. `entry` type must be a record which definition is inlined in the configuration module
3. `@field.collection` can't be combined with other attributes but each field in `entry` record can be handled as any other field in the `input` record. E.g. you can do this:

```reason
type rec input = {
  authors: @field.collection array<author>,
}
and author = {name: @field.async string};
```

Also, make sure the naming is consistent. E.g. annotated `authors` field (plural) holds an array of `author` records (singular).

When the `output` type is different, the implementation might look like this:

```reason
type rec input = {
  authors: @field.collection array<author>,
}
and author = {name: string};

type rec output = {
  authors: array<author'>,
}
and author' = {name: Author.name};
```

Collection validator has the following shape:

```reason
authors: {
  collection: input => result<unit, message>,
  fields: {
    name: (input, ~at: int) => result<[OUTPUT_TYPE_OF_FIELD], message>,
  }
}
```

Function under `collection` key validates collection as a whole. E.g. you might want to ensure that there is at least 1 author exists. If you don't want to perform any checks, set `collection: None`.

`fields` record holds fields of collection entry. It works the same way as with the general `input` field with small addition: validator function receives `~at` named argument which defines an index of entity that's being validated.

Here is an example of implementation:

```reason
let validators = {
  authors: {
    collection: input =>
      switch (input.authors) {
      | [||] => Error("There must be at least one author")
      | _ => Ok()
      },
    fields: {
      name: {
        strategy: OnFirstSuccessOrFirstBlur,
        validate: (input, ~at) => {
          switch (input.authors->Array.getUnsafe(at)) {
          | {name: ""} => Error("Author name is required")
          | {name} => Ok(name)
          };
        },
      },
    },
  },
};
```

Note that collections are not recursive, i.e. you can't have nested collections, one in another.

## Rendering
Getting the input and results, as well as handling addition, removal and field updates of the collection are possible via functions provided by the `useForm` hook. In the case of `authors` collection, you can use the following:

- `form.addAuthor({name: ""})`: adds `author` entry to collection
- `form.removeAuthor(~at: index)`: removes `author` entry from collection
- `form.blurAuthorName(~at: index)`: triggers blur in `author.name` field at index
- `form.updateAuthorName(~at: index, (input, 'inputValue) => input, 'inputValue)`: updates `author.name` field at index
- `form.authorNameResult(~at=index)`: returns validation result for `author.name` field at index
- `form.authorsResult`: returns result of the whole collection validation, if validator exists

And this is how it might look like in UI:

```reason
let form = MyForm.useForm(...);

<div>
  {
    form.input.authors
    ->Array.mapWithIndex((index, author) =>
        <>
          <input
            value={author.name}
            disabled={form.submitting}
            onBlur={_ => form.blurAuthorName(~at=index)}
            onChange={
              event =>
                form.updateAuthorName(
                  ~at=index,
                  (input, value) => {
                    ...input,
                    authors:
                      input.authors
                      ->Array.mapWithIndex((idx, author) =>
                          if (idx != index) {
                            author;
                          } else {
                            {name: value};
                          }
                        ),
                  },
                  event->ReactEvent.Form.target##value,
                )
            }
          />
          <button
            type_="button"
            onClick={_ => form.removeAuthor(~at=index)}>
            "Remove author"->React.string
          </button>
          {
            switch (form.authorNameResult(~at=index)) {
            | Some(Error(message)) =>
              <div className="error"> message->React.string </div>
            | Some(Ok(_))
            | None => React.null
            }
          }
        </>
      )
    ->React.array
  }
  <button type_="button" onClick={_ => form.addAuthor({name: ""})}>
    "Add author"->React.string
  </button>
  {switch (form.authorsResult) {
   | Some(Error(message)) =>
     <div className="error">
       message->React.string
     </div>
   | Some(Ok ())
   | None => React.null
   }}
</div>
```

---

Next: **[Dependent Fields →](./07-DependentFields.md)**
