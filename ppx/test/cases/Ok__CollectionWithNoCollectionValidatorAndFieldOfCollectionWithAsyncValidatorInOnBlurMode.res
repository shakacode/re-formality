module Form = %form(
  type rec input = {authors: @field.collection array<author>}
  and author = {name: @field.async({mode: OnBlur}) string}
  let validators = {
    authors: {
      collection: None,
      fields: {
        name: {
          strategy: OnSubmit,
          validate: ({authors, _}, ~at) => Ok((authors->Belt.Array.getUnsafe(at)).name),
          validateAsync: name => Js.Promise.resolve(Ok(name)),
        },
      },
    },
  }
)
