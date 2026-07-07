module Form = %form(
  type rec input = {authors: @field.collection array<author>}
  and author = {
    name: string,
    age: int,
  }
  let validators = {
    authors: {
      collection: None,
      fields: {
        name: {
          strategy: OnSubmit,
          validate: ({authors, _}, ~at) => Ok((authors->Belt.Array.getUnsafe(at)).name),
        },
        age: {
          strategy: OnSubmit,
          validate: ({authors, _}, ~at) => Ok((authors->Belt.Array.getUnsafe(at)).age),
        },
      },
    },
  }
)
