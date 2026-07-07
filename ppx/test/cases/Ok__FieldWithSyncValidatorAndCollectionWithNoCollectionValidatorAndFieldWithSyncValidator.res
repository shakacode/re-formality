module Form = %form(
  type rec input = {
    title: string,
    authors: @field.collection array<author>,
  }
  and author = {name: string}
  let validators = {
    title: {
      strategy: OnSubmit,
      validate: ({title, _}) => Ok(title),
    },
    authors: {
      collection: None,
      fields: {
        name: {
          strategy: OnSubmit,
          validate: ({authors, _}, ~at) => Ok((authors->Belt.Array.getUnsafe(at)).name),
        },
      },
    },
  }
)
