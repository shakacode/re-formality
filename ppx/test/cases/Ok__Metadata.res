module Email: {
  type t
  let \"+++": (t, string) => t
} = {
  type t = string
  let \"+++" = (x, y) => x ++ y
}

module Form = %form(
  type rec input = {
    name: string,
    emailOnChange: @field.async Email.t,
    emailOptionOnChange: @field.async option<Email.t>,
    emailStringOnChange: @field.async string,
    emailOptionStringOnChange: @field.async option<string>,
    emailOnBlur: @field.async({mode: OnBlur}) Email.t,
    emailOptionOnBlur: @field.async({mode: OnBlur}) option<Email.t>,
    emailStringOnBlur: @field.async({mode: OnBlur}) string,
    emailOptionStringOnBlur: @field.async({mode: OnBlur}) option<string>,
    users: @field.collection array<user>,
  }
  and user = {
    userName: string,
    userEmailOnChange: @field.async Email.t,
    userEmailOptionOnChange: @field.async option<Email.t>,
    userEmailStringOnChange: @field.async string,
    userEmailOptionStringOnChange: @field.async option<string>,
    userEmailOnBlur: @field.async({mode: OnBlur}) Email.t,
    userEmailOptionOnBlur: @field.async({mode: OnBlur}) option<Email.t>,
    userEmailStringOnBlur: @field.async({mode: OnBlur}) string,
    userEmailOptionStringOnBlur: @field.async({mode: OnBlur}) option<string>,
  }
  type metadata = string
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name, _}, metadata) => Ok(name ++ metadata),
    },
    emailOnChange: {
      strategy: OnSubmit,
      validate: ({emailOnChange, _}, metadata) => Ok({
        open Email
        \"+++"(emailOnChange, metadata)
      }),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(
          Ok({
            open Email
            \"+++"(email, metadata)
          }),
        ),
    },
    emailOptionOnChange: {
      strategy: OnSubmit,
      validate: ({emailOptionOnChange, _}, metadata) => Ok({
        open Email
        emailOptionOnChange->Belt.Option.map(x => \"+++"(x, metadata))
      }),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(
          Ok({
            open Email
            email->Belt.Option.map(x => \"+++"(x, metadata))
          }),
        ),
    },
    emailStringOnChange: {
      strategy: OnSubmit,
      validate: ({emailStringOnChange, _}, metadata) => Ok(emailStringOnChange ++ metadata),
      validateAsync: (email, metadata) => Js.Promise.resolve(Ok(email ++ metadata)),
    },
    emailOptionStringOnChange: {
      strategy: OnSubmit,
      validate: ({emailOptionStringOnChange, _}, metadata) => Ok(
        emailOptionStringOnChange->Belt.Option.map(x => x ++ metadata),
      ),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(Ok(email->Belt.Option.map(x => x ++ metadata))),
    },
    emailOnBlur: {
      strategy: OnSubmit,
      validate: ({emailOnBlur, _}, metadata) => Ok({
        open Email
        \"+++"(emailOnBlur, metadata)
      }),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(
          Ok({
            open Email
            \"+++"(email, metadata)
          }),
        ),
    },
    emailOptionOnBlur: {
      strategy: OnSubmit,
      validate: ({emailOptionOnBlur, _}, metadata) => Ok({
        open Email
        emailOptionOnBlur->Belt.Option.map(x => \"+++"(x, metadata))
      }),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(
          Ok({
            open Email
            email->Belt.Option.map(x => \"+++"(x, metadata))
          }),
        ),
    },
    emailStringOnBlur: {
      strategy: OnSubmit,
      validate: ({emailStringOnBlur, _}, metadata) => Ok(emailStringOnBlur ++ metadata),
      validateAsync: (email, metadata) => Js.Promise.resolve(Ok(email ++ metadata)),
    },
    emailOptionStringOnBlur: {
      strategy: OnSubmit,
      validate: ({emailOptionStringOnBlur, _}, metadata) => Ok(
        emailOptionStringOnBlur->Belt.Option.map(x => x ++ metadata),
      ),
      validateAsync: (email, metadata) =>
        Js.Promise.resolve(Ok(email->Belt.Option.map(x => x ++ metadata))),
    },
    users: {
      collection: (_input, _metadata) => Ok(),
      fields: {
        userName: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok(
            (users->Belt.Array.getUnsafe(at)).userName ++ metadata,
          ),
        },
        userEmailOnChange: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok({
            open Email
            \"+++"((users->Belt.Array.getUnsafe(at)).userEmailOnChange, metadata)
          }),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(
              Ok({
                open Email
                \"+++"(email, metadata)
              }),
            ),
        },
        userEmailOptionOnChange: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok({
            open Email
            (users->Belt.Array.getUnsafe(at)).userEmailOptionOnChange->Belt.Option.map(x =>
              \"+++"(x, metadata)
            )
          }),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(
              Ok({
                open Email
                email->Belt.Option.map(x => \"+++"(x, metadata))
              }),
            ),
        },
        userEmailStringOnChange: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok(
            (users->Belt.Array.getUnsafe(at)).userEmailStringOnChange ++ metadata,
          ),
          validateAsync: (email, metadata) => Js.Promise.resolve(Ok(email ++ metadata)),
        },
        userEmailOptionStringOnChange: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok(
            (users->Belt.Array.getUnsafe(at)).userEmailOptionStringOnChange->Belt.Option.map(x =>
              x ++ metadata
            ),
          ),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(Ok(email->Belt.Option.map(x => x ++ metadata))),
        },
        userEmailOnBlur: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok({
            open Email
            \"+++"((users->Belt.Array.getUnsafe(at)).userEmailOnBlur, metadata)
          }),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(
              Ok({
                open Email
                \"+++"(email, metadata)
              }),
            ),
        },
        userEmailOptionOnBlur: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok({
            open Email
            (users->Belt.Array.getUnsafe(at)).userEmailOptionOnBlur->Belt.Option.map(x =>
              \"+++"(x, metadata)
            )
          }),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(
              Ok({
                open Email
                email->Belt.Option.map(x => \"+++"(x, metadata))
              }),
            ),
        },
        userEmailStringOnBlur: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok(
            (users->Belt.Array.getUnsafe(at)).userEmailStringOnBlur ++ metadata,
          ),
          validateAsync: (email, metadata) => Js.Promise.resolve(Ok(email ++ metadata)),
        },
        userEmailOptionStringOnBlur: {
          strategy: OnSubmit,
          validate: ({users, _}, ~at, ~metadata) => Ok(
            (users->Belt.Array.getUnsafe(at)).userEmailOptionStringOnBlur->Belt.Option.map(x =>
              x ++ metadata
            ),
          ),
          validateAsync: (email, metadata) =>
            Js.Promise.resolve(Ok(email->Belt.Option.map(x => x ++ metadata))),
        },
      },
    },
  }
)
