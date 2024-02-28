module BlogPostForm = %form(
  type rec input = {
    title: string,
    category: string,
    authors: @field.collection array<author>,
  }
  and author = {name: @field.deps(author.name) string}
  type metadata = {categories: array<string>}
  let validators = {
    title: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({title}, _metadata) =>
        switch title {
        | "" => Error("Title is required")
        | _ => Ok(title)
        },
    },
    category: {
      strategy: OnFirstChange,
      validate: ({category}, metadata) =>
        switch category {
        | "" => Error("Category is required")
        | _ if !(metadata.categories->Js.Array2.includes(category)) => Error("Invalid category")
        | _ => Ok(category)
        },
    },
    authors: {
      collection: (input, _metadata) =>
        switch input.authors {
        | [] => Error("There must be at least one author")
        | _ => Ok()
        },
      fields: {
        name: {
          strategy: OnFirstSuccessOrFirstBlur,
          validate: ({authors}, ~at, ~metadata as _) =>
            switch authors->Array.getUnsafe(at) {
            | {name: ""} => Error("Author name is required")
            | {name}
              if authors->Js.Array2.somei((author, idx) =>
                if at == idx {
                  false
                } else {
                  author.name == name
                }
              ) =>
              Error("Author name must be uniq")
            | {name} => Ok(name)
            },
        },
      },
    },
  }
)

let initialInput: BlogPostForm.input = {
  title: "",
  category: "",
  authors: [{name: ""}],
}

let categories = ["Design", "Tech", "Marketing", "Productivity"]

@react.component
let make = () => {
  let form = BlogPostForm.useForm(~initialInput, ~metadata={categories: categories}, ~onSubmit=(
    output,
    form,
  ) => {
    Js.log2("Submitted with:", output)
    Js.Global.setTimeout(() => {
      form.notifyOnSuccess(None)
      form.reset->Js.Global.setTimeout(3000)->ignore
    }, 500)->ignore
  })

  <Form className="form" onSubmit=form.submit>
    <div className="form-messages-area form-messages-area-lg" />
    <div className="form-content">
      <h2 className="push-lg"> {"Blog post"->React.string} </h2>
      <div className="form-row">
        <label htmlFor="blog-post--title" className="label-lg"> {"Title"->React.string} </label>
        <input
          id="blog-post--title"
          type_="text"
          value=form.input.title
          disabled=form.submitting
          onBlur={_ => form.blurTitle()}
          onChange={event =>
            form.updateTitle(
              (input, value) => {...input, title: value},
              (event->ReactEvent.Form.target)["value"],
            )}
        />
        {switch form.titleResult {
        | Some(Error(message)) =>
          <div className={cx(["form-message", "form-message-for-field", "failure"])}>
            {message->React.string}
          </div>
        | Some(Ok(_)) =>
          <div className={cx(["form-message", "form-message-for-field", "success"])}>
            {"✓"->React.string}
          </div>
        | None => React.null
        }}
      </div>
      <div className="form-row">
        <label htmlFor="blog-post--category" className="label-lg">
          {"Category"->React.string}
        </label>
        <select
          id="blog-post--category"
          type_=""
          value=form.input.category
          disabled=form.submitting
          onBlur={_ => form.blurCategory()}
          onChange={event =>
            form.updateCategory(
              (input, value) => {...input, category: value},
              (event->ReactEvent.Form.target)["value"],
            )}>
          <option value="" default=true disabled=true hidden=true>
            {"Select category..."->React.string}
          </option>
          {categories
          ->Array.map(category =>
            <option key=category value=category> {category->React.string} </option>
          )
          ->React.array}
        </select>
        {switch form.categoryResult {
        | Some(Error(message)) =>
          <div className={cx(["form-message", "form-message-for-field", "failure"])}>
            {message->React.string}
          </div>
        | Some(Ok(_)) =>
          <div className={cx(["form-message", "form-message-for-field", "success"])}>
            {"✓"->React.string}
          </div>
        | None => React.null
        }}
      </div>
      <div className="form-row">
        <h3 className="push-lg"> {"Authors"->React.string} </h3>
        {switch form.authorsResult {
        | Some(Error(message)) =>
          <div className={cx(["form-message", "form-message-for-field", "failure"])}>
            {message->React.string}
          </div>
        | Some(Ok())
        | None => React.null
        }}
      </div>
      {form.input.authors
      ->Array.mapWithIndex((index, author) => {
        let authorNameDomId = "blog-post--author-name" ++ index->Int.toString

        <div key=authorNameDomId className="form-row">
          <label htmlFor=authorNameDomId className="label-lg"> {"Name"->React.string} </label>
          <input
            id=authorNameDomId
            type_="text"
            value=author.name
            disabled=form.submitting
            onBlur={_ => form.blurAuthorName(~at=index)}
            onChange={event =>
              form.updateAuthorName(
                ~at=index,
                (input, value) => {
                  ...input,
                  authors: input.authors->Array.mapWithIndex((idx, author) =>
                    if idx != index {
                      author
                    } else {
                      {
                        name: value,
                      }
                    }
                  ),
                },
                (event->ReactEvent.Form.target)["value"],
              )}
          />
          <button type_="button" className="control" onClick={_ => form.removeAuthor(~at=index)}>
            {"✕"->React.string}
          </button>
          {switch form.authorNameResult(~at=index) {
          | Some(Error(message)) =>
            <div
              className={cx(["form-message", "form-message-for-field-of-collection", "failure"])}>
              {message->React.string}
            </div>
          | Some(Ok(_)) =>
            <div
              className={cx(["form-message", "form-message-for-field-of-collection", "success"])}>
              {"✓"->React.string}
            </div>
          | None => React.null
          }}
        </div>
      })
      ->React.array}
      <div className="form-row">
        <button
          type_="button"
          className={cx(["secondary", "push-lg"])}
          onClick={_ => form.addAuthor({name: ""})}>
          {"Add author"->React.string}
        </button>
      </div>
      <div className="form-row">
        <button className={cx(["primary", "push-lg"])} disabled=form.submitting>
          {(form.submitting ? "Submitting..." : "Submit")->React.string}
        </button>
        {switch form.status {
        | Submitted =>
          <div className={cx(["form-status", "success"])}> {"✓ Posted"->React.string} </div>
        | _ => React.null
        }}
      </div>
    </div>
  </Form>
}
