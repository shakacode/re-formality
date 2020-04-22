module BlogPostForm = [%form
  type input = {
    title: string,
    authors: [@field.collection] array(author),
  }
  and author = {name: [@field.deps author.name] string};
  let validators = {
    title: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({title}) => {
        switch (title) {
        | "" => Error("Title is required")
        | _ => Ok(title)
        };
      },
    },
    authors: {
      collection: input =>
        switch (input.authors) {
        | [||] => Error("There must be at least one author")
        | _ => Ok()
        },
      fields: {
        name: {
          strategy: OnFirstSuccessOrFirstBlur,
          validate: ({authors}, ~at) => {
            switch (authors->Array.getUnsafe(at)) {
            | {name: ""} => Error("Author name is required")
            | {name}
                when
                  authors->Js.Array2.somei((author, idx) =>
                    if (at == idx) {
                      false;
                    } else {
                      author.name == name;
                    }
                  ) =>
              Error("Author name must be uniq")
            | {name} => Ok(name)
            };
          },
        },
      },
    },
  }
];

let initialInput: BlogPostForm.input = {title: "", authors: [|{name: ""}|]};

[@react.component]
let make = () => {
  let form =
    BlogPostForm.useForm(
      ~initialInput,
      ~onSubmit=(output, form) => {
        Js.log2("Submitted with:", output);
        Js.Global.setTimeout(
          () => {
            form.notifyOnSuccess(None);
            form.reset->Js.Global.setTimeout(3000)->ignore;
          },
          500,
        )
        ->ignore;
      },
    );

  <Form className="form" onSubmit={form.submit}>
    <div className="form-messages-area form-messages-area-lg" />
    <div className="form-content">
      <h2 className="push-lg"> "Blog post"->React.string </h2>
      <div className="form-row">
        <label htmlFor="blog-post--title" className="label-lg">
          "Title"->React.string
        </label>
        <input
          id="blog-post--title"
          type_="text"
          value={form.input.title}
          disabled={form.submitting}
          onBlur={_ => form.blurTitle()}
          onChange={event =>
            form.updateTitle(
              (input, value) => {...input, title: value},
              event->ReactEvent.Form.target##value,
            )
          }
        />
        {switch (form.titleResult) {
         | Some(Error(message)) =>
           <div
             className={Cn.make([
               "form-message",
               "form-message-for-field",
               "failure",
             ])}>
             message->React.string
           </div>
         | Some(Ok(_)) =>
           <div
             className={Cn.make([
               "form-message",
               "form-message-for-field",
               "success",
             ])}>
             {j|✓|j}->React.string
           </div>
         | None => React.null
         }}
      </div>
      <div className="form-row">
        <h3 className="push-lg"> "Authors"->React.string </h3>
        {switch (form.authorsResult) {
         | Some(Error(message)) =>
           <div
             className={Cn.make([
               "form-message",
               "form-message-for-field",
               "failure",
             ])}>
             message->React.string
           </div>
         | Some(Ok ())
         | None => React.null
         }}
      </div>
      {form.input.authors
       ->Array.mapWithIndex((index, author) => {
           let authorNameDomId =
             "blog-post--author-name" ++ index->Int.toString;

           <div key=authorNameDomId className="form-row">
             <label htmlFor=authorNameDomId className="label-lg">
               "Name"->React.string
             </label>
             <input
               id=authorNameDomId
               type_="text"
               value={author.name}
               disabled={form.submitting}
               onBlur={_ => form.blurAuthorName(~at=index)}
               onChange={event =>
                 form.updateAuthorName(
                   ~at=index,
                   (input, value) =>
                     {
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
               className="control"
               onClick={_ => form.removeAuthor(~at=index)}>
               {j|✕|j}->React.string
             </button>
             {switch (form.authorNameResult(~at=index)) {
              | Some(Error(message)) =>
                <div
                  className={Cn.make([
                    "form-message",
                    "form-message-for-field-of-collection",
                    "failure",
                  ])}>
                  message->React.string
                </div>
              | Some(Ok(_)) =>
                <div
                  className={Cn.make([
                    "form-message",
                    "form-message-for-field-of-collection",
                    "success",
                  ])}>
                  {j|✓|j}->React.string
                </div>
              | None => React.null
              }}
           </div>;
         })
       ->React.array}
      <div className="form-row">
        <button
          type_="button"
          className={Cn.make(["secondary", "push-lg"])}
          onClick={_ => form.addAuthor({name: ""})}>
          "Add author"->React.string
        </button>
      </div>
      <div className="form-row">
        <button
          className={Cn.make(["primary", "push-lg"])}
          disabled={form.submitting}>
          (form.submitting ? "Submitting..." : "Submit")->React.string
        </button>
        {switch (form.status) {
         | Submitted =>
           <div className={Cn.make(["form-status", "success"])}>
             {j|✓ Posted|j}->React.string
           </div>
         | _ => React.null
         }}
      </div>
    </div>
  </Form>;
};
