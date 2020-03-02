module BlogPostForm = {
  open Formality;
  type input = {
    title: string,
    authors: [@field.collection] array(author),
  }
  and author = {
    name: [@field.deps author.name] string,
    age: int,
  };
  type output = input;
  type message = string;
  type submissionError = unit;
  type fieldsStatuses = {
    title: fieldStatus(string, message),
    authors: array(authorFieldsStatuses),
  }
  and authorFieldsStatuses = {
    age: fieldStatus(int, message),
    name: fieldStatus([@field.deps author.name] string, message),
  };
  type collectionsStatuses = {authors: option(collectionStatus(message))};
  type state = {
    input,
    fieldsStatuses,
    collectionsStatuses,
    formStatus: formStatus(submissionError),
    submissionStatus,
  };
  type action =
    | UpdateAuthorNameField(input, index)
    | UpdateAuthorAgeField(input, index)
    | UpdateTitleField(input)
    | BlurAuthorAgeField(index)
    | BlurAuthorNameField(index)
    | BlurTitleField
    | AddAuthorEntry(author)
    | RemoveAuthorEntry(index)
    | Submit
    | SetSubmittedStatus(option(input))
    | SetSubmissionFailedStatus(submissionError)
    | MapSubmissionError(submissionError => submissionError)
    | DismissSubmissionError
    | DismissSubmissionResult
    | Reset;
  type validators = {
    title: singleValueValidator(input, string, message),
    authors:
      collectionValidatorWithWholeCollectionValidator(
        input,
        message,
        authorsValidators,
      ),
  }
  and authorsValidators = {
    age: unit,
    name:
      valueOfCollectionValidator(
        input,
        [@field.deps author.name] string,
        message,
      ),
  };
  type interface = {
    updateAuthorName: (input, ~at: index) => unit,
    updateAuthorAge: (input, ~at: index) => unit,
    updateTitle: input => unit,
    blurAuthorName: (~at: index) => unit,
    blurAuthorAge: (~at: index) => unit,
    blurTitle: unit => unit,
    authorNameResult:
      (~at: index) =>
      option(result([@field.deps author.name] string, message)),
    authorAgeResult: (~at: index) => option(result(int, message)),
    titleResult: option(result(string, message)),
    addAuthor: author => unit,
    removeAuthor: (~at: index) => unit,
    authorsResult: option(collectionStatus(message)),
    input,
    status: formStatus(submissionError),
    dirty: unit => bool,
    valid: unit => bool,
    submitting: bool,
    submit: unit => unit,
    dismissSubmissionError: unit => unit,
    dismissSubmissionResult: unit => unit,
    mapSubmissionError: (submissionError => submissionError) => unit,
    reset: unit => unit,
  };
  let validators: validators = {
    title: {
      strategy: OnFirstSuccessOrFirstBlur,
      validate: ({title}) =>
        switch (title) {
        | "" => Error("Title is required")
        | _ => Ok(title)
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
          validate: ({authors}, ~at as index) =>
            switch (authors->Array.getUnsafe(index)) {
            | {name: ""} => Error("Author name is required")
            | {name}
                when
                  authors->Js.Array2.somei((author, idx) =>
                    if (index == idx) {
                      false;
                    } else {
                      author.name == name;
                    }
                  ) =>
              Error("Author name must be uniq")
            | {name} => Ok(name)
            },
        },
        age: (),
      },
    },
  };
  let initialFieldsStatuses = (input: input): fieldsStatuses => {
    title: Pristine,
    authors:
      Belt.Array.make(
        Belt.Array.length(input.authors),
        {age: Pristine, name: Pristine}: authorFieldsStatuses,
      ),
  };
  let initialCollectionsStatuses = {authors: None};
  let initialState = input => {
    input,
    fieldsStatuses: input->initialFieldsStatuses,
    collectionsStatuses: initialCollectionsStatuses,
    formStatus: Editing,
    submissionStatus: NeverSubmitted,
  };
  let validateForm =
      (input: input, ~validators: validators, ~fieldsStatuses: fieldsStatuses)
      : formValidationResult(output, fieldsStatuses, collectionsStatuses) =>
    switch (
      (
        switch (fieldsStatuses.title) {
        | Pristine => validators.title.validate(input)
        | Dirty(result, _) => result
        },
        Shown,
      ),
      (
        validators.authors.collection(input),
        Belt.Array.reduceWithIndex(
          fieldsStatuses.authors,
          (Ok([||]), [||]),
          (
            (
              output: result(array(author), unit),
              statuses: array(authorFieldsStatuses),
            ),
            fieldStatus,
            index,
          ) =>
          switch (
            output,
            (Ok(Array.getUnsafe(input.authors, index).age), Hidden),
            (
              switch (fieldStatus.name) {
              | Pristine =>
                validators.authors.fields.name.validate(input, ~at=index)
              | Dirty(result, _) => result
              },
              Shown,
            ),
          ) {
          | (
              Ok(output),
              (Ok(age) as ageResult, ageResultVisibility),
              (Ok(name) as nameResult, nameResultVisibility),
            ) =>
            ignore(Js.Array2.push(output, {age, name}));
            ignore(
              Js.Array2.push(
                statuses,
                {
                  age: Dirty(ageResult, ageResultVisibility),
                  name: Dirty(nameResult, nameResultVisibility),
                },
              ),
            );
            (Ok(output), statuses);
          | (
              _,
              (ageResult, ageResultVisibility),
              (nameResult, nameResultVisibility),
            ) =>
            ignore(
              Js.Array2.push(
                statuses,
                {
                  age: Dirty(ageResult, ageResultVisibility),
                  name: Dirty(nameResult, nameResultVisibility),
                },
              ),
            );
            (Error(), statuses);
          }
        ),
      ),
    ) {
    | (
        (Ok(title) as titleResult, titleResultVisibility),
        (
          Ok () as authorsCollectionResult,
          (Ok(authors), authorsCollectionFieldsStatuses),
        ),
      ) =>
      Valid({
        output: {
          title,
          authors,
        },
        fieldsStatuses: {
          title: Dirty(titleResult, titleResultVisibility),
          authors: authorsCollectionFieldsStatuses,
        },
        collectionsStatuses: {
          authors: Some(authorsCollectionResult),
        },
      })
    | (
        (titleResult, titleResultVisibility),
        (authorsCollectionResult, (_, authorsCollectionFieldsStatuses)),
      ) =>
      Invalid({
        fieldsStatuses: {
          title: Dirty(titleResult, titleResultVisibility),
          authors: authorsCollectionFieldsStatuses,
        },
        collectionsStatuses: {
          authors: Some(authorsCollectionResult),
        },
      })
    };
  let useForm =
      (
        ~initialInput: input,
        ~onSubmit:
           (output, submissionCallbacks(input, submissionError)) => unit,
      ) => {
    let memoizedInitialState =
      React.useMemo1(() => initialInput->initialState, [|initialInput|]);
    let (state, dispatch) =
      memoizedInitialState->ReactUpdate.useReducer((state, action) =>
        switch (action) {
        | UpdateAuthorNameField(nextInput, index) =>
          let nextFieldsStatuses = ref(state.fieldsStatuses);
          Belt.Array.forEachWithIndex(
            nextFieldsStatuses^.authors, (index', item) =>
            if (index != index') {
              switch (
                validateDependentFieldOfCollectionOnChange(
                  ~input=nextInput,
                  ~index=index',
                  ~fieldStatus=item.name,
                  ~validator=validators.authors.fields.name,
                  ~setStatus=status =>
                  {
                    ...nextFieldsStatuses^,
                    authors:
                      Belt.Array.mapWithIndex(
                        nextFieldsStatuses^.authors, (idx_, item) =>
                        if (idx_ == index') {
                          {...item, name: status};
                        } else {
                          item;
                        }
                      ),
                  }
                )
              ) {
              | Some(result) => nextFieldsStatuses := result
              | None => ()
              };
            } else {
              ();
            }
          );
          Update({
            ...state,
            input: nextInput,
            fieldsStatuses:
              validateFieldOfCollectionOnChangeWithValidator(
                ~input=nextInput,
                ~index,
                ~fieldStatus=
                  Belt.Array.getUnsafe(nextFieldsStatuses^.authors, index).
                    name,
                ~submissionStatus=state.submissionStatus,
                ~validator=validators.authors.fields.name,
                ~setStatus=status =>
                {
                  ...nextFieldsStatuses^,
                  authors:
                    Belt.Array.mapWithIndex(
                      nextFieldsStatuses^.authors, (idx_, item) =>
                      if (idx_ == index) {
                        {...item, name: status};
                      } else {
                        item;
                      }
                    ),
                }
              ),
          });
        | UpdateAuthorAgeField(nextInput, index) =>
          Update({
            ...state,
            input: nextInput,
            fieldsStatuses:
              validateFieldOnChangeWithoutValidator(
                ~fieldInput=
                  Belt.Array.getUnsafe(nextInput.authors, index).age,
                ~setStatus=status =>
                {
                  ...state.fieldsStatuses,
                  authors:
                    Belt.Array.mapWithIndex(
                      state.fieldsStatuses.authors, (index', item) =>
                      if (index' == index) {
                        {...item, age: status};
                      } else {
                        item;
                      }
                    ),
                }
              ),
          })
        | UpdateTitleField(nextInput) =>
          Update({
            ...state,
            input: nextInput,
            fieldsStatuses:
              validateFieldOnChangeWithValidator(
                ~input=nextInput,
                ~fieldStatus=state.fieldsStatuses.title,
                ~submissionStatus=state.submissionStatus,
                ~validator=validators.title,
                ~setStatus=status =>
                {...state.fieldsStatuses, title: status}
              ),
          })
        | BlurAuthorNameField(index) =>
          let result =
            validateFieldOfCollectionOnBlurWithValidator(
              ~input=state.input,
              ~index,
              ~fieldStatus=
                Belt.Array.getUnsafe(state.fieldsStatuses.authors, index).
                  name,
              ~validator=validators.authors.fields.name,
              ~setStatus=status =>
              {
                ...state.fieldsStatuses,
                authors:
                  Belt.Array.mapWithIndex(
                    state.fieldsStatuses.authors, (index', item) =>
                    if (index' == index) {
                      {...item, name: status};
                    } else {
                      item;
                    }
                  ),
              }
            );
          switch (result) {
          | Some(fieldsStatuses) => Update({...state, fieldsStatuses})
          | None => NoUpdate
          };
        | BlurAuthorAgeField(index) =>
          let result =
            validateFieldOnBlurWithoutValidator(
              ~fieldInput=
                Belt.Array.getUnsafe(state.input.authors, index).age,
              ~fieldStatus=
                Belt.Array.getUnsafe(state.fieldsStatuses.authors, index).age,
              ~setStatus=status =>
              {
                ...state.fieldsStatuses,
                authors:
                  Belt.Array.mapWithIndex(
                    state.fieldsStatuses.authors, (index', item) =>
                    if (index' == index) {
                      {...item, age: status};
                    } else {
                      item;
                    }
                  ),
              }
            );
          switch (result) {
          | Some(fieldsStatuses) => Update({...state, fieldsStatuses})
          | None => NoUpdate
          };
        | BlurTitleField =>
          let result =
            validateFieldOnBlurWithValidator(
              ~input=state.input,
              ~fieldStatus=state.fieldsStatuses.title,
              ~validator=validators.title,
              ~setStatus=status =>
              {...state.fieldsStatuses, title: status}
            );
          switch (result) {
          | Some(fieldsStatuses) => Update({...state, fieldsStatuses})
          | None => NoUpdate
          };
        | AddAuthorEntry(entry) =>
          let nextInput = {
            ...state.input,
            authors: Belt.Array.concat(state.input.authors, [|entry|]),
          };
          let nextFieldsStatuses =
            ref({
              ...state.fieldsStatuses,
              authors:
                Belt.Array.concat(
                  state.fieldsStatuses.authors,
                  [|{age: Pristine, name: Pristine}|],
                ),
            });
          Belt.Array.forEachWithIndex(
            nextFieldsStatuses^.authors, (index', item) =>
            switch (
              validateDependentFieldOfCollectionOnChange(
                ~input=nextInput,
                ~index=index',
                ~fieldStatus=item.name,
                ~validator=validators.authors.fields.name,
                ~setStatus=status =>
                {
                  ...nextFieldsStatuses^,
                  authors:
                    Belt.Array.mapWithIndex(
                      nextFieldsStatuses^.authors, (idx_, item) =>
                      if (idx_ == index') {
                        {...item, name: status};
                      } else {
                        item;
                      }
                    ),
                }
              )
            ) {
            | Some(result) => nextFieldsStatuses := result
            | None => ()
            }
          );
          Update({
            ...state,
            input: nextInput,
            fieldsStatuses: nextFieldsStatuses^,
            collectionsStatuses: {
              authors: Some(validators.authors.collection(nextInput)),
            },
          });
        | RemoveAuthorEntry(index) =>
          let nextInput = {
            ...state.input,
            authors:
              Belt.Array.keepWithIndex(state.input.authors, (_, i) =>
                i != index
              ),
          };
          let nextFieldsStatuses =
            ref({
              ...state.fieldsStatuses,
              authors:
                Belt.Array.keepWithIndex(state.fieldsStatuses.authors, (_, i) =>
                  i != index
                ),
            });
          Belt.Array.forEachWithIndex(
            nextFieldsStatuses^.authors, (index', item) =>
            switch (
              validateDependentFieldOfCollectionOnChange(
                ~input=nextInput,
                ~index=index',
                ~fieldStatus=item.name,
                ~validator=validators.authors.fields.name,
                ~setStatus=status =>
                {
                  ...nextFieldsStatuses^,
                  authors:
                    Belt.Array.mapWithIndex(
                      nextFieldsStatuses^.authors, (idx_, item) =>
                      if (idx_ == index') {
                        {...item, name: status};
                      } else {
                        item;
                      }
                    ),
                }
              )
            ) {
            | Some(result) => nextFieldsStatuses := result
            | None => ()
            }
          );
          Update({
            ...state,
            input: nextInput,
            fieldsStatuses: nextFieldsStatuses^,
            collectionsStatuses: {
              authors: Some(validators.authors.collection(nextInput)),
            },
          });
        | Submit =>
          switch (state.formStatus) {
          | Submitting(_) => NoUpdate
          | Editing
          | Submitted
          | SubmissionFailed(_) =>
            switch (
              state.input
              ->validateForm(
                  ~validators,
                  ~fieldsStatuses=state.fieldsStatuses,
                )
            ) {
            | Valid({output, fieldsStatuses, collectionsStatuses}) =>
              UpdateWithSideEffects(
                {
                  ...state,
                  fieldsStatuses,
                  collectionsStatuses,
                  formStatus:
                    Submitting(
                      switch (state.formStatus) {
                      | SubmissionFailed(error) => Some(error)
                      | Editing
                      | Submitted
                      | Submitting(_) => None
                      },
                    ),
                  submissionStatus: AttemptedToSubmit,
                },
                ({dispatch}) =>
                  output->onSubmit({
                    notifyOnSuccess: input =>
                      SetSubmittedStatus(input)->dispatch,
                    notifyOnFailure: error =>
                      SetSubmissionFailedStatus(error)->dispatch,
                    reset: () => Reset->dispatch,
                    dismissSubmissionResult: () =>
                      DismissSubmissionResult->dispatch,
                  }),
              )
            | Invalid({fieldsStatuses, collectionsStatuses}) =>
              Update({
                ...state,
                fieldsStatuses,
                collectionsStatuses,
                formStatus: Editing,
                submissionStatus: AttemptedToSubmit,
              })
            }
          }
        | SetSubmittedStatus(input) =>
          switch (input) {
          | Some(input) =>
            Update({
              ...state,
              input,
              formStatus: Submitted,
              fieldsStatuses: input->initialFieldsStatuses,
            })
          | None =>
            Update({
              ...state,
              formStatus: Submitted,
              fieldsStatuses: state.input->initialFieldsStatuses,
            })
          }
        | SetSubmissionFailedStatus(error) =>
          Update({...state, formStatus: SubmissionFailed(error)})
        | MapSubmissionError(map) =>
          switch (state.formStatus) {
          | Submitting(Some(error)) =>
            Update({...state, formStatus: Submitting(Some(error->map))})
          | SubmissionFailed(error) =>
            Update({...state, formStatus: SubmissionFailed(error->map)})
          | Editing
          | Submitting(None)
          | Submitted => NoUpdate
          }
        | DismissSubmissionError =>
          switch (state.formStatus) {
          | Editing
          | Submitting(_)
          | Submitted => NoUpdate
          | SubmissionFailed(_) => Update({...state, formStatus: Editing})
          }
        | DismissSubmissionResult =>
          switch (state.formStatus) {
          | Editing
          | Submitting(_) => NoUpdate
          | Submitted
          | SubmissionFailed(_) => Update({...state, formStatus: Editing})
          }
        | Reset => Update(initialInput->initialState)
        }
      );
    {
      updateAuthorName: (input, ~at as index) =>
        UpdateAuthorNameField(input, index)->dispatch,
      updateAuthorAge: (input, ~at as index) =>
        UpdateAuthorAgeField(input, index)->dispatch,
      updateTitle: input => UpdateTitleField(input)->dispatch,
      blurAuthorName: (~at as index) => BlurAuthorNameField(index)->dispatch,
      blurAuthorAge: (~at as index) => BlurAuthorAgeField(index)->dispatch,
      blurTitle: () => BlurTitleField->dispatch,
      authorNameResult: (~at as index) =>
        exposeFieldResult(
          Belt.Array.getUnsafe(state.fieldsStatuses.authors, index).name,
        ),
      authorAgeResult: (~at as index) =>
        exposeFieldResult(
          Belt.Array.getUnsafe(state.fieldsStatuses.authors, index).age,
        ),
      titleResult: exposeFieldResult(state.fieldsStatuses.title),
      addAuthor: entry => AddAuthorEntry(entry)->dispatch,
      removeAuthor: (~at as index) => RemoveAuthorEntry(index)->dispatch,
      authorsResult: state.collectionsStatuses.authors,
      input: state.input,
      status: state.formStatus,
      dirty: () =>
        switch (state.fieldsStatuses) {
        | {title: Pristine, authors}
            when
              Belt.Array.every(authors, item =>
                switch (item) {
                | {age: Pristine, name: Pristine} => true
                | _ => false
                }
              ) =>
          false
        | _ => true
        },
      valid: () =>
        switch (
          state.input
          ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
        ) {
        | Valid(_) => true
        | Invalid(_) => false
        },
      submitting:
        switch (state.formStatus) {
        | Submitting(_) => true
        | Editing
        | Submitted
        | SubmissionFailed(_) => false
        },
      submit: () => Submit->dispatch,
      mapSubmissionError: map => MapSubmissionError(map)->dispatch,
      dismissSubmissionError: () => DismissSubmissionError->dispatch,
      dismissSubmissionResult: () => DismissSubmissionResult->dispatch,
      reset: () => Reset->dispatch,
    };
  };
};

let initialInput: BlogPostForm.input = {title: "", authors: [||]};

[@react.component]
let make = () => {
  let form =
    BlogPostForm.useForm(
      ~initialInput,
      ~onSubmit=(state, form) => {
        Js.log2("Submitted with:", state);
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
            form.updateTitle({
              ...form.input,
              title: event->ReactEvent.Form.target##value,
            })
          }
        />
        {switch (form.titleResult) {
         | Some(Error(message)) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Ok(_)) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | None => React.null
         }}
      </div>
      <h3 className="push-lg"> "Authors"->React.string </h3>
      {switch (form.authorsResult) {
       | Some(Error(message)) =>
         <div className={Cn.make(["form-message", "failure"])}>
           message->React.string
         </div>
       | Some(Ok ())
       | None => React.null
       }}
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
                   {
                     ...form.input,
                     authors:
                       form.input.authors
                       ->Array.mapWithIndex((idx, author) =>
                           if (idx == index) {
                             {
                               ...author,
                               name: event->ReactEvent.Form.target##value,
                             };
                           } else {
                             author;
                           }
                         ),
                   },
                   ~at=index,
                 )
               }
             />
             <div onClick={_ => form.removeAuthor(~at=index)}>
               "Remove"->React.string
             </div>
             {switch (form.authorNameResult(~at=index)) {
              | Some(Error(message)) =>
                <div className={Cn.make(["form-message", "failure"])}>
                  message->React.string
                </div>
              | Some(Ok(_)) =>
                <div className={Cn.make(["form-message", "success"])}>
                  {j|✓|j}->React.string
                </div>
              | None => React.null
              }}
           </div>;
         })
       ->React.array}
      <button
        type_="button" onClick={_ => form.addAuthor({name: "", age: 30})}>
        "Add author"->React.string
      </button>
      <div className="form-row">
        <button className="push-lg" disabled={form.submitting}>
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
