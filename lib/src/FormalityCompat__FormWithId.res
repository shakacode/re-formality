module Validation = FormalityCompat__Validation
module Strategy = FormalityCompat__Strategy
module FormStatus = FormalityCompat__FormStatus
module ReactUpdate = FormalityCompat__ReactUpdate

module type Form = {
  type field
  type state
  type message
  type submissionError
  let validators: list<Validation.validator<field, state, message>>
  module FieldId: {
    type identity
    type t = field
    let cmp: Belt.Id.cmp<t, identity>
  }
}
module Make = (Form: Form) => {
  module FieldId = Form.FieldId

  type state = {
    input: Form.state,
    status: FormStatus.t<Form.submissionError>,
    fields: Map.t<Form.field, Validation.status<Form.message>, FieldId.identity>,
    validators: ref<
      Map.t<
        Form.field,
        Validation.validator<Form.field, Form.state, Form.message>,
        FieldId.identity,
      >,
    >,
    submittedOnce: bool,
  }

  type action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
    | Submit
    | SetSubmittedStatus(option<Form.state>)
    | SetSubmissionFailedStatus(Form.submissionError)
    | MapSubmissionError(Form.submissionError => Form.submissionError)
    | DismissSubmissionError
    | DismissSubmissionResult
    | Reset

  type interface = {
    state: Form.state,
    status: FormStatus.t<Form.submissionError>,
    result: Form.field => option<Validation.Result.result<Form.message>>,
    dirty: unit => bool,
    valid: unit => bool,
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    mapSubmissionError: (Form.submissionError => Form.submissionError) => unit,
    dismissSubmissionError: unit => unit,
    dismissSubmissionResult: unit => unit,
    reset: unit => unit,
  }

  let getInitialState = input => {
    input,
    status: FormStatus.Editing,
    fields: Form.validators->List.reduce(Map.make(~id=module(FieldId)), (fields, validator) =>
      fields->Map.set(validator.field, Validation.Pristine)
    ),
    validators: ref(
      Form.validators->List.reduce(Map.make(~id=module(FieldId)), (fields, validator) =>
        fields->Map.set(validator.field, validator)
      ),
    ),
    submittedOnce: false,
  }

  let useForm = (
    ~initialState: Form.state,
    ~onSubmit: (
      Form.state,
      Validation.submissionCallbacks<Form.state, Form.submissionError>,
    ) => unit,
  ) => {
    let memoizedInitialState = React.useMemo1(() => initialState->getInitialState, [initialState])

    let (state, dispatch) = ReactUpdate.useReducer(memoizedInitialState, (state, action) =>
      switch action {
      | Change(field, input) =>
        let validator = state.validators.contents->Map.get(field)
        switch validator {
        | None =>
          Update({
            ...state,
            input,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | Some(validator) =>
          let status = state.fields->Map.get(field)
          let result = input->validator.validate
          let fields = switch validator.dependents {
          | None => state.fields
          | Some(dependents) =>
            dependents->List.reduce(state.fields, (fields, field) => {
              let status = fields->Map.get(field)
              switch status {
              | None
              | Some(Pristine)
              | Some(Dirty(_, Hidden)) => fields
              | Some(Dirty(_, Shown)) =>
                let validator = state.validators.contents->Map.getExn(field)
                fields->Map.set(field, Dirty(input->validator.validate, Shown))
              }
            })
          }
          switch (validator.strategy, status, state.submittedOnce) {
          | (_, Some(Dirty(_, Shown)), _)
          | (_, _, true)
          | (OnFirstChange, _, false) =>
            Update({
              ...state,
              input,
              fields: fields->Map.set(field, Dirty(result, Shown)),
            })
          | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, false) =>
            Update({
              ...state,
              input,
              fields: switch result {
              | Ok(Valid | NoValue) => fields->Map.set(field, Dirty(result, Shown))
              | Error(_) => fields->Map.set(field, Dirty(result, Hidden))
              },
            })
          | (OnFirstBlur | OnSubmit, _, false) =>
            Update({
              ...state,
              input,
              fields: fields->Map.set(field, Dirty(result, Hidden)),
            })
          }
        }

      | Blur(field) =>
        let status = state.fields->Map.get(field)
        let validator = state.validators.contents->Map.get(field)
        switch (status, validator) {
        | (Some(Dirty(_, Shown)), Some(_) | None)
        | (Some(Dirty(_, Hidden)), None) =>
          NoUpdate
        | (Some(Pristine) | None, None) =>
          Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | (Some(Pristine) | None, Some(validator)) =>
          let result = state.input->validator.validate
          switch validator.strategy {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Shown)),
            })
          }
        | (Some(Dirty(_, Hidden)), Some(validator)) =>
          let result = state.input->validator.validate
          switch validator.strategy {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Shown)),
            })
          }
        }

      | Submit =>
        switch state.status {
        | Submitting(_) => NoUpdate
        | Editing
        | Submitted
        | SubmissionFailed(_) =>
          let (valid, fields) = state.validators.contents->Map.reduce((true, state.fields), (
            (valid, fields),
            field,
            validator,
          ) => {
            let result = state.input->validator.validate
            let fields = fields->Map.set(field, Dirty(result, Shown))
            switch (valid, result) {
            | (false, _)
            | (true, Error(_)) => (false, fields)
            | (true, Ok(Valid | NoValue)) => (true, fields)
            }
          })
          if valid {
            UpdateWithSideEffects(
              {
                ...state,
                fields,
                status: FormStatus.Submitting(
                  switch state.status {
                  | SubmissionFailed(error) => Some(error)
                  | Editing
                  | Submitted
                  | Submitting(_) =>
                    None
                  },
                ),
                submittedOnce: true,
              },
              ({state, dispatch}) =>
                state.input->onSubmit({
                  notifyOnSuccess: state => SetSubmittedStatus(state)->dispatch,
                  notifyOnFailure: error => SetSubmissionFailedStatus(error)->dispatch,
                  reset: () => Reset->dispatch,
                  dismissSubmissionResult: () => DismissSubmissionResult->dispatch,
                }),
            )
          } else {
            Update({
              ...state,
              fields,
              status: FormStatus.Editing,
              submittedOnce: true,
            })
          }
        }

      | SetSubmittedStatus(data) =>
        switch data {
        | Some(data) =>
          Update({
            ...state,
            input: data,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Pristine),
          })
        | None =>
          Update({
            ...state,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Pristine),
          })
        }

      | SetSubmissionFailedStatus(error) =>
        Update({...state, status: FormStatus.SubmissionFailed(error)})

      | MapSubmissionError(map) =>
        switch state.status {
        | Submitting(Some(error)) => Update({...state, status: Submitting(Some(error->map))})
        | SubmissionFailed(error) => Update({...state, status: SubmissionFailed(error->map)})
        | Editing
        | Submitting(None)
        | Submitted =>
          NoUpdate
        }

      | DismissSubmissionError =>
        switch state.status {
        | Editing
        | Submitting(_)
        | Submitted =>
          NoUpdate
        | SubmissionFailed(_) => Update({...state, status: FormStatus.Editing})
        }

      | DismissSubmissionResult =>
        switch state.status {
        | Editing
        | Submitting(_) =>
          NoUpdate
        | Submitted
        | SubmissionFailed(_) =>
          Update({...state, status: FormStatus.Editing})
        }

      | Reset => Update(initialState->getInitialState)
      }
    )

    {
      state: state.input,
      status: state.status,
      result: field =>
        switch state.fields->Map.get(field) {
        | None
        | Some(Pristine)
        | Some(Dirty(_, Hidden)) =>
          None
        | Some(Dirty(result, Shown)) => Some(result)
        },
      dirty: () =>
        state.fields->Map.some((_, status) =>
          switch status {
          | Dirty(_) => true
          | Pristine => false
          }
        ),
      valid: () =>
        state.fields->Map.every((field, status) =>
          switch status {
          | Dirty(Ok(_), _) => true
          | Dirty(Error(_), _) => false
          | Pristine =>
            state.validators.contents
            ->Map.get(field)
            ->Option.map(validator =>
              switch state.input->validator.validate {
              | Ok(_) => true
              | Error(_) => false
              }
            )
            ->Option.getWithDefault(true)
          }
        ),
      submitting: switch state.status {
      | Submitting(_) => true
      | Editing
      | Submitted
      | SubmissionFailed(_) => false
      },
      change: (field, state) => Change(field, state)->dispatch,
      blur: field => Blur(field)->dispatch,
      submit: () => Submit->dispatch,
      mapSubmissionError: map => MapSubmissionError(map)->dispatch,
      dismissSubmissionError: () => DismissSubmissionError->dispatch,
      dismissSubmissionResult: () => DismissSubmissionResult->dispatch,
      reset: () => Reset->dispatch,
    }
  }
}
