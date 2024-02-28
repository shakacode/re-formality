module Validation = FormalityCompat__Validation
module Strategy = FormalityCompat__Strategy
module FormStatus = FormalityCompat__FormStatus
module ReactUpdate = FormalityCompat__ReactUpdate

module type Form = {
  type field
  type state
  type message
  type submissionError
  let validators: list<Validation.Async.validator<field, state, message>>
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
    fields: Map.t<Form.field, Validation.Async.status<Form.message>, FieldId.identity>,
    validators: ref<
      Map.t<
        Form.field,
        Validation.Async.validator<Form.field, Form.state, Form.message>,
        FieldId.identity,
      >,
    >,
    submittedOnce: bool,
  }

  type action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
    | ApplyAsyncResult(Form.field, Form.state, Validation.Result.result<Form.message>)
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
    validating: Form.field => bool,
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
      fields->Map.set(validator.field, Validation.Async.Pristine)
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
    ~onSubmit: (Form.state, Validation.submissionCallbacks<Form.state, 'submissionError>) => unit,
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
              | Some(Validating)
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
            switch (result, validator.validateAsync) {
            | (_, None) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some(_)) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            | (Ok(NoValue) | Error(_), Some(_)) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            }

          | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, false) =>
            switch (result, validator.validateAsync) {
            | (Ok(Valid | NoValue), None) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), None) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })

            | (Ok(Valid), Some(_)) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            | (Ok(NoValue), Some(_)) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), Some(_)) =>
              Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            }

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
        | (Some(Validating), _)
        | (Some(Dirty(_, Shown)), Some(_) | None)
        | (Some(Dirty(_, Hidden)), None) =>
          NoUpdate
        | (Some(Pristine) | None, None) =>
          Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })

        | (Some(Pristine | Dirty(_, Hidden)) | None, Some(validator)) =>
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
            switch (result, validator.validateAsync) {
            | (_, None) =>
              Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some((validateAsync, _))) =>
              UpdateWithSideEffects(
                {
                  ...state,
                  fields: state.fields->Map.set(field, Validating),
                },
                ({dispatch}) => {
                  open Js.Promise
                  state.input
                  ->validateAsync
                  ->then_(result => {
                    ApplyAsyncResult(field, state.input, result)->dispatch
                    resolve()
                  }, _)
                  ->ignore
                },
              )
            | (Ok(NoValue) | Error(_), Some((_, _))) =>
              Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            }
          }
        }

      | ApplyAsyncResult(field, input, result) =>
        let validator = state.validators.contents->Map.getExn(field)
        let eq = validator.validateAsync->Option.getExn->snd
        if input->eq(state.input) {
          Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(result, Shown)),
          })
        } else {
          NoUpdate
        }

      | Submit =>
        switch state.status {
        | Submitting(_) => NoUpdate
        | Editing
        | Submitted
        | SubmissionFailed(_) =>
          let (valid, fields, validating) = state.validators.contents->Map.reduce(
            (true, state.fields, false),
            ((valid, fields, validating), field, validator) => {
              let status = fields->Map.get(field)
              switch status {
              | _ if validating => (valid, fields, true)
              | Some(Validating) => (valid, fields, true)
              | Some(_)
              | None =>
                let currentResultIsInvalid = switch status {
                | Some(Dirty(Error(_), _)) => true
                | Some(Dirty(Ok(_), _) | Pristine | Validating)
                | None => false
                }
                let result = state.input->validator.validate
                let fields = switch (currentResultIsInvalid, result, validator.validateAsync) {
                | (true, Ok(Valid), Some(_)) => fields
                | (_, _, _) => fields->Map.set(field, Dirty(result, Shown))
                }
                switch (valid, fields->Map.get(field)) {
                | (false, _)
                | (true, Some(Dirty(Error(_), _))) => (false, fields, false)
                | (
                  true,
                  Some(
                    Dirty(Ok(Valid | NoValue), _)
                    | Pristine
                    | Validating,
                  ),
                )
                | (true, None) => (true, fields, false)
                }
              }
            },
          )
          if validating {
            NoUpdate
          } else if valid {
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
                  notifyOnSuccess: data => data->SetSubmittedStatus->dispatch,
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
            fields: state.fields->Map.map(_ => Validation.Async.Pristine),
          })
        | None =>
          Update({
            ...state,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Async.Pristine),
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
        | Some(Validating)
        | Some(Dirty(_, Hidden)) =>
          None
        | Some(Dirty(result, Shown)) => Some(result)
        },
      dirty: () =>
        state.fields->Map.some((_, status) =>
          switch status {
          | Dirty(_)
          | Validating => true
          | Pristine => false
          }
        ),
      validating: field =>
        switch state.fields->Map.get(field) {
        | Some(Validating) => true
        | None
        | Some(Pristine)
        | Some(Dirty(_)) => false
        },
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
