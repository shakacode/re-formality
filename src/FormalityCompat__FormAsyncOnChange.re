module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

let defaultDebounceInterval = 700;

module type Form = {
  type field;
  type state;
  type message;
  type submissionError;
  let debounceInterval: int;
  let validators: list(Validation.Async.validator(field, state, message));
};

module Make = (Form: Form) => {
  module FieldId =
    Id.MakeComparable({
      type t = Form.field;
      let cmp = Pervasives.compare;
    });

  type state = {
    input: Form.state,
    status: FormStatus.t(Form.submissionError),
    fields:
      Map.t(
        Form.field,
        Validation.Async.status(Form.message),
        FieldId.identity,
      ),
    validators:
      ref(Map.t(Form.field, debouncedAsyncValidator, FieldId.identity)),
    submittedOnce: bool,
  }
  and action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
    | InvokeDebouncedAsyncValidation(
        Form.field,
        Form.state,
        (
          (
            Form.field,
            Form.state,
            ReasonReact.self(state, ReasonReact.noRetainedProps, action),
          )
        ) =>
        unit,
      )
    | TriggerAsyncValidation(
        Form.field,
        Form.state,
        Validation.Async.validate(Form.state, Form.message),
      )
    | ApplyAsyncResult(
        Form.field,
        Form.state,
        Validation.Result.result(Form.message),
      )
    | Submit
    | SetSubmittedStatus(option(Form.state))
    | SetSubmissionFailedStatus(Form.submissionError)
    | MapSubmissionError(Form.submissionError => Form.submissionError)
    | DismissSubmissionError
    | DismissSubmissionResult
    | Reset
  and debouncedAsyncValidator = {
    field: Form.field,
    strategy: Strategy.t,
    dependents: option(list(Form.field)),
    validate: Validation.validate(Form.state, Form.message),
    validateAsync:
      option(
        (
          (
            (
              Form.field,
              Form.state,
              ReasonReact.self(state, ReasonReact.noRetainedProps, action),
            )
          ) =>
          unit,
          Validation.Async.equalityChecker(Form.state),
        ),
      ),
  };

  type interface = {
    state: Form.state,
    status: FormStatus.t(Form.submissionError),
    result: Form.field => option(Validation.Result.result(Form.message)),
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
  };

  let debounce = (~wait, fn) => {
    let fn = ((field, data, {ReasonReact.send})) =>
      TriggerAsyncValidation(field, data, fn)->send;
    fn->(Debouncer.make(~wait));
  };

  let getInitialState = input => {
    input,
    status: FormStatus.Editing,
    fields:
      Form.validators->List.reduce(
        Map.make(~id=(module FieldId)), (fields, validator) =>
        fields->Map.set(validator.field, Validation.Async.Pristine)
      ),
    validators:
      ref(
        Form.validators->List.reduce(
          Map.make(~id=(module FieldId)), (validators, validator) =>
          validators->Map.set(
            validator.field,
            {
              field: validator.field,
              strategy: validator.strategy,
              dependents: validator.dependents,
              validate: validator.validate,
              validateAsync:
                validator.validateAsync
                ->Option.map(((fn, eq)) =>
                    (fn->debounce(~wait=Form.debounceInterval), eq)
                  ),
            },
          )
        ),
      ),
    submittedOnce: false,
  };

  let component = ReasonReact.reducerComponent("Formality.Form");
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit:
           (
             Form.state,
             Validation.submissionCallbacks(Form.state, Form.submissionError)
           ) =>
           unit,
        children,
      ) => {
    ...component,
    initialState: () => initialState->getInitialState,
    reducer: (action, state) =>
      switch (action) {
      | Change(field, input) =>
        let validator = (state.validators^)->Map.get(field);
        switch (validator) {
        | None =>
          ReasonReact.Update({
            ...state,
            input,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | Some(validator) =>
          let status = state.fields->Map.get(field);
          let result = input->(validator.validate);
          let fields =
            switch (validator.dependents) {
            | None => state.fields
            | Some(dependents) =>
              dependents->List.reduce(
                state.fields,
                (fields, field) => {
                  let status = fields->Map.get(field);
                  switch (status) {
                  | None
                  | Some(Pristine)
                  | Some(Validating)
                  | Some(Dirty(_, Hidden)) => fields
                  | Some(Dirty(_, Shown)) =>
                    let validator = (state.validators^)->Map.getExn(field);
                    fields->Map.set(
                      field,
                      Dirty(input->(validator.validate), Shown),
                    );
                  };
                },
              )
            };
          switch (validator.strategy, status, state.submittedOnce) {
          | (_, Some(Dirty(_, Shown)), _)
          | (_, _, true)
          | (OnFirstChange, _, false) =>
            switch (result, validator.validateAsync) {
            | (_, None) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some((validateAsync, _))) =>
              ReasonReact.UpdateWithSideEffects(
                {
                  ...state,
                  input,
                  fields: fields->Map.set(field, Validating),
                },
                ({send}) =>
                  InvokeDebouncedAsyncValidation(field, input, validateAsync)
                  ->send,
              )
            | (Ok(NoValue) | Error(_), Some(_)) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            }

          | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, false) =>
            switch (result, validator.validateAsync) {
            | (Ok(Valid | NoValue), None) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), None) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })

            | (Ok(Valid), Some((validateAsync, _))) =>
              ReasonReact.UpdateWithSideEffects(
                {
                  ...state,
                  input,
                  fields: fields->Map.set(field, Validating),
                },
                ({send}) =>
                  InvokeDebouncedAsyncValidation(field, input, validateAsync)
                  ->send,
              )
            | (Ok(NoValue), Some(_)) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), Some(_)) =>
              ReasonReact.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            }

          | (OnFirstBlur | OnSubmit, _, false) =>
            ReasonReact.Update({
              ...state,
              input,
              fields: fields->Map.set(field, Dirty(result, Hidden)),
            })
          };
        };

      | Blur(field) =>
        let status = state.fields->Map.get(field);
        let validator = (state.validators^)->Map.get(field);
        switch (status, validator) {
        | (Some(Validating), _)
        | (Some(Dirty(_, Shown)), Some(_) | None)
        | (Some(Dirty(_, Hidden)), None) => ReasonReact.NoUpdate
        | (Some(Pristine) | None, None) =>
          ReasonReact.Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | (Some(Pristine | Dirty(_, Hidden)) | None, Some(validator)) =>
          let result = state.input->(validator.validate);
          switch (validator.strategy) {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            ReasonReact.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            switch (result, validator.validateAsync) {
            | (_, None) =>
              ReasonReact.Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some((validateAsync, _))) =>
              ReasonReact.UpdateWithSideEffects(
                {...state, fields: state.fields->Map.set(field, Validating)},
                ({send}) =>
                  InvokeDebouncedAsyncValidation(
                    field,
                    state.input,
                    validateAsync,
                  )
                  ->send,
              )
            | (Ok(NoValue) | Error(_), Some(_)) =>
              ReasonReact.Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            }
          };
        };

      | InvokeDebouncedAsyncValidation(field, input, validateAsync) =>
        ReasonReact.SideEffects(self => (field, input, self)->validateAsync)

      | TriggerAsyncValidation(field, input, validateAsync) =>
        ReasonReact.SideEffects(
          ({send}) =>
            Js.Promise.(
              input
              ->validateAsync
              ->then_(
                  result => {
                    ApplyAsyncResult(field, input, result)->send;
                    resolve();
                  },
                  _,
                )
              ->ignore
            ),
        )

      | ApplyAsyncResult(field, input, result) =>
        let validator = (state.validators^)->Map.getExn(field);
        let eq = validator.validateAsync->Option.getExn->snd;
        if (input->eq(state.input)) {
          ReasonReact.Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(result, Shown)),
          });
        } else {
          ReasonReact.NoUpdate;
        };

      | Submit =>
        switch (state.status) {
        | Submitting(_) => ReasonReact.NoUpdate
        | Editing
        | Submitted
        | SubmissionFailed(_) =>
          let (valid, fields, validating) =
            (state.validators^)
            ->Map.reduce(
                (true, state.fields, false),
                ((valid, fields, validating), field, validator) => {
                  let status = fields->Map.get(field);
                  switch (status) {
                  | _ when validating => (valid, fields, true)
                  | Some(Validating) => (valid, fields, true)
                  | Some(_)
                  | None =>
                    let currentResultIsInvalid =
                      switch (status) {
                      | Some(Dirty(Error(_), _)) => true
                      | Some(Dirty(Ok(_), _) | Pristine | Validating)
                      | None => false
                      };
                    let result = state.input->(validator.validate);
                    let fields =
                      switch (
                        currentResultIsInvalid,
                        result,
                        validator.validateAsync,
                      ) {
                      | (true, Ok(Valid), Some(_)) => fields
                      | (_, _, _) =>
                        fields->Map.set(field, Dirty(result, Shown))
                      };
                    switch (valid, fields->Map.get(field)) {
                    | (false, _)
                    | (true, Some(Dirty(Error(_), _))) => (
                        false,
                        fields,
                        false,
                      )
                    | (
                        true,
                        Some(
                          Dirty(Ok(Valid | NoValue), _) | Pristine |
                          Validating,
                        ),
                      )
                    | (true, None) => (true, fields, false)
                    };
                  };
                },
              );
          if (validating) {
            ReasonReact.NoUpdate;
          } else if (valid) {
            ReasonReact.UpdateWithSideEffects(
              {
                ...state,
                fields,
                status:
                  FormStatus.Submitting(
                    switch (state.status) {
                    | SubmissionFailed(error) => Some(error)
                    | Editing
                    | Submitted
                    | Submitting(_) => None
                    },
                  ),
                submittedOnce: true,
              },
              ({state, send}) =>
                state.input
                ->onSubmit({
                    notifyOnSuccess: data => data->SetSubmittedStatus->send,
                    notifyOnFailure: error =>
                      SetSubmissionFailedStatus(error)->send,
                    reset: () => Reset->send,
                    dismissSubmissionResult: () =>
                      DismissSubmissionResult->send,
                  }),
            );
          } else {
            ReasonReact.Update({
              ...state,
              fields,
              status: FormStatus.Editing,
              submittedOnce: true,
            });
          };
        }

      | SetSubmittedStatus(data) =>
        switch (data) {
        | Some(data) =>
          ReasonReact.Update({
            ...state,
            input: data,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Async.Pristine),
          })
        | None =>
          ReasonReact.Update({
            ...state,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Async.Pristine),
          })
        }

      | SetSubmissionFailedStatus(error) =>
        ReasonReact.Update({
          ...state,
          status: FormStatus.SubmissionFailed(error),
        })

      | MapSubmissionError(map) =>
        switch (state.status) {
        | Submitting(Some(error)) =>
          ReasonReact.Update({
            ...state,
            status: Submitting(Some(error->map)),
          })
        | SubmissionFailed(error) =>
          ReasonReact.Update({
            ...state,
            status: SubmissionFailed(error->map),
          })
        | Editing
        | Submitting(None)
        | Submitted => ReasonReact.NoUpdate
        }

      | DismissSubmissionError =>
        switch (state.status) {
        | Editing
        | Submitting(_)
        | Submitted => ReasonReact.NoUpdate
        | SubmissionFailed(_) =>
          ReasonReact.Update({...state, status: FormStatus.Editing})
        }

      | DismissSubmissionResult =>
        switch (state.status) {
        | Editing
        | Submitting(_) => ReasonReact.NoUpdate
        | Submitted
        | SubmissionFailed(_) =>
          ReasonReact.Update({...state, status: FormStatus.Editing})
        }

      | Reset => ReasonReact.Update(initialState->getInitialState)
      },

    render: ({state, send}) =>
      children({
        state: state.input,
        status: state.status,
        result: field =>
          switch (state.fields->Map.get(field)) {
          | None
          | Some(Pristine)
          | Some(Validating)
          | Some(Dirty(_, Hidden)) => None
          | Some(Dirty(result, Shown)) => Some(result)
          },
        dirty: () =>
          state.fields
          ->Map.some((_, status) =>
              switch (status) {
              | Dirty(_)
              | Validating => true
              | Pristine => false
              }
            ),
        validating: field =>
          switch (state.fields->Map.get(field)) {
          | Some(Validating) => true
          | None
          | Some(Pristine)
          | Some(Dirty(_)) => false
          },
        submitting:
          switch (state.status) {
          | Submitting(_) => true
          | Editing
          | Submitted
          | SubmissionFailed(_) => false
          },
        change: (field, state) => Change(field, state)->send,
        blur: field => Blur(field)->send,
        submit: () => Submit->send,
        mapSubmissionError: map => MapSubmissionError(map)->send,
        dismissSubmissionError: () => DismissSubmissionError->send,
        dismissSubmissionResult: () => DismissSubmissionResult->send,
        reset: () => Reset->send,
      }),
  };
};
