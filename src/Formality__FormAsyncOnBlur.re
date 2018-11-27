module React = ReasonReact;

module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module type Form = {
  type field;
  type state;
  type message;
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
    status: FormStatus.t(Form.field, Form.message),
    fields:
      Map.t(
        Form.field,
        Validation.Async.status(Form.message),
        FieldId.identity,
      ),
    validators:
      ref(
        Map.t(
          Form.field,
          Validation.Async.validator(Form.field, Form.state, Form.message),
          FieldId.identity,
        ),
      ),
    submittedOnce: bool,
  };

  type action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
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
    | SetSubmissionFailedStatus(
        list((Form.field, Form.message)),
        option(Form.message),
      )
    | DismissSubmissionResult
    | Reset;

  type interface = {
    state: Form.state,
    status: FormStatus.t(Form.field, Form.message),
    result: Form.field => option(Validation.Result.result(Form.message)),
    dirty: unit => bool,
    validating: Form.field => bool,
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    dismissSubmissionResult: unit => unit,
    reset: unit => unit,
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
          Map.make(~id=(module FieldId)), (fields, validator) =>
          fields->Map.set(validator.field, validator)
        ),
      ),
    submittedOnce: false,
  };

  let component = React.reducerComponent("Formality.Form");
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit:
           (
             Form.state,
             Validation.submissionCallbacks(
               Form.field,
               Form.state,
               Form.message,
             )
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
          React.Update({
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
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some(_)) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            | (Ok(NoValue) | Error(_), Some(_)) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            }

          | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, false) =>
            switch (result, validator.validateAsync) {
            | (Ok(Valid | NoValue), None) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), None) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })

            | (Ok(Valid), Some(_)) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            | (Ok(NoValue), Some(_)) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Error(_), Some(_)) =>
              React.Update({
                ...state,
                input,
                fields: fields->Map.set(field, Dirty(result, Hidden)),
              })
            }

          | (OnFirstBlur | OnSubmit, _, false) =>
            React.Update({
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
        | (Some(Dirty(_, Hidden)), None) => React.NoUpdate
        | (Some(Pristine) | None, None) =>
          React.Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })

        | (Some(Pristine | Dirty(_, Hidden)) | None, Some(validator)) =>
          let result = state.input->(validator.validate);
          switch (validator.strategy) {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            React.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            let result = state.input->(validator.validate);
            switch (result, validator.validateAsync) {
            | (_, None) =>
              React.Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            | (Ok(Valid), Some((validateAsync, _))) =>
              React.UpdateWithSideEffects(
                {...state, fields: state.fields->Map.set(field, Validating)},
                (
                  ({send}) =>
                    TriggerAsyncValidation(field, state.input, validateAsync)
                    ->send
                ),
              )
            | (Ok(NoValue) | Error(_), Some((_, _))) =>
              React.Update({
                ...state,
                fields: state.fields->Map.set(field, Dirty(result, Shown)),
              })
            };
          };
        };

      | TriggerAsyncValidation(field, input, validateAsync) =>
        React.SideEffects(
          (
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
              )
          ),
        )

      | ApplyAsyncResult(field, input, result) =>
        let validator = (state.validators^)->Map.getExn(field);
        let eq = validator.validateAsync->Option.getExn->snd;
        if (input->eq(state.input)) {
          React.Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(result, Shown)),
          });
        } else {
          React.NoUpdate;
        };

      | Submit =>
        switch (state.status) {
        | Submitting => React.NoUpdate
        | Editing
        | Submitted
        | SubmissionFailed(_, _) =>
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
            React.NoUpdate;
          } else if (valid) {
            React.UpdateWithSideEffects(
              {
                ...state,
                fields,
                status: FormStatus.Submitting,
                submittedOnce: true,
              },
              (
                ({state, send}) =>
                  state.input
                  ->onSubmit({
                      notifyOnSuccess: data => data->SetSubmittedStatus->send,
                      notifyOnFailure: (fieldLevelErrors, serverMessage) =>
                        SetSubmissionFailedStatus(
                          fieldLevelErrors,
                          serverMessage,
                        )
                        ->send,
                      reset: () => Reset->send,
                    })
              ),
            );
          } else {
            React.Update({
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
          React.Update({...state, input: data, status: FormStatus.Submitted})
        | None => React.Update({...state, status: FormStatus.Submitted})
        }

      | SetSubmissionFailedStatus(fieldLevelErrors, serverMessage) =>
        React.Update({
          ...state,
          status:
            FormStatus.SubmissionFailed(fieldLevelErrors, serverMessage),
        })

      | DismissSubmissionResult =>
        switch (state.status) {
        | Editing
        | Submitting => React.NoUpdate
        | Submitted
        | SubmissionFailed(_, _) =>
          React.Update({...state, status: FormStatus.Editing})
        }

      | Reset => React.Update(initialState->getInitialState)
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
          | Submitting => true
          | Editing
          | Submitted
          | SubmissionFailed(_, _) => false
          },
        change: (field, state) => Change(field, state)->send,
        blur: field => Blur(field)->send,
        submit: () => Submit->send,
        dismissSubmissionResult: () => DismissSubmissionResult->send,
        reset: () => Reset->send,
      }),
  };
};
