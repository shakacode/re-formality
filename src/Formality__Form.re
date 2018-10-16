module React = ReasonReact;

module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module type Form = {
  type field;
  type state;
  type message;
  let validators: list(Validation.validator(field, state, message));
};

module Make = (Form: Form) => {
  module FieldComparator =
    Id.MakeComparable({
      type t = Form.field;
      let cmp = Pervasives.compare;
    });

  type state = {
    data: Form.state,
    status: FormStatus.t(Form.field, Form.message),
    validators:
      ref(
        Map.t(
          Form.field,
          Validation.validator(Form.field, Form.state, Form.message),
          FieldComparator.identity,
        ),
      ),
    results:
      Map.t(
        Form.field,
        Validation.status(Form.message),
        FieldComparator.identity,
      ),
    submittedOnce: bool,
  };

  type action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
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
    result: Form.field => option(Validation.status(Form.message)),
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    dismissSubmissionResult: unit => unit,
    isValid: unit => bool,
    isDirty: unit => bool,
  };

  let getInitialState = data => {
    let validators =
      Form.validators->List.reduce(
        Map.make(~id=(module FieldComparator)), (fields, validator) =>
        fields->Map.set(validator.field, validator)
      );
    let results =
      Form.validators->List.reduce(
        Map.make(~id=(module FieldComparator)), (fields, validator) =>
        fields->(Map.set(validator.field, Validation.Pristine))
      );

    {
      data,
      status: FormStatus.Editing,
      validators: ref(validators),
      results,
      submittedOnce: false,
    };
  };

  let validateDependents =
      (
        ~data: Form.state,
        ~validators:
           ref(
             Map.t(
               Form.field,
               Validation.validator(Form.field, Form.state, Form.message),
               FieldComparator.identity,
             ),
           ),
        ~results:
           Map.t(
             Form.field,
             Validation.status(Form.message),
             FieldComparator.identity,
           ),
        dependents: option(list(Form.field)),
      ) =>
    switch (dependents) {
    | None => results
    | Some(dependents) =>
      dependents->List.reduce(
        results,
        (results, field) => {
          let validator = (validators^)->Map.getExn(field);
          let emitted =
            results
            ->(Map.get(field))
            ->(
                Option.map(
                  fun
                  | Dirty(_) => true
                  | Pristine => false,
                )
              )
            ->(Option.getWithDefault(false));
          if (emitted) {
            let result = data->(validator.validate);
            results->Map.set(field, Dirty(result));
          } else {
            results;
          };
        },
      )
    };

  let component = React.reducerComponent("FormalityForm");
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
      | Change(field, data) =>
        let validator = (state.validators^)->Map.get(field);
        switch (validator) {
        | None => React.Update({...state, data})
        | Some(validator) =>
          switch (validator.strategy, state.submittedOnce) {
          | (_, true)
          | (Strategy.OnFirstChange, false) =>
            let result = data->(validator.validate);
            let results =
              validator.dependents
              ->validateDependents(
                  ~data,
                  ~validators=state.validators,
                  ~results=state.results,
                );
            React.Update({
              ...state,
              data,
              results: results->Map.set(field, Dirty(result)),
            });

          | (
              Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur,
              false,
            ) =>
            let result = data->(validator.validate);
            let results =
              validator.dependents
              ->validateDependents(
                  ~data,
                  ~validators=state.validators,
                  ~results=state.results,
                );
            React.Update({
              ...state,
              data,
              results: results->Map.set(field, Dirty(result)),
            });

          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false) =>
            React.Update({...state, data})
          }
        };

      | Blur(field) =>
        let validator = (state.validators^)->Map.get(field);
        switch (validator) {
        | None => React.NoUpdate
        | Some(validator) =>
          switch (validator.strategy) {
          | Strategy.OnFirstChange
          | Strategy.OnFirstSuccess
          | Strategy.OnSubmit => React.NoUpdate
          | Strategy.OnFirstBlur
          | Strategy.OnFirstSuccessOrFirstBlur =>
            let result = state.data->(validator.validate);
            React.Update({
              ...state,
              results: state.results->Map.set(field, Dirty(result)),
            });
          }
        };

      | Submit =>
        switch (state.status) {
        | FormStatus.Submitting => React.NoUpdate
        | _ =>
          let (valid, results) =
            (state.validators^)
            ->Map.reduce(
                (true, state.results),
                ((valid, results), field, validator) => {
                  let result = state.data->(validator.validate);
                  let results = results->Map.set(field, Dirty(result));
                  switch (valid, result) {
                  | (false, _)
                  | (true, Error(_)) => (false, results)
                  | (true, Ok(Valid | NoValue)) => (true, results)
                  };
                },
              );
          if (valid) {
            React.UpdateWithSideEffects(
              {
                ...state,
                results,
                status: FormStatus.Submitting,
                submittedOnce: true,
              },
              (
                ({state, send}) =>
                  state.data
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
              results,
              status: FormStatus.Editing,
              submittedOnce: true,
            });
          };
        }

      | SetSubmittedStatus(data) =>
        switch (data) {
        | Some(data) =>
          React.Update({...state, data, status: FormStatus.Submitted})
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
        | FormStatus.Editing
        | FormStatus.Submitting => React.NoUpdate
        | FormStatus.Submitted
        | FormStatus.SubmissionFailed(_, _) =>
          React.Update({...state, status: FormStatus.Editing})
        }
      | Reset => React.Update(initialState->getInitialState)
      },

    render: ({state, send}) =>
      children({
        state: state.data,
        status: state.status,
        result: field => state.results->Map.get(field),
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
        isValid: () =>
          (state.validators^)
          ->Map.some((_key, validator) =>
              state.data->(validator.validate)->Belt.Result.isError
            )
          != true,
        isDirty: () =>
          state.results
          ->(
              Map.some((_key, value) =>
                switch (value) {
                | Pristine => false
                | Dirty(_) => true
                }
              )
            ),
      }),
  };
};
