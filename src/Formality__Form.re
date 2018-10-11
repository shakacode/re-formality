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
        Validation.result(Form.message),
        FieldComparator.identity,
      ),
    emittedFields: Set.t(Form.field, FieldComparator.identity),
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
    result: Form.field => option(Validation.result(Form.message)),
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    dismissSubmissionResult: unit => unit,
  };

  let getInitialState = data => {
    data,
    status: FormStatus.Editing,
    validators:
      ref(
        Form.validators->List.reduce(
          Map.make(~id=(module FieldComparator)), (fields, validator) =>
          fields->Map.set(validator.field, validator)
        ),
      ),
    results: Map.make(~id=(module FieldComparator)),
    emittedFields: Set.make(~id=(module FieldComparator)),
    submittedOnce: false,
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
             Validation.result(Form.message),
             FieldComparator.identity,
           ),
        ~emittedFields: Set.t(Form.field, FieldComparator.identity),
        dependents: option(list(Form.field)),
      ) =>
    switch (dependents) {
    | None => (results, emittedFields)
    | Some(dependents) =>
      dependents->List.reduce(
        (results, emittedFields),
        ((results, emittedFields), field) => {
          let validator = (validators^)->Map.getExn(field);
          let emitted = emittedFields->Set.has(field);
          if (emitted) {
            let result = data->(validator.validate);
            (
              results->Map.set(field, result),
              emittedFields->Set.add(field),
            );
          } else {
            (results, emittedFields);
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
          let emitted = state.emittedFields->Set.has(field);
          switch (validator.strategy, emitted, state.submittedOnce) {
          | (_, true, _)
          | (_, _, true)
          | (Strategy.OnFirstChange, false, false) =>
            let result = data->(validator.validate);
            let (results, emittedFields) =
              validator.dependents
              ->validateDependents(
                  ~data,
                  ~validators=state.validators,
                  ~results=state.results,
                  ~emittedFields=state.emittedFields,
                );
            React.Update({
              ...state,
              data,
              results: results->Map.set(field, result),
              emittedFields: emittedFields->Set.add(field),
            });

          | (
              Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur,
              false,
              false,
            ) =>
            let result = data->(validator.validate);
            let (results, emittedFields) =
              validator.dependents
              ->validateDependents(
                  ~data,
                  ~validators=state.validators,
                  ~results=state.results,
                  ~emittedFields=state.emittedFields,
                );
            switch (result) {
            | Ok(Valid | NoValue) =>
              React.Update({
                ...state,
                data,
                results: results->Map.set(field, result),
                emittedFields: emittedFields->Set.add(field),
              })
            | Error(_) =>
              React.Update({...state, data, results, emittedFields})
            };

          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false, false) =>
            React.Update({...state, data})
          };
        };

      | Blur(field) =>
        let validator = (state.validators^)->Map.get(field);
        let emitted = state.emittedFields->Set.has(field);
        switch (validator, emitted) {
        | (None, _)
        | (Some(_), true) => React.NoUpdate
        | (Some(validator), false) =>
          switch (validator.strategy) {
          | Strategy.OnFirstChange
          | Strategy.OnFirstSuccess
          | Strategy.OnSubmit => React.NoUpdate
          | Strategy.OnFirstBlur
          | Strategy.OnFirstSuccessOrFirstBlur =>
            let result = state.data->(validator.validate);
            React.Update({
              ...state,
              results: state.results->Map.set(field, result),
              emittedFields: state.emittedFields->Set.add(field),
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
                  let results = results->Map.set(field, result);
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
      }),
  };
};
