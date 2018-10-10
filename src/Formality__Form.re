module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module type Config = {
  type field;
  type value;
  type state;
  type message;

  let get: (state, field) => value;
  let set: (state, (field, value)) => state;
  let valueEmpty: value => bool;
  let validators: list(Validation.validator(field, value, state, message));
};

module Make = (Form: Config) => {
  module FieldsComparator =
    Id.MakeComparable({
      type t = Form.field;
      let cmp = Pervasives.compare;
    });

  type state = {
    data: Form.state,
    status: FormStatus.t(Form.field, Form.message),
    results:
      Map.t(
        Form.field,
        Validation.validationResult(Form.message),
        FieldsComparator.identity,
      ),
    emittedFields: Set.t(Form.field, FieldsComparator.identity),
    submittedOnce: bool,
  };

  type action =
    | Change((Form.field, Form.value))
    | Blur((Form.field, Form.value))
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
    results: Form.field => option(Validation.validationResult(Form.message)),
    submitting: bool,
    change: (Form.value, Form.field) => unit,
    blur: (Form.value, Form.field) => unit,
    submit: unit => unit,
    dismissSubmissionResult: unit => unit,
  };

  let getInitialState = data => {
    data,
    status: FormStatus.Editing,
    results: Map.make(~id=(module FieldsComparator)),
    emittedFields: Set.make(~id=(module FieldsComparator)),
    submittedOnce: false,
  };

  let validator = field =>
    Form.validators->List.getBy(validator => validator.field === field);

  let validateDependents = (~data, ~results, ~emittedFields, dependents) =>
    dependents->List.reduce(
      (results, emittedFields),
      ((results, emittedFields), field) => {
        let validator = field->validator;
        let emitted = emittedFields->Set.has(field);
        switch (validator, emitted) {
        | (None, _)
        | (_, false) => (results, emittedFields)
        | (Some(validator), true) =>
          let value = data->Form.get(field);
          let result = data |> validator.validate(value);
          (
            switch (result) {
            | Valid when value->Form.valueEmpty => results->Map.remove(field)
            | Valid
            | Invalid(_) => results->Map.set(field, result)
            },
            emittedFields->Set.add(field),
          );
        };
      },
    );

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
      | Change((field, value)) =>
        let data = state.data->Form.set((field, value));
        switch (field->validator) {
        | None => React.Update({...state, data})
        | Some(validator) =>
          let emitted = state.emittedFields->Set.has(field);
          switch (validator.strategy, emitted, state.submittedOnce) {
          | (_, true, _)
          | (_, _, true)
          | (Strategy.OnFirstChange, false, false) =>
            let result = data |> validator.validate(value);
            let (results, emittedFields) =
              switch (validator.dependents) {
              | None => (state.results, state.emittedFields)
              | Some(dependents) =>
                dependents->validateDependents(
                  ~data,
                  ~results=state.results,
                  ~emittedFields=state.emittedFields,
                )
              };
            React.Update({
              ...state,
              data,
              results:
                switch (result) {
                | Valid when value->Form.valueEmpty =>
                  results->Map.remove(field)
                | Valid
                | Invalid(_) => results->Map.set(field, result)
                },
              emittedFields: emittedFields->Set.add(field),
            });

          | (
              Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur,
              false,
              false,
            ) =>
            let result = data |> validator.validate(value);
            let (results, emittedFields) =
              switch (validator.dependents) {
              | None => (state.results, state.emittedFields)
              | Some(dependents) =>
                dependents->validateDependents(
                  ~data,
                  ~results=state.results,
                  ~emittedFields=state.emittedFields,
                )
              };
            switch (result) {
            | Valid =>
              React.Update({
                ...state,
                data,
                results:
                  value->Form.valueEmpty ?
                    results->Map.remove(field) :
                    results->Map.set(field, result),
                emittedFields: emittedFields->Set.add(field),
              })
            | Invalid(_) =>
              React.Update({...state, data, results, emittedFields})
            };

          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false, false) =>
            React.Update({...state, data})
          };
        };

      | Blur((field, value)) =>
        let validator = field->validator;
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
            let result = state.data |> validator.validate(value);
            React.Update({
              ...state,
              results:
                switch (result) {
                | Valid when value->Form.valueEmpty =>
                  state.results->Map.remove(field)
                | Valid
                | Invalid(_) => state.results->Map.set(field, result)
                },
              emittedFields: state.emittedFields->Set.add(field),
            });
          }
        };

      | Submit =>
        switch (state.status) {
        | FormStatus.Submitting => React.NoUpdate
        | _ =>
          let (valid, results) =
            Form.validators->List.reduce(
              (true, state.results),
              ((valid, results), validator) => {
                let value = state.data->Form.get(validator.field);
                let result = state.data |> validator.validate(value);
                let results =
                  switch (result) {
                  | Valid when value->Form.valueEmpty =>
                    results->Map.remove(validator.field)
                  | Valid
                  | Invalid(_) => results->Map.set(validator.field, result)
                  };
                switch (valid, result) {
                | (false, _)
                | (true, Invalid(_)) => (false, results)
                | (true, Valid) => (true, results)
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
        results: field => state.results->Map.get(field),
        submitting:
          switch (state.status) {
          | Submitting => true
          | Editing
          | Submitted
          | SubmissionFailed(_, _) => false
          },
        change: (value, field) => (field, value)->Change->send,
        blur: (value, field) => (field, value)->Blur->send,
        submit: () => Submit->send,
        dismissSubmissionResult: () => DismissSubmissionResult->send,
      }),
  };
};
