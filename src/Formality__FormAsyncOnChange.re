module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

let defaultDebounceInterval = 700;

module type Form = {
  type field;
  type state;
  type message;
  let debounceInterval: int;
  let validators: list(Validation.asyncValidator(field, state, message));
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
        Map.t(Form.field, debouncedAsyncValidator, FieldComparator.identity),
      ),
    results:
      Map.t(
        Form.field,
        Validation.result(Form.message),
        FieldComparator.identity,
      ),
    emittedFields: Set.t(Form.field, FieldComparator.identity),
    validatingFields: Set.t(Form.field, FieldComparator.identity),
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
            React.self(state, React.noRetainedProps, action),
          )
        ) =>
        unit,
      )
    | TriggerAsyncValidation(
        Form.field,
        Form.state,
        Validation.validateAsync(Form.state, Form.message),
      )
    | ApplyAsyncResult(
        Form.field,
        Form.state,
        Validation.result(Form.message),
      )
    | Submit
    | SetSubmittedStatus(option(Form.state))
    | SetSubmissionFailedStatus(
        list((Form.field, Form.message)),
        option(Form.message),
      )
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
              React.self(state, React.noRetainedProps, action),
            )
          ) =>
          unit,
          Validation.checkEquality(Form.state),
        ),
      ),
  };

  type interface = {
    state: Form.state,
    status: FormStatus.t(Form.field, Form.message),
    result: Form.field => option(Validation.result(Form.message)),
    validating: Form.field => bool,
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    dismissSubmissionResult: unit => unit,
  };

  let debounce = (~wait, fn) => {
    let fn = ((field, data, {React.send})) =>
      TriggerAsyncValidation(field, data, fn)->send;
    fn->(Debouncer.make(~wait));
  };

  let getInitialState = data => {
    data,
    status: FormStatus.Editing,
    validators:
      ref(
        Form.validators->List.reduce(
          Map.make(~id=(module FieldComparator)), (fields, validator) =>
          fields->Map.set(
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
    results: Map.make(~id=(module FieldComparator)),
    emittedFields: Set.make(~id=(module FieldComparator)),
    validatingFields: Set.make(~id=(module FieldComparator)),
    submittedOnce: false,
  };

  let validateDependents =
      (
        ~data: Form.state,
        ~validators:
           ref(
             Map.t(
               Form.field,
               debouncedAsyncValidator,
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
            switch (validator.validateAsync) {
            | None =>
              React.Update({
                ...state,
                data,
                results: results->Map.set(field, result),
                emittedFields: emittedFields->Set.add(field),
              })
            | Some((validateAsync, _)) =>
              switch (result) {
              | Valid =>
                React.UpdateWithSideEffects(
                  {
                    ...state,
                    data,
                    results: results->Map.remove(field),
                    emittedFields: emittedFields->Set.add(field),
                    validatingFields: state.validatingFields->Set.add(field),
                  },
                  (
                    ({send}) =>
                      InvokeDebouncedAsyncValidation(
                        field,
                        data,
                        validateAsync,
                      )
                      ->send
                  ),
                )

              | Optional
              | Invalid(_) =>
                React.Update({
                  ...state,
                  data,
                  results: results->Map.set(field, result),
                  emittedFields: emittedFields->Set.add(field),
                  validatingFields: state.validatingFields->Set.remove(field),
                })
              }
            };

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
            switch (validator.validateAsync) {
            | None =>
              switch (result) {
              | Valid
              | Optional =>
                React.Update({
                  ...state,
                  data,
                  results: results->Map.set(field, result),
                  emittedFields: emittedFields->Set.add(field),
                })
              | Invalid(_) =>
                React.Update({...state, data, results, emittedFields})
              }

            | Some((validateAsync, _)) =>
              switch (result) {
              | Valid =>
                React.UpdateWithSideEffects(
                  {
                    ...state,
                    data,
                    results: results->Map.remove(field),
                    emittedFields: emittedFields->Set.add(field),
                    validatingFields: state.validatingFields->Set.add(field),
                  },
                  (
                    ({send}) =>
                      InvokeDebouncedAsyncValidation(
                        field,
                        data,
                        validateAsync,
                      )
                      ->send
                  ),
                )
              | Optional =>
                React.Update({
                  ...state,
                  data,
                  results: results->Map.set(field, result),
                  validatingFields: state.validatingFields->Set.remove(field),
                  emittedFields: emittedFields->Set.add(field),
                })
              | Invalid(_) =>
                React.Update({...state, data, results, emittedFields})
              }
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
            switch (validator.validateAsync) {
            | None =>
              React.Update({
                ...state,
                results: state.results->Map.set(field, result),
                emittedFields: state.emittedFields->Set.add(field),
              })
            | Some((validateAsync, _)) =>
              switch (result) {
              | Valid =>
                React.UpdateWithSideEffects(
                  {
                    ...state,
                    results: state.results->Map.remove(field),
                    emittedFields: state.emittedFields->Set.add(field),
                    validatingFields: state.validatingFields->Set.add(field),
                  },
                  (
                    ({send}) =>
                      InvokeDebouncedAsyncValidation(
                        field,
                        state.data,
                        validateAsync,
                      )
                      ->send
                  ),
                )
              | Optional
              | Invalid(_) =>
                React.Update({
                  ...state,
                  results: state.results->Map.set(field, result),
                  emittedFields: state.emittedFields->Set.add(field),
                  validatingFields: state.validatingFields->Set.remove(field),
                })
              }
            };
          }
        };

      | InvokeDebouncedAsyncValidation(field, data, validateAsync) =>
        React.SideEffects((self => (field, data, self)->validateAsync))

      | TriggerAsyncValidation(field, data, validateAsync) =>
        React.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                data
                ->validateAsync
                ->then_(
                    result => {
                      ApplyAsyncResult(field, data, result)->send;
                      resolve();
                    },
                    _,
                  )
                ->ignore
              )
          ),
        )

      | ApplyAsyncResult(field, data, result) =>
        let validator = (state.validators^)->Map.getExn(field);
        let eq = validator.validateAsync->Option.getExn->snd;
        if (data->eq(state.data)) {
          React.Update({
            ...state,
            results: state.results->Map.set(field, result),
            emittedFields: state.emittedFields->Set.add(field),
            validatingFields: state.validatingFields->Set.remove(field),
          });
        } else {
          React.NoUpdate;
        };

      | Submit =>
        switch (state.status, state.validatingFields->Set.isEmpty) {
        | (_, false)
        | (FormStatus.Submitting, _) => React.NoUpdate
        | _ =>
          let (valid, results) =
            (state.validators^)
            ->Map.reduce(
                (true, state.results),
                ((valid, results), field, validator) => {
                  let currentResultIsInvalid =
                    switch (results->Map.get(field)) {
                    | Some(Invalid(_)) => true
                    | Some(Valid)
                    | Some(Optional)
                    | None => false
                    };
                  let result = state.data->(validator.validate);
                  let results =
                    switch (
                      currentResultIsInvalid,
                      result,
                      validator.validateAsync,
                    ) {
                    | (true, Valid, Some(_)) => results
                    | (_, _, _) => results->Map.set(field, result)
                    };
                  switch (valid, results->Map.get(field)) {
                  | (false, _)
                  | (true, Some(Invalid(_))) => (false, results)
                  | (true, Some(Valid | Optional))
                  | (_, None) => (true, results)
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
        validating: field => state.validatingFields->Set.has(field),
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
