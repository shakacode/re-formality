module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

let defaultDebounceInterval = 700;

module type Config = {
  type field;
  type value;
  type state;
  type message;

  let get: (state, field) => value;
  let set: (state, (field, value)) => state;
  let valueEmpty: value => bool;
  let debounceInterval: int;
  let validators:
    list(Validation.asyncValidator(field, value, state, message));
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
    validatingFields: Set.t(Form.field, FieldsComparator.identity),
    submittedOnce: bool,
  };

  type action =
    | Change((Form.field, Form.value))
    | Blur((Form.field, Form.value))
    | InvokeDebouncedAsyncValidation(
        Form.field,
        Form.value,
        (
          ~field: Form.field,
          ~value: Form.value,
          React.self(state, React.noRetainedProps, action)
        ) =>
        unit,
      )
    | TriggerAsyncValidation(
        Form.field,
        Form.value,
        Validation.validateAsync(Form.value, Form.message),
      )
    | ApplyAsyncResult(
        Form.field,
        Form.value,
        Validation.validationResult(Form.message),
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
    results: Form.field => option(Validation.validationResult(Form.message)),
    validating: Form.field => bool,
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
    validatingFields: Set.make(~id=(module FieldsComparator)),
    submittedOnce: false,
  };

  let debounce = (~validateAsync, ~wait) => {
    let lastSelf = ref(None);
    let lastField = ref(None);
    let lastValue = ref(None);
    let lastCallTime = ref(None);
    let timerId = ref(None);
    let shouldInvoke = time =>
      switch (lastCallTime^) {
      | None => true
      | Some(lastCallTime) =>
        let timeSinceLastCall = time - lastCallTime;
        timeSinceLastCall >= wait || timeSinceLastCall < 0;
      };
    let remainingWait = time =>
      switch (lastCallTime^) {
      | None => wait
      | Some(lastCallTime) =>
        let timeSinceLastCall = time - lastCallTime;
        wait - timeSinceLastCall;
      };
    let rec timerExpired = () => {
      let time = Js.Date.now() |> int_of_float;
      time |> shouldInvoke ?
        invoke() :
        timerId :=
          Some(Js.Global.setTimeout(timerExpired, time |> remainingWait));
    }
    and invoke = () => {
      timerId := None;
      if (lastValue^ |> Js.Option.isSome) {
        let field = lastField^;
        let value = lastValue^;
        let self = lastSelf^;
        lastSelf := None;
        lastValue := None;
        switch (field, value, self) {
        | (Some(field), Some(value), Some({React.send})) =>
          send(TriggerAsyncValidation(field, value, validateAsync))
        | _ => ()
        };
      };
    };
    let debounced = (~field, ~value, self) => {
      let time = Js.Date.now() |> int_of_float;
      lastCallTime := Some(time);
      lastField := Some(field);
      lastValue := Some(value);
      lastSelf := Some(self);
      timerId := Some(Js.Global.setTimeout(timerExpired, wait));
    };
    debounced;
  };

  type debouncedValidator = {
    field: Form.field,
    strategy: Strategy.t,
    dependents: option(list(Form.field)),
    validate: Validation.validate(Form.value, Form.state, Form.message),
    validateAsync:
      option(
        (
          ~field: Form.field,
          ~value: Form.value,
          React.self(state, React.noRetainedProps, action)
        ) =>
        unit,
      ),
  };

  let debouncedValidators =
    Form.validators->List.map(validator =>
      switch (validator.validateAsync) {
      | Some(validateAsync) => {
          field: validator.field,
          strategy: validator.strategy,
          dependents: validator.dependents,
          validate: validator.validate,
          validateAsync:
            Some(debounce(~validateAsync, ~wait=Form.debounceInterval)),
        }
      | None => {
          field: validator.field,
          strategy: validator.strategy,
          dependents: validator.dependents,
          validate: validator.validate,
          validateAsync: None,
        }
      }
    );

  let validator = field =>
    debouncedValidators->List.getBy(validator => validator.field === field);

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
            switch (validator.validateAsync) {
            | None =>
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
              })
            | Some(validateAsync) =>
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
                        value,
                        validateAsync,
                      )
                      ->send
                  ),
                )
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
            switch (validator.validateAsync) {
            | None =>
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
              }
            | Some(validateAsync) =>
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
                        value,
                        validateAsync,
                      )
                      ->send
                  ),
                )
              | Invalid(_) =>
                React.Update({...state, data, results, emittedFields})
              }
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
            switch (validator.validateAsync) {
            | None =>
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
              })
            | Some(validateAsync) =>
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
                        value,
                        validateAsync,
                      )
                      ->send
                  ),
                )
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

      | InvokeDebouncedAsyncValidation(field, value, validateAsync) =>
        React.SideEffects(validateAsync(~field, ~value))

      | TriggerAsyncValidation(field, value, validateAsync) =>
        React.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                value
                ->validateAsync
                ->then_(
                    result => {
                      ApplyAsyncResult(field, value, result)->send;
                      resolve();
                    },
                    _,
                  )
                ->ignore
              )
          ),
        )

      | ApplyAsyncResult(field, value, result) =>
        if (value === state.data->Form.get(field)) {
          React.Update({
            ...state,
            results: state.results->Map.set(field, result),
            emittedFields: state.emittedFields->Set.add(field),
            validatingFields: state.validatingFields->Set.remove(field),
          });
        } else {
          React.NoUpdate;
        }

      | Submit =>
        switch (state.status, state.validatingFields->Set.isEmpty) {
        | (_, false)
        | (FormStatus.Submitting, _) => React.NoUpdate
        | _ =>
          let (valid, results) =
            debouncedValidators->List.reduce(
              (true, state.results),
              ((valid, results), validator) => {
                let value = state.data->Form.get(validator.field);
                let currentResultIsInvalid =
                  switch (results->Map.get(validator.field)) {
                  | Some(Invalid(_)) => true
                  | Some(Valid)
                  | None => false
                  };
                let result = state.data |> validator.validate(value);
                let results =
                  switch (
                    currentResultIsInvalid,
                    result,
                    validator.validateAsync,
                  ) {
                  | (true, Valid, Some(_)) => results
                  | (_, Valid, _) =>
                    if (value->Form.valueEmpty) {
                      results->Map.remove(validator.field);
                    } else {
                      results->Map.set(validator.field, result);
                    }
                  | (_, _, _) => results->Map.set(validator.field, result)
                  };
                switch (valid, results->Map.get(validator.field)) {
                | (false, _)
                | (true, Some(Invalid(_))) => (false, results)
                | (true, Some(Valid))
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
        results: field => state.results->Map.get(field),
        validating: field => state.validatingFields->Set.has(field),
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
