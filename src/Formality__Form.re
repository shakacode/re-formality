module Validation = Formality__Validation;

module Strategy = Formality__Strategy;

module Utils = Formality__Utils;

module type Config = {
  type field;
  type value;
  type state;
  type message;
  let get: (field, state) => value;
  let update: ((field, value), state) => state;
  let valueEmpty: value => bool;
  type validators;
  let validators: validators;
  module Validators: {
    let find:
      (field, validators) =>
      Validation.validator(field, value, state, message);
    let fold:
      (
        (field, Validation.validator(field, value, state, message), 'a) => 'a,
        validators,
        'a
      ) =>
      'a;
  };
};

module Make = (Form: Config) => {
  module FieldsSetOrigin =
    Set.Make(
      {
        type t = Form.field;
        let compare = Utils.comparator;
      },
    );
  module FieldsSet = {
    type t = FieldsSetOrigin.t;
    let empty = FieldsSetOrigin.empty;
    let mem = FieldsSetOrigin.mem;
    let add = FieldsSetOrigin.add;
    let remove = FieldsSetOrigin.remove;
  };
  module ResultsMapOrigin =
    Map.Make(
      {
        type t = Form.field;
        let compare = Utils.comparator;
      },
    );
  module ResultsMap = {
    type key = ResultsMapOrigin.key;
    type t =
      ResultsMapOrigin.t(option(Validation.validationResult(Form.message)));
    let empty = ResultsMapOrigin.empty;
    let add = ResultsMapOrigin.add;
    let get = (key: key, map: t) =>
      switch (map |> ResultsMapOrigin.find(key)) {
      | result => result
      | exception Not_found => None
      };
  };
  type state = {
    data: Form.state,
    results: ResultsMap.t,
    submitting: bool,
    submittedOnce: bool,
    emittedFields: FieldsSet.t,
  };
  type action =
    | Change((Form.field, Form.value))
    | Blur((Form.field, Form.value))
    | Submit
    | Reset
    | HandleSubmissionError;
  type interface = {
    state: Form.state,
    results: Form.field => option(Validation.validationResult(Form.message)),
    submitting: bool,
    change: (Form.field, Form.value) => unit,
    blur: (Form.field, Form.value) => unit,
    submit: unit => unit,
  };
  let getInitialState = data => {
    data,
    results: ResultsMap.empty,
    submitting: false,
    submittedOnce: false,
    emittedFields: FieldsSet.empty,
  };
  let getValidator = field =>
    switch (Form.validators |> Form.Validators.find(field)) {
    | validator => Some(validator)
    | exception Not_found => None
    };
  let validateDependents = (~data, ~results, ~emittedFields, dependents) =>
    dependents
    |> List.fold_left(
         ((results', emittedFields'), field') => {
           let validator = field' |> getValidator;
           let emitted = emittedFields |> FieldsSet.mem(field');
           switch (validator, emitted) {
           | (None, _)
           | (_, false) => (results', emittedFields')
           | (Some(validator), true) =>
             let value = data |> Form.get(field');
             let result = data |> validator.validate(value);
             (
               results'
               |> ResultsMap.add(
                    field',
                    value |> Form.valueEmpty ? None : Some(result),
                  ),
               emittedFields' |> FieldsSet.add(field'),
             );
           };
         },
         (results, emittedFields),
       );
  let component = "FormalityForm" |> ReasonReact.reducerComponent;
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit: (Form.state, Validation.notifiers) => unit,
        children,
      ) => {
    ...component,
    initialState: () => getInitialState(initialState),
    reducer: (action, state) =>
      switch (action) {
      | Change((field, value)) =>
        let data = state.data |> Form.update((field, value));
        switch (field |> getValidator) {
        | None => ReasonReact.Update({...state, data})
        | Some(validator) =>
          let emitted = state.emittedFields |> FieldsSet.mem(field);
          switch (validator.strategy, emitted, state.submittedOnce) {
          | (_, true, _)
          | (_, _, true)
          | (Strategy.OnFirstChange, false, false) =>
            switch (validator.dependents) {
            | Some(dependents) =>
              let result = data |> validator.validate(value);
              let (results, emittedFields) =
                dependents
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields,
                   );
              ReasonReact.Update({
                ...state,
                data,
                results:
                  results
                  |> ResultsMap.add(
                       field,
                       value |> Form.valueEmpty ? None : Some(result),
                     ),
                emittedFields: emittedFields |> FieldsSet.add(field),
              });
            | None =>
              let result = data |> validator.validate(value);
              ReasonReact.Update({
                ...state,
                data,
                results:
                  state.results
                  |> ResultsMap.add(
                       field,
                       value |> Form.valueEmpty ? None : Some(result),
                     ),
                emittedFields: state.emittedFields |> FieldsSet.add(field),
              });
            }
          | (
              Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur,
              false,
              false,
            ) =>
            data
            |> validator.validate(value)
            |> Validation.ifResult(
                 ~valid=
                   result =>
                     switch (validator.dependents) {
                     | Some(dependents) =>
                       let (results, emittedFields) =
                         validateDependents(
                           ~data,
                           ~results=state.results,
                           ~emittedFields=state.emittedFields,
                           dependents,
                         );
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           results
                           |> ResultsMap.add(
                                field,
                                value |> Form.valueEmpty ?
                                  None : Some(result),
                              ),
                         emittedFields: emittedFields |> FieldsSet.add(field),
                       });
                     | None =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           state.results
                           |> ResultsMap.add(
                                field,
                                value |> Form.valueEmpty ?
                                  None : Some(result),
                              ),
                         emittedFields:
                           state.emittedFields |> FieldsSet.add(field),
                       })
                     },
                 ~invalid=
                   (_) =>
                     switch (validator.dependents) {
                     | Some(dependents) =>
                       let (results, emittedFields) =
                         validateDependents(
                           ~data,
                           ~results=state.results,
                           ~emittedFields=state.emittedFields,
                           dependents,
                         );
                       ReasonReact.Update({
                         ...state,
                         data,
                         results,
                         emittedFields,
                       });
                     | None => ReasonReact.Update({...state, data})
                     },
               )
          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false, false) =>
            ReasonReact.Update({...state, data})
          };
        };
      | Blur((field, value)) =>
        let validator = field |> getValidator;
        let emitted = state.emittedFields |> FieldsSet.mem(field);
        switch (validator, emitted) {
        | (None, _)
        | (Some(_), true) => ReasonReact.NoUpdate
        | (Some(validator), false) =>
          switch (validator.strategy) {
          | Strategy.OnFirstChange
          | Strategy.OnFirstSuccess
          | Strategy.OnSubmit => ReasonReact.NoUpdate
          | Strategy.OnFirstBlur
          | Strategy.OnFirstSuccessOrFirstBlur =>
            let result = state.data |> validator.validate(value);
            ReasonReact.Update({
              ...state,
              results:
                state.results
                |> ResultsMap.add(
                     field,
                     value |> Form.valueEmpty ? None : Some(result),
                   ),
              emittedFields: state.emittedFields |> FieldsSet.add(field),
            });
          }
        };
      | Submit =>
        let (valid, results) =
          (true, state.results)
          |> Form.Validators.fold(
               (field', validator', (valid', results')) => {
                 let value = state.data |> Form.get(field');
                 let result = state.data |> validator'.validate(value);
                 let results =
                   switch (result) {
                   | Validation.Valid =>
                     results'
                     |> ResultsMap.add(
                          field',
                          value |> Form.valueEmpty ? None : Some(result),
                        )
                   | Validation.Invalid(_) =>
                     results' |> ResultsMap.add(field', Some(result))
                   };
                 switch (valid', result) {
                 | (false, _)
                 | (true, Validation.Invalid(_)) => (false, results)
                 | (true, Validation.Valid) => (true, results)
                 };
               },
               Form.validators,
             );
        valid ?
          ReasonReact.UpdateWithSideEffects(
            {...state, results, submitting: true, submittedOnce: true},
            (
              ({state, send}) =>
                onSubmit(
                  state.data,
                  {
                    onSuccess: () => Reset |> send,
                    onFailure: () => HandleSubmissionError |> send,
                  },
                )
            ),
          ) :
          ReasonReact.Update({
            ...state,
            results,
            submitting: false,
            submittedOnce: true,
          });
      | Reset => ReasonReact.Update(initialState |> getInitialState)
      | HandleSubmissionError =>
        ReasonReact.Update({...state, submitting: false})
      },
    render: ({state, send}) =>
      children({
        state: state.data,
        results: field => state.results |> ResultsMap.get(field),
        submitting: state.submitting,
        change: (field, value) => Change((field, value)) |> send,
        blur: (field, value) => Blur((field, value)) |> send,
        submit: () => Submit |> send,
      }),
  };
};
