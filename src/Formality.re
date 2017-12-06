module Strategy = {
  type t =
    | OnFirstBlur
    | OnFirstChange
    | OnFirstSuccess
    | OnFirstSuccessOrFirstBlur
    | OnSubmit;
};

module AsyncStrategy = {
  type t =
    | OnChange
    | OnBlur;
};

module Utils = {
  let comparator = (a, b) => a === b ? 0 : 1;
  let targetValue = (element) => (element |> ReactDOMRe.domElementToObj)##value;
  let formEventTargetValue = (event) => event |> ReactEventRe.Form.target |> targetValue;
  let focusEventTargetValue = (event) => event |> ReactEventRe.Focus.target |> targetValue;
};

/* TODO: Make variant? */
type value = string;

/* TODO: Define type in config as `message` might be anything, e.g. i18n object */
type validityBag = {
  valid: bool,
  tag: option(string),
  message: option(string)
};

type resolution =
  | Valid(bool)
  | ValidityBag(validityBag);

/* TODO: Dependant fields */
/* TODO: Async validation */
type validator('field, 'state) = {
  strategy: option(Strategy.t),
  dependents: option(list('field)),
  validate: (option(value), 'state) => resolution
};

module type ValidatorsConfig = {type t;};

module MakeValidators = (Config: ValidatorsConfig) =>
  Map.Make(
    {
      type t = Config.t;
      let compare = Utils.comparator;
    }
  );

module type Form = {
  type field;
  type state;
  let update: ((field, value), state) => state;
  /* Validation */
  let strategy: Strategy.t;
  type validators;
  let validators: validators;
  module Validators: {
    let find: (field, validators) => validator(field, state);
    let fold: ((field, validator(field, state), 'a) => 'a, validators, 'a) => 'a;
  };
  /* TODO: mapStateToView ? */
  /* TODO: mapViewToState ? */
};

module Make = (Form: Form) => {
  module FieldsSet =
    Set.Make(
      {
        type t = Form.field;
        let compare = Utils.comparator;
      }
    );
  module ResultsMap =
    Map.Make(
      {
        type t = Form.field;
        let compare = Utils.comparator;
      }
    );
  module Results = {
    type key = ResultsMap.key;
    type t = ResultsMap.t(option(resolution));
    let get = (key: key, map: t) =>
      switch (map |> ResultsMap.find(key)) {
      | resolution => resolution
      | exception Not_found => None
      };
  };
  type state = {
    data: Form.state,
    results: Results.t,
    submitting: bool,
    submittedOnce: bool,
    emittedFields: FieldsSet.t
  };
  type action =
    | Change((Form.field, value))
    | Blur((Form.field, value))
    | Submit
    | Reset
    | HandleSubmissionError;
  type interface = {
    state: Form.state,
    results: Form.field => option(resolution),
    submitting: bool,
    update: Form.field => ReasonReact.Callback.t(ReactEventRe.Form.t),
    blur: (Form.field, ReactEventRe.Focus.t) => unit,
    submit: ReasonReact.Callback.t(ReactEventRe.Form.t)
  };
  let getInitialState = (data) => {
    data,
    results: ResultsMap.empty,
    submitting: false,
    submittedOnce: false,
    emittedFields: FieldsSet.empty
  };
  let getValidator = (field) =>
    switch (Form.validators |> Form.Validators.find(field)) {
    | validator => Some(validator)
    | exception Not_found => None
    };
  let getStrategy = (validator) => validator.strategy |> Js.Option.getWithDefault(Form.strategy);
  let validateDependents = (~data, ~results, ~emittedFields, dependents) =>
    dependents
    |> List.fold_left(
         ((results', emittedFields'), field') => {
           let validator' = field' |> getValidator;
           let emitted' = emittedFields |> FieldsSet.mem(field');
           switch (validator', emitted') {
           | (None, _)
           | (_, false) => (results', emittedFields')
           | (Some(validator'), true) =>
             let result' = data |> validator'.validate(None);
             (
               results' |> ResultsMap.add(field', Some(result')),
               emittedFields' |> FieldsSet.add(field')
             )
           }
         },
         (results, emittedFields)
       );
  let component = ReasonReact.reducerComponent("Formality");
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit:
           (~notifyOnSuccess: unit => unit, ~notifyOnFailure: unit => unit, Form.state) => unit,
        children
      ) => {
    ...component,
    initialState: () => getInitialState(initialState),
    reducer: (action, state) =>
      switch action {
      | Change((field, value)) =>
        let data = state.data |> Form.update((field, value));
        switch (field |> getValidator) {
        | None => ReasonReact.Update({...state, data})
        | Some(validator) =>
          let strategy = validator |> getStrategy;
          let emitted = state.emittedFields |> FieldsSet.mem(field);
          switch (strategy, emitted, state.submittedOnce) {
          | (_, true, _)
          | (_, _, true)
          | (Strategy.OnFirstChange, false, false) =>
            let result = data |> validator.validate(Some(value));
            switch validator.dependents {
            | Some(dependents) =>
              let (results, emittedFields) =
                validateDependents(
                  ~data,
                  ~results=state.results,
                  ~emittedFields=state.emittedFields,
                  dependents
                );
              ReasonReact.Update({
                ...state,
                data,
                results: results |> ResultsMap.add(field, Some(result)),
                emittedFields: emittedFields |> FieldsSet.add(field)
              })
            | None =>
              ReasonReact.Update({
                ...state,
                data,
                results: state.results |> ResultsMap.add(field, Some(result)),
                emittedFields: state.emittedFields |> FieldsSet.add(field)
              })
            }
          | (Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur, false, false) =>
            let result = data |> validator.validate(Some(value));
            switch result {
            | Valid(false) => ReasonReact.Update({...state, data})
            | ValidityBag(validityBag) when ! validityBag.valid =>
              switch validator.dependents {
              | Some(dependents) =>
                let (results, emittedFields) =
                  validateDependents(
                    ~data,
                    ~results=state.results,
                    ~emittedFields=state.emittedFields,
                    dependents
                  );
                ReasonReact.Update({...state, data, results, emittedFields})
              | None => ReasonReact.Update({...state, data})
              }
            | Valid(true)
            | ValidityBag(_) =>
              switch validator.dependents {
              | Some(dependents) =>
                let (results, emittedFields) =
                  validateDependents(
                    ~data,
                    ~results=state.results,
                    ~emittedFields=state.emittedFields,
                    dependents
                  );
                ReasonReact.Update({
                  ...state,
                  data,
                  results: results |> ResultsMap.add(field, Some(result)),
                  emittedFields: emittedFields |> FieldsSet.add(field)
                })
              | None =>
                ReasonReact.Update({
                  ...state,
                  data,
                  results: state.results |> ResultsMap.add(field, Some(result)),
                  emittedFields: state.emittedFields |> FieldsSet.add(field)
                })
              }
            }
          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false, false) =>
            ReasonReact.Update({...state, data})
          }
        }
      | Blur((field, value)) =>
        let validator = field |> getValidator;
        let emitted = state.emittedFields |> FieldsSet.mem(field);
        switch (validator, emitted) {
        | (_, true)
        | (None, false) => ReasonReact.NoUpdate
        | (Some(validator), false) =>
          switch (validator |> getStrategy) {
          | Strategy.OnFirstBlur
          | Strategy.OnFirstSuccessOrFirstBlur =>
            let result = state.data |> validator.validate(Some(value));
            ReasonReact.Update({
              ...state,
              results: state.results |> ResultsMap.add(field, Some(result)),
              emittedFields: state.emittedFields |> FieldsSet.add(field)
            })
          | Strategy.OnFirstChange
          | Strategy.OnFirstSuccess
          | Strategy.OnSubmit => ReasonReact.NoUpdate
          }
        }
      | Submit =>
        let (valid, results) =
          (true, state.results)
          |> Form.Validators.fold(
               (field, validator, (valid, results)) => {
                 let result = state.data |> validator.validate(None);
                 let results' = results |> ResultsMap.add(field, Some(result));
                 switch (valid, result) {
                 | (false, _) => (false, results')
                 | (true, Valid(valid')) => (valid', results')
                 | (true, ValidityBag(validity)) => (validity.valid, results')
                 }
               },
               Form.validators
             );
        valid ?
          ReasonReact.UpdateWithSideEffects(
            {...state, results, submitting: true, submittedOnce: true},
            /* TODO: notifyOnFailure should accept errors */
            (
              ({state, reduce}) =>
                state.data
                |> onSubmit(
                     ~notifyOnSuccess=reduce(() => Reset),
                     ~notifyOnFailure=reduce(() => HandleSubmissionError)
                   )
            )
          ) :
          ReasonReact.Update({...state, results, submitting: false, submittedOnce: true})
      | Reset => ReasonReact.Update(initialState |> getInitialState)
      | HandleSubmissionError => ReasonReact.Update({...state, submitting: false})
      },
    render: ({state, reduce}) =>
      children({
        state: state.data,
        results: (field) => state.results |> Results.get(field),
        submitting: state.submitting,
        update: (field) => reduce((event) => Change((field, event |> Utils.formEventTargetValue))),
        blur: (field) => reduce((event) => Blur((field, event |> Utils.focusEventTargetValue))),
        submit:
          reduce(
            (event) => {
              if (! ReactEventRe.Form.defaultPrevented(event)) {
                event |> ReactEventRe.Form.preventDefault
              };
              Submit
            }
          )
      })
  };
};
