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

module Log = {
  [@bs.val] [@bs.scope "console"] external error : 'a => unit = "error";
  let noAsyncStrategyError = () =>
    error(
      "There's an async validator defined on one of the fields, but asyncStrategy is set to None"
    );
};

module Utils = {
  let comparator = (a, b) => a === b ? 0 : 1;
  let targetValue = element => (element |> ReactDOMRe.domElementToObj)##value;
  let formEventTargetValue = event =>
    event |> ReactEventRe.Form.target |> targetValue;
  let focusEventTargetValue = event =>
    event |> ReactEventRe.Focus.target |> targetValue;
};

/* TODO: Make variant? */
type value = string;

/* TODO: Define type in config as `message` might be anything, e.g. i18n object */
type validityBag = {
  valid: bool,
  tag: option(string),
  message: option(string)
};

type result =
  | Valid(bool)
  | ValidityBag(validityBag);

type validator('field, 'state) = {
  strategy: option(Strategy.t),
  dependents: option(list('field)),
  validate: (option(value), 'state) => result,
  validateAsync: option(value => Js.Promise.t(result))
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
  let get: (field, state) => value;
  let update: ((field, value), state) => state;
  /* Validation */
  let strategy: Strategy.t;
  let asyncStrategy: option(AsyncStrategy.t);
  type validators;
  let validators: validators;
  module Validators: {
    let find: (field, validators) => validator(field, state);
    let fold:
      ((field, validator(field, state), 'a) => 'a, validators, 'a) => 'a;
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
    type t = ResultsMap.t(option(result));
    let get = (key: key, map: t) =>
      switch (map |> ResultsMap.find(key)) {
      | result => result
      | exception Not_found => None
      };
  };
  type state = {
    data: Form.state,
    results: Results.t,
    validating: FieldsSet.t,
    submitting: bool,
    submittedOnce: bool,
    emittedFields: FieldsSet.t
  };
  type action =
    | Change((Form.field, value))
    | Blur((Form.field, value))
    | ApplyAsyncResult(Form.field, value, result)
    | Submit
    | Reset
    | HandleSubmissionError;
  type interface = {
    state: Form.state,
    results: Form.field => option(result),
    validating: Form.field => bool,
    submitting: bool,
    update: Form.field => ReasonReact.Callback.t(ReactEventRe.Form.t),
    blur: (Form.field, ReactEventRe.Focus.t) => unit,
    submit: ReasonReact.Callback.t(ReactEventRe.Form.t)
  };
  let getInitialState = data => {
    data,
    results: ResultsMap.empty,
    validating: FieldsSet.empty,
    submitting: false,
    submittedOnce: false,
    emittedFields: FieldsSet.empty
  };
  let getValidator = field =>
    switch (Form.validators |> Form.Validators.find(field)) {
    | validator => Some(validator)
    | exception Not_found => None
    };
  let getStrategy = validator =>
    validator.strategy |> Js.Option.getWithDefault(Form.strategy);
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
             );
           };
         },
         (results, emittedFields)
       );
  let validateFieldAsync = (field, value, validateAsync, {ReasonReact.reduce}) =>
    Js.Promise.(
      value
      |> validateAsync
      |> then_(result => {
           reduce(() => ApplyAsyncResult(field, value, result), ());
           resolve();
         })
      |> ignore
    );
  let ifResult = (~valid, ~invalid, result) =>
    switch result {
    | Valid(true) => result |> valid
    | ValidityBag(validity) when validity.valid => result |> valid
    | Valid(false)
    | ValidityBag(_) => result |> invalid
    };
  let component = ReasonReact.reducerComponent("Formality");
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit:
           (
             ~notifyOnSuccess: unit => unit,
             ~notifyOnFailure: unit => unit,
             Form.state
           ) =>
           unit,
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
            switch (Form.asyncStrategy, validator.validateAsync) {
            | (Some(AsyncStrategy.OnBlur), Some(_))
                when Js.Option.isNone(validator.dependents) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results: state.results |> ResultsMap.add(field, None)
                       }),
                   ~invalid=
                     result =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           state.results |> ResultsMap.add(field, Some(result)),
                         validating:
                           state.validating |> FieldsSet.remove(field),
                         emittedFields:
                           state.emittedFields |> FieldsSet.add(field)
                       })
                 )
            | (Some(AsyncStrategy.OnBlur), Some(_)) =>
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results: results |> ResultsMap.add(field, None),
                         emittedFields
                       }),
                   ~invalid=
                     result =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           results |> ResultsMap.add(field, Some(result)),
                         validating:
                           state.validating |> FieldsSet.remove(field),
                         emittedFields: emittedFields |> FieldsSet.add(field)
                       })
                 );
            | (None, Some(_)) when Js.Option.isNone(validator.dependents) =>
              let result = data |> validator.validate(Some(value));
              ReasonReact.UpdateWithSideEffects(
                {
                  ...state,
                  data,
                  results: state.results |> ResultsMap.add(field, Some(result)),
                  emittedFields: state.emittedFields |> FieldsSet.add(field)
                },
                ((_) => Log.noAsyncStrategyError())
              );
            | (None, Some(_)) =>
              let result = data |> validator.validate(Some(value));
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              ReasonReact.UpdateWithSideEffects(
                {
                  ...state,
                  data,
                  results: results |> ResultsMap.add(field, Some(result)),
                  emittedFields: emittedFields |> FieldsSet.add(field)
                },
                ((_) => Log.noAsyncStrategyError())
              );
            | (Some(AsyncStrategy.OnBlur), None)
            | (Some(AsyncStrategy.OnChange), None)
            | (None, None) =>
              switch validator.dependents {
              | Some(dependents) =>
                let result = data |> validator.validate(Some(value));
                let (results, emittedFields) =
                  dependents
                  |> validateDependents(
                       ~data,
                       ~results=state.results,
                       ~emittedFields=state.emittedFields
                     );
                ReasonReact.Update({
                  ...state,
                  data,
                  results: results |> ResultsMap.add(field, Some(result)),
                  emittedFields: emittedFields |> FieldsSet.add(field)
                });
              | None =>
                let result = data |> validator.validate(Some(value));
                ReasonReact.Update({
                  ...state,
                  data,
                  results: state.results |> ResultsMap.add(field, Some(result)),
                  emittedFields: state.emittedFields |> FieldsSet.add(field)
                });
              }
            | (Some(AsyncStrategy.OnChange), Some(validateAsync))
                when Js.Option.isNone(validator.dependents) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results:
                             state.results |> ResultsMap.add(field, None),
                           validating: state.validating |> FieldsSet.add(field),
                           emittedFields:
                             state.emittedFields |> FieldsSet.add(field)
                         },
                         validateFieldAsync(field, value, validateAsync)
                       ),
                   ~invalid=
                     result =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           state.results |> ResultsMap.add(field, Some(result)),
                         validating:
                           state.validating |> FieldsSet.remove(field),
                         emittedFields:
                           state.emittedFields |> FieldsSet.add(field)
                       })
                 )
            | (Some(AsyncStrategy.OnChange), Some(validateAsync)) =>
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results: results |> ResultsMap.add(field, None),
                           validating: state.validating |> FieldsSet.add(field),
                           emittedFields: emittedFields |> FieldsSet.add(field)
                         },
                         validateFieldAsync(field, value, validateAsync)
                       ),
                   ~invalid=
                     result =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results:
                           results |> ResultsMap.add(field, Some(result)),
                         validating:
                           state.validating |> FieldsSet.remove(field),
                         emittedFields: emittedFields |> FieldsSet.add(field)
                       })
                 );
            }
          | (
              Strategy.OnFirstSuccess | Strategy.OnFirstSuccessOrFirstBlur,
              false,
              false
            ) =>
            switch (Form.asyncStrategy, validator.validateAsync) {
            | (Some(AsyncStrategy.OnBlur), Some(_))
                when Js.Option.isNone(validator.dependents) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results: state.results |> ResultsMap.add(field, None)
                       }),
                   ~invalid=(_) => ReasonReact.Update({...state, data})
                 )
            | (Some(AsyncStrategy.OnBlur), Some(_)) =>
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results: results |> ResultsMap.add(field, None),
                         emittedFields
                       }),
                   ~invalid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results,
                         emittedFields
                       })
                 );
            | (None, Some(_)) when Js.Option.isNone(validator.dependents) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     result =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results:
                             state.results
                             |> ResultsMap.add(field, Some(result)),
                           emittedFields:
                             state.emittedFields |> FieldsSet.add(field)
                         },
                         (_) => Log.noAsyncStrategyError()
                       ),
                   ~invalid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {...state, data},
                         (_) => Log.noAsyncStrategyError()
                       )
                 )
            | (None, Some(_)) =>
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     result =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results:
                             results |> ResultsMap.add(field, Some(result)),
                           emittedFields: emittedFields |> FieldsSet.add(field)
                         },
                         (_) => Log.noAsyncStrategyError()
                       ),
                   ~invalid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {...state, data, results, emittedFields},
                         (_) => Log.noAsyncStrategyError()
                       )
                 );
            | (Some(AsyncStrategy.OnBlur), None)
            | (Some(AsyncStrategy.OnChange), None)
            | (None, None) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     result =>
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
                           results:
                             results |> ResultsMap.add(field, Some(result)),
                           emittedFields: emittedFields |> FieldsSet.add(field)
                         });
                       | None =>
                         ReasonReact.Update({
                           ...state,
                           data,
                           results:
                             state.results
                             |> ResultsMap.add(field, Some(result)),
                           emittedFields:
                             state.emittedFields |> FieldsSet.add(field)
                         })
                       },
                   ~invalid=
                     (_) =>
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
                           results,
                           emittedFields
                         });
                       | None => ReasonReact.Update({...state, data})
                       }
                 )
            | (Some(AsyncStrategy.OnChange), Some(validateAsync))
                when Js.Option.isNone(validator.dependents) =>
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results:
                             state.results |> ResultsMap.add(field, None),
                           validating: state.validating |> FieldsSet.add(field),
                           emittedFields:
                             state.emittedFields |> FieldsSet.add(field)
                         },
                         validateFieldAsync(field, value, validateAsync)
                       ),
                   ~invalid=(_) => ReasonReact.Update({...state, data})
                 )
            | (Some(AsyncStrategy.OnChange), Some(validateAsync)) =>
              let (results, emittedFields) =
                validator.dependents
                |> Js.Option.getExn
                |> validateDependents(
                     ~data,
                     ~results=state.results,
                     ~emittedFields=state.emittedFields
                   );
              data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           data,
                           results: results |> ResultsMap.add(field, None),
                           validating: state.validating |> FieldsSet.add(field),
                           emittedFields: emittedFields |> FieldsSet.add(field)
                         },
                         validateFieldAsync(field, value, validateAsync)
                       ),
                   ~invalid=
                     (_) =>
                       ReasonReact.Update({
                         ...state,
                         data,
                         results,
                         emittedFields
                       })
                 );
            }
          | (Strategy.OnFirstBlur | Strategy.OnSubmit, false, false) =>
            ReasonReact.Update({...state, data})
          };
        };
      | Blur((field, value)) =>
        let validator = field |> getValidator;
        let emitted = state.emittedFields |> FieldsSet.mem(field);
        switch (validator, emitted) {
        | (None, true)
        | (None, false) => ReasonReact.NoUpdate
        | (Some(validator), true) =>
          switch (Form.asyncStrategy, validator.validateAsync) {
          | (Some(AsyncStrategy.OnBlur), Some(validateAsync)) =>
            state.data
            |> validator.validate(Some(value))
            |> ifResult(
                 ~valid=
                   (_) =>
                     ReasonReact.UpdateWithSideEffects(
                       {
                         ...state,
                         results: state.results |> ResultsMap.add(field, None),
                         validating: state.validating |> FieldsSet.add(field),
                         emittedFields:
                           state.emittedFields |> FieldsSet.add(field)
                       },
                       validateFieldAsync(field, value, validateAsync)
                     ),
                 ~invalid=
                   result =>
                     ReasonReact.Update({
                       ...state,
                       results:
                         state.results |> ResultsMap.add(field, Some(result)),
                       validating: state.validating |> FieldsSet.remove(field)
                     })
               )
          | (None, Some(_)) =>
            ReasonReact.SideEffects(((_) => Log.noAsyncStrategyError()))
          | (Some(AsyncStrategy.OnChange), Some(_))
          | (Some(_), None)
          | (None, None) => ReasonReact.NoUpdate
          }
        | (Some(validator), false) =>
          switch (validator |> getStrategy) {
          | Strategy.OnFirstChange
          | Strategy.OnFirstSuccess
          | Strategy.OnSubmit => ReasonReact.NoUpdate
          | Strategy.OnFirstBlur
          | Strategy.OnFirstSuccessOrFirstBlur =>
            switch (Form.asyncStrategy, validator.validateAsync) {
            | (None, None)
            | (Some(_), None)
            | (Some(AsyncStrategy.OnChange), Some(_)) =>
              let result = state.data |> validator.validate(Some(value));
              ReasonReact.Update({
                ...state,
                results: state.results |> ResultsMap.add(field, Some(result)),
                emittedFields: state.emittedFields |> FieldsSet.add(field)
              });
            | (None, Some(_)) =>
              let result = state.data |> validator.validate(Some(value));
              ReasonReact.UpdateWithSideEffects(
                {
                  ...state,
                  results: state.results |> ResultsMap.add(field, Some(result)),
                  emittedFields: state.emittedFields |> FieldsSet.add(field)
                },
                ((_) => Log.noAsyncStrategyError())
              );
            | (Some(AsyncStrategy.OnBlur), Some(validateAsync)) =>
              state.data
              |> validator.validate(Some(value))
              |> ifResult(
                   ~valid=
                     (_) =>
                       ReasonReact.UpdateWithSideEffects(
                         {
                           ...state,
                           results:
                             state.results |> ResultsMap.add(field, None),
                           validating: state.validating |> FieldsSet.add(field),
                           emittedFields:
                             state.emittedFields |> FieldsSet.add(field)
                         },
                         validateFieldAsync(field, value, validateAsync)
                       ),
                   ~invalid=
                     result =>
                       ReasonReact.Update({
                         ...state,
                         results:
                           state.results |> ResultsMap.add(field, Some(result)),
                         validating:
                           state.validating |> FieldsSet.remove(field),
                         emittedFields:
                           state.emittedFields |> FieldsSet.add(field)
                       })
                 )
            }
          }
        };
      | ApplyAsyncResult(field, value, result) =>
        value === (state.data |> Form.get(field)) ?
          ReasonReact.Update({
            ...state,
            results: state.results |> ResultsMap.add(field, Some(result)),
            validating: state.validating |> FieldsSet.remove(field),
            emittedFields: state.emittedFields |> FieldsSet.add(field)
          }) :
          ReasonReact.NoUpdate
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
                 };
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
          ReasonReact.Update({
            ...state,
            results,
            submitting: false,
            submittedOnce: true
          });
      | Reset => ReasonReact.Update(initialState |> getInitialState)
      | HandleSubmissionError =>
        ReasonReact.Update({...state, submitting: false})
      },
    render: ({state, reduce}) =>
      children({
        state: state.data,
        results: field => state.results |> Results.get(field),
        validating: field => state.validating |> FieldsSet.mem(field),
        submitting: state.submitting,
        update: field =>
          reduce(event => Change((field, event |> Utils.formEventTargetValue))),
        blur: field =>
          reduce(event => Blur((field, event |> Utils.focusEventTargetValue))),
        submit:
          reduce(event => {
            if (! ReactEventRe.Form.defaultPrevented(event)) {
              event |> ReactEventRe.Form.preventDefault;
            };
            Submit;
          })
      })
  };
};
