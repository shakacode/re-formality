open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~field: Scheme.field,
      ~collection: Collection.t,
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => {
  %expr
  {
    let nextFieldsStatuses =
      switch%e (optionality) {
      | None =>
        %expr
        {
          Async.validateFieldOfCollectionOnChangeInOnChangeMode(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(OptionType) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(StringType) =>
        %expr
        {
          Async.validateFieldOfCollectionOfStringTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(OptionStringType) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionStringTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      };
    switch (
      [%e
        field.name
        |> E.field_of_collection(~in_="nextFieldsStatuses", ~collection, ~loc)
      ]
    ) {
    | Validating(value) =>
      UpdateWithSideEffects(
        {...state, input: nextInput, fieldsStatuses: nextFieldsStatuses},
        ({state: _, dispatch}) => {
          %e
          E.apply_field4(
            ~in_=("validators", collection.plural, "fields", field.name),
            ~fn="validateAsync",
            ~args=[(Nolabel, [%expr (value, index, dispatch)])],
            ~loc,
          )
        },
      )
    | Pristine
    | Dirty(_, Shown | Hidden) =>
      Update({...state, input: nextInput, fieldsStatuses: nextFieldsStatuses})
    };
  };
};
