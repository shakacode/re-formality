open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~field: Scheme.field,
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
          Async.validateFieldOnChangeInOnChangeMode(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(OptionType) =>
        %expr
        {
          Async.validateFieldOfOptionTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(StringType) =>
        %expr
        {
          Async.validateFieldOfStringTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | Some(OptionStringType) =>
        %expr
        {
          Async.validateFieldOfOptionStringTypeOnChangeInOnChangeMode(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      };
    switch ([%e field.name |> E.field(~in_="nextFieldsStatuses", ~loc)]) {
    | Validating(value) =>
      UpdateWithSideEffects(
        {...state, input: nextInput, fieldsStatuses: nextFieldsStatuses},
        ({state: _, dispatch}) => {
          %e
          E.apply_field2(
            ~in_=("validators", field.name),
            ~fn="validateAsync",
            ~args=[(Nolabel, [%expr (value, dispatch)])],
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
