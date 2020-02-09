open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~field: Field.t,
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
            ~input,
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
            ~input,
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
            ~input,
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
            ~input,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      };
    switch ([%e field |> E.field(~of_="nextFieldsStatuses", ~loc)]) {
    | Validating(value) =>
      UpdateWithSideEffects(
        {...state, input, fieldsStatuses: nextFieldsStatuses},
        ({dispatch}) => {
          let validator = [%e field |> E.field(~of_="validators", ~loc)];
          validator.validateAsync((value, dispatch));
        },
      )
    | Pristine
    | Dirty(_, Shown | Hidden) =>
      Update({...state, input, fieldsStatuses: nextFieldsStatuses})
    };
  };
};
