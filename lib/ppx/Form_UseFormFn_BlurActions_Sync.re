open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~validator: result(FieldValidator.sync, unit),
      ~field_status_expr: expression,
      ~field_input_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => {
  %expr
  {
    let result =
      switch%e (validator) {
      | Ok(Required | Optional(Some(_)))
      | Error () =>
        %expr
        validateFieldOnBlurWithValidator(
          ~input=state.input,
          ~fieldStatus=[%e field_status_expr],
          ~validator=[%e validator_expr],
          ~setStatus=[%e [%expr status => [%e set_status_expr]]],
        )
      | Ok(Optional(None)) =>
        %expr
        validateFieldOnBlurWithoutValidator(
          ~fieldInput=[%e field_input_expr],
          ~fieldStatus=[%e field_status_expr],
          ~setStatus=[%e [%expr status => [%e set_status_expr]]],
        )
      };

    switch (result) {
    | Some(fieldsStatuses) => Update({...state, fieldsStatuses})
    | None => NoUpdate
    };
  };
};
