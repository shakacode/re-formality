open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~field: Scheme.field,
      ~kind: [ | `Field | `FieldOfCollection],
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => {
  %expr
  {
    let result =
      switch%e (optionality) {
      | None =>
        switch (kind) {
        | `Field =>
          %expr
          {
            Async.validateFieldOnBlur(
              ~input=state.input,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOnBlur(
              ~input=state.input,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        }
      | Some(OptionType) =>
        switch (kind) {
        | `Field =>
          %expr
          {
            Async.validateFieldOfOptionTypeOnBlur(
              ~input=state.input,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfOptionTypeOnBlur(
              ~input=state.input,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        }
      | Some(StringType) =>
        switch (kind) {
        | `Field =>
          %expr
          {
            Async.validateFieldOfStringTypeOnBlur(
              ~input=state.input,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfStringTypeOnBlur(
              ~input=state.input,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        }
      | Some(OptionStringType) =>
        switch (kind) {
        | `Field =>
          %expr
          {
            Async.validateFieldOfOptionStringTypeOnBlur(
              ~input=state.input,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfOptionStringTypeOnBlur(
              ~input=state.input,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        }
      };

    switch (result) {
    | None => NoUpdate
    | Some(fieldsStatuses) =>
      switch ([%e field.name |> E.field(~in_="fieldsStatuses", ~loc)]) {
      | Validating(value) =>
        UpdateWithSideEffects(
          {...state, fieldsStatuses},
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
      | Dirty(_, Shown | Hidden) => Update({...state, fieldsStatuses})
      }
    };
  };
};
