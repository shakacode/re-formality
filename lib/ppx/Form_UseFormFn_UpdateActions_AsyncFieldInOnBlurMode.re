open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~kind: [ | `Field | `FieldOfCollection],
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => [%expr
  Update({
    ...state,
    input: nextInput,
    fieldsStatuses:
      switch%e (optionality) {
      | None =>
        switch (kind) {
        | `Field =>
          %expr
          {
            Async.validateFieldOnChangeInOnBlurMode(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOnChangeInOnBlurMode(
              ~input=nextInput,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
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
            Async.validateFieldOfOptionTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfOptionTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
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
            Async.validateFieldOfStringTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfStringTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
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
            Async.validateFieldOfOptionStringTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        | `FieldOfCollection =>
          %expr
          {
            Async.validateFieldOfCollectionOfOptionStringTypeOnChangeInOnBlurMode(
              ~input=nextInput,
              ~index,
              ~fieldStatus=[%e field_status_expr],
              ~submissionStatus=state.submissionStatus,
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            );
          }
        }
      },
  })
];
