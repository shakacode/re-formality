open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~metadata: option(unit),
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => [%expr
  Update({
    ...state,
    input: nextInput,
    fieldsStatuses:
      switch%e (metadata, optionality) {
      | (None, None) =>
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
      | (None, Some(OptionType)) =>
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
      | (None, Some(StringType)) =>
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
      | (None, Some(OptionStringType)) =>
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
      | (Some (), None) =>
        %expr
        {
          Async.validateFieldOfCollectionOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(OptionType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(StringType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfStringTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(OptionStringType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionStringTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      },
  })
];
