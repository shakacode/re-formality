open Meta;

open Ppxlib;

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
          Async.validateFieldOnChangeInOnBlurMode(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (None, Some(OptionType)) =>
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
      | (None, Some(StringType)) =>
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
      | (None, Some(OptionStringType)) =>
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
      | (Some (), None) =>
        %expr
        {
          Async.validateFieldOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
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
          Async.validateFieldOfOptionTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
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
          Async.validateFieldOfStringTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
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
          Async.validateFieldOfOptionStringTypeOnChangeInOnBlurModeWithMetadata(
            ~input=nextInput,
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
