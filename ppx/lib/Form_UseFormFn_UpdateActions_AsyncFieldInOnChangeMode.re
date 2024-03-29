open Meta;
open AstHelpers;

open Ppxlib;

let ast =
    (
      ~loc,
      ~field: Scheme.field,
      ~metadata: option(unit),
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => {
  %expr
  {
    let nextFieldsStatuses =
      switch%e (metadata, optionality) {
      | (None, None) =>
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
      | (None, Some(OptionType)) =>
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
      | (None, Some(StringType)) =>
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
      | (None, Some(OptionStringType)) =>
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
      | (Some (), None) =>
        %expr
        {
          Async.validateFieldOnChangeInOnChangeModeWithMetadata(
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
          Async.validateFieldOfOptionTypeOnChangeInOnChangeModeWithMetadata(
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
          Async.validateFieldOfStringTypeOnChangeInOnChangeModeWithMetadata(
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
          Async.validateFieldOfOptionStringTypeOnChangeInOnChangeModeWithMetadata(
            ~input=nextInput,
            ~fieldStatus=[%e field_status_expr],
            ~submissionStatus=state.submissionStatus,
            ~validator=[%e validator_expr],
            ~metadata,
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
            ~args=[
              (
                Nolabel,
                switch (metadata) {
                | None =>
                  %expr
                  (value, dispatch)
                | Some () =>
                  %expr
                  (value, metadata, dispatch)
                },
              ),
            ],
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
