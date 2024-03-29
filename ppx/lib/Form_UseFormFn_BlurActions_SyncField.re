open Meta;

open Ppxlib;

let ast =
    (
      ~loc,
      ~validator: result(FieldValidator.sync, unit),
      ~metadata: option(unit),
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
        switch (metadata) {
        | None =>
          %expr
          validateFieldOnBlurWithValidator(
            ~input=state.input,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          )
        | Some () =>
          %expr
          validateFieldOnBlurWithValidatorAndMetadata(
            ~input=state.input,
            ~metadata,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          )
        }

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
