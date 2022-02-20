open Meta;
open AstHelpers;

open Ppxlib;

let ast =
    (
      ~loc,
      ~field: Scheme.field,
      ~collection: Collection.t,
      ~metadata: option(unit),
      ~optionality: option(FieldOptionality.t),
      ~field_status_expr: expression,
      ~validator_expr: expression,
      ~set_status_expr: expression,
    ) => {
  %expr
  {
    let result =
      switch%e (metadata, optionality) {
      | (None, None) =>
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
      | (None, Some(OptionType)) =>
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
      | (None, Some(StringType)) =>
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
      | (None, Some(OptionStringType)) =>
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
      | (Some (), None) =>
        %expr
        {
          Async.validateFieldOfCollectionOnBlurWithMetadata(
            ~input=state.input,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(OptionType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionTypeOnBlurWithMetadata(
            ~input=state.input,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(StringType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfStringTypeOnBlurWithMetadata(
            ~input=state.input,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      | (Some (), Some(OptionStringType)) =>
        %expr
        {
          Async.validateFieldOfCollectionOfOptionStringTypeOnBlurWithMetadata(
            ~input=state.input,
            ~index,
            ~fieldStatus=[%e field_status_expr],
            ~validator=[%e validator_expr],
            ~metadata,
            ~setStatus=[%e [%expr status => [%e set_status_expr]]],
          );
        }
      };

    switch (result) {
    | None => NoUpdate
    | Some(fieldsStatuses) =>
      switch (
        [%e
          field.name
          |> E.field_of_collection(~in_="fieldsStatuses", ~collection, ~loc)
        ]
      ) {
      | Validating(value) =>
        UpdateWithSideEffects(
          {...state, fieldsStatuses},
          ({state: _, dispatch}) => {
            %e
            E.apply_field4(
              ~in_=("validators", collection.plural, "fields", field.name),
              ~fn="validateAsync",
              ~args=[
                (
                  Nolabel,
                  switch (metadata) {
                  | None =>
                    %expr
                    (value, index, dispatch)
                  | Some () =>
                    %expr
                    (value, index, metadata, dispatch)
                  },
                ),
              ],
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
