open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, ~dep: Field.t, ~deps: list(Field.t), ~scheme: Scheme.t) => {
  let validate_dep = (dep: Field.t) => {
    let (dep_field, dep_validator) =
      switch (
        scheme
        |> List.find((entry: Scheme.entry) =>
             switch (entry, dep) {
             | (Field({name}), Field(dep)) => name == dep
             }
           )
      ) {
      | Field({name, validator}) => (Field.Field(name), validator)
      };

    let field_status_expr =
      dep_field |> E.ref_field(~of_="nextFieldsStatuses", ~loc);
    let validator_expr = dep_field |> E.field(~of_="validators", ~loc);
    let set_status_expr =
      dep_field
      |> E.update_ref_field(
           ~of_="nextFieldsStatuses",
           ~with_=[%expr status],
           ~loc,
         );

    switch (dep_validator) {
    | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
      switch%expr (
        validateFieldDependencyOnChange(
          ~input,
          ~fieldStatus=[%e field_status_expr],
          ~validator=[%e validator_expr],
          ~setStatus=[%e [%expr status => [%e set_status_expr]]],
        )
      ) {
      | Some(result) => nextFieldsStatuses := result
      | None => ()
      }
    | SyncValidator(Ok(Optional(None))) =>
      %expr
      ()
    // Should we trigger async validator of dependency?
    | AsyncValidator({mode: OnChange | OnBlur}) =>
      switch%expr (
        Async.validateFieldDependencyOnChange(
          ~input,
          ~fieldStatus=[%e field_status_expr],
          ~validator=[%e validator_expr],
          ~setStatus=[%e [%expr status => [%e set_status_expr]]],
        )
      ) {
      | Some(result) => nextFieldsStatuses := result
      | None => ()
      }
    };
  };

  deps |> E.seq(~exp=dep |> validate_dep, ~make=validate_dep);
};
