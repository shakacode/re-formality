open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) =>
  scheme
  |> List.map((entry: Scheme.entry) =>
       switch (entry) {
       | Field({name, validator}) =>
         let field = Field.Field(name);
         Exp.case(
           Pat.construct(
             Lident(field |> Field.blur_action) |> lid(~loc),
             None,
           ),
           {
             let field_status_expr =
               field |> E.field2(~of_=("state", "fieldsStatuses"), ~loc);
             let field_input_expr =
               field |> E.field2(~of_=("state", "input"), ~loc);
             let validator_expr = field |> E.field(~of_="validators", ~loc);
             let set_status_expr =
               field
               |> E.update_field2(
                    ~of_=("state", "fieldsStatuses"),
                    ~with_=[%expr status],
                    ~loc,
                  );

             switch (validator) {
             | SyncValidator(validator) =>
               Form_UseFormFn_BlurActions_Sync.ast(
                 ~loc,
                 ~validator,
                 ~field_status_expr,
                 ~field_input_expr,
                 ~validator_expr,
                 ~set_status_expr,
               )
             | AsyncValidator({optionality}) =>
               Form_UseFormFn_BlurActions_Async.ast(
                 ~loc,
                 ~field,
                 ~optionality,
                 ~field_status_expr,
                 ~validator_expr,
                 ~set_status_expr,
               )
             };
           },
         );
       }
     );
