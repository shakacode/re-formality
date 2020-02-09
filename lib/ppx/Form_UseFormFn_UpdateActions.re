open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) =>
  scheme
  |> List.map((entry: Scheme.entry) =>
       switch (entry) {
       | Field({name, deps, validator}) =>
         let field = Field.Field(name);

         Exp.case(
           Pat.construct(
             ~attrs=[explicit_arity(~loc)],
             Lident(field |> Field.update_action) |> lid(~loc),
             Some(Pat.tuple([Pat.var("input" |> str(~loc))])),
           ),
           switch (deps) {
           | [] =>
             let field_status_expr =
               field |> E.field2(~of_=("state", "fieldsStatuses"), ~loc);
             let field_input_expr = field |> E.field(~of_="input", ~loc);
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
               Form_UseFormFn_UpdateActions_Sync.ast(
                 ~loc,
                 ~validator,
                 ~field_status_expr,
                 ~field_input_expr,
                 ~validator_expr,
                 ~set_status_expr,
               )
             | AsyncValidator({mode: OnBlur, optionality}) =>
               Form_UseFormFn_UpdateActions_AsyncOnBlurMode.ast(
                 ~loc,
                 ~optionality,
                 ~field_status_expr,
                 ~validator_expr,
                 ~set_status_expr,
               )
             | AsyncValidator({mode: OnChange, optionality}) =>
               Form_UseFormFn_UpdateActions_AsyncOnChangeMode.ast(
                 ~loc,
                 ~field,
                 ~optionality,
                 ~field_status_expr,
                 ~validator_expr,
                 ~set_status_expr,
               )
             };

           | [dep, ...deps] =>
             %expr
             {
               let nextFieldsStatuses = ref(state.fieldsStatuses);

               %e
               {
                 Form_UseFormFn_UpdateActions_Deps.ast(
                   ~loc,
                   ~dep,
                   ~deps,
                   ~scheme,
                 );
               };

               %e
               {
                 let field_status_expr =
                   field |> E.ref_field(~of_="nextFieldsStatuses", ~loc);
                 let field_input_expr = field |> E.field(~of_="input", ~loc);
                 let validator_expr =
                   field |> E.field(~of_="validators", ~loc);
                 let set_status_expr =
                   field
                   |> E.update_ref_field(
                        ~of_="nextFieldsStatuses",
                        ~with_=[%expr status],
                        ~loc,
                      );

                 switch (validator) {
                 | SyncValidator(validator) =>
                   Form_UseFormFn_UpdateActions_Sync.ast(
                     ~loc,
                     ~validator,
                     ~field_status_expr,
                     ~field_input_expr,
                     ~validator_expr,
                     ~set_status_expr,
                   )
                 | AsyncValidator({mode: OnBlur, optionality}) =>
                   Form_UseFormFn_UpdateActions_AsyncOnBlurMode.ast(
                     ~loc,
                     ~optionality,
                     ~field_status_expr,
                     ~validator_expr,
                     ~set_status_expr,
                   )
                 | AsyncValidator({mode: OnChange, optionality}) =>
                   Form_UseFormFn_UpdateActions_AsyncOnChangeMode.ast(
                     ~loc,
                     ~field,
                     ~optionality,
                     ~field_status_expr,
                     ~validator_expr,
                     ~set_status_expr,
                   )
                 };
               };
             }
           },
         );
       }
     );
