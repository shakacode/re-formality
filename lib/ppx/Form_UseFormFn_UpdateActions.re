open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) =>
  scheme
  |> List.fold_left(
       (acc, entry: Scheme.entry) =>
         switch (entry) {
         | Field(field) => [
             Exp.case(
               Pat.construct(
                 ~attrs=[explicit_arity(~loc)],
                 Lident(FieldPrinter.update_action(~field=field.name))
                 |> lid(~loc),
                 Some(Pat.tuple([Pat.var("nextInputFn" |> str(~loc))])),
               ),
               switch (field.deps) {
               | [] =>
                 let field_status_expr =
                   field.name
                   |> E.field2(~in_=("state", "fieldsStatuses"), ~loc);
                 let field_input_expr =
                   field.name |> E.field(~in_="nextInput", ~loc);
                 let validator_expr =
                   field.name |> E.field(~in_="validators", ~loc);
                 let set_status_expr =
                   field.name
                   |> E.update_field2(
                        ~in_=("state", "fieldsStatuses"),
                        ~with_=[%expr status],
                        ~loc,
                      );

                 %expr
                 {
                   let nextInput = nextInputFn(state.input);

                   switch%e (field.validator) {
                   | SyncValidator(validator) =>
                     Form_UseFormFn_UpdateActions_SyncField.ast(
                       ~loc,
                       ~kind=`Field,
                       ~validator,
                       ~field_status_expr,
                       ~field_input_expr,
                       ~validator_expr,
                       ~set_status_expr,
                     )
                   | AsyncValidator({mode: OnBlur, optionality}) =>
                     Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast(
                       ~loc,
                       ~kind=`Field,
                       ~optionality,
                       ~field_status_expr,
                       ~validator_expr,
                       ~set_status_expr,
                     )
                   | AsyncValidator({mode: OnChange, optionality}) =>
                     Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast(
                       ~loc,
                       ~field,
                       ~kind=`Field,
                       ~optionality,
                       ~field_status_expr,
                       ~validator_expr,
                       ~set_status_expr,
                     )
                   };
                 };

               | [dep, ...deps] =>
                 %expr
                 {
                   let nextInput = nextInputFn(state.input);
                   let nextFieldsStatuses = ref(state.fieldsStatuses);

                   %e
                   {
                     scheme
                     |> Form_UseFormFn_DependentFields.ast(
                          ~loc,
                          ~dep,
                          ~deps,
                          ~trigger=`Field(field.name),
                        );
                   };

                   %e
                   {
                     let field_status_expr =
                       field.name
                       |> E.ref_field(~in_="nextFieldsStatuses", ~loc);
                     let field_input_expr =
                       field.name |> E.field(~in_="nextInput", ~loc);
                     let validator_expr =
                       field.name |> E.field(~in_="validators", ~loc);
                     let set_status_expr =
                       field.name
                       |> E.update_ref_field(
                            ~in_="nextFieldsStatuses",
                            ~with_=[%expr status],
                            ~loc,
                          );

                     switch (field.validator) {
                     | SyncValidator(validator) =>
                       Form_UseFormFn_UpdateActions_SyncField.ast(
                         ~loc,
                         ~kind=`Field,
                         ~validator,
                         ~field_status_expr,
                         ~field_input_expr,
                         ~validator_expr,
                         ~set_status_expr,
                       )
                     | AsyncValidator({mode: OnBlur, optionality}) =>
                       Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast(
                         ~loc,
                         ~kind=`Field,
                         ~optionality,
                         ~field_status_expr,
                         ~validator_expr,
                         ~set_status_expr,
                       )
                     | AsyncValidator({mode: OnChange, optionality}) =>
                       Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast(
                         ~loc,
                         ~field,
                         ~kind=`Field,
                         ~optionality,
                         ~field_status_expr,
                         ~validator_expr,
                         ~set_status_expr,
                       )
                     };
                   };
                 }
               },
             ),
             ...acc,
           ]
         | Collection({collection, fields}) =>
           fields
           |> List.fold_left(
                (acc, field: Scheme.field) =>
                  [
                    Exp.case(
                      Pat.construct(
                        ~attrs=[explicit_arity(~loc)],
                        Lident(
                          FieldOfCollectionPrinter.update_action(
                            ~collection,
                            ~field=field.name,
                          ),
                        )
                        |> lid(~loc),
                        Some(
                          Pat.tuple([
                            Pat.var("nextInputFn" |> str(~loc)),
                            Pat.var("index" |> str(~loc)),
                          ]),
                        ),
                      ),
                      switch (field.deps) {
                      | [] =>
                        let field_status_expr =
                          field.name
                          |> E.field_of_collection2(
                               ~in_=("state", "fieldsStatuses"),
                               ~collection,
                               ~loc,
                             );
                        let field_input_expr =
                          field.name
                          |> E.field_of_collection(
                               ~in_="nextInput",
                               ~collection,
                               ~loc,
                             );
                        let validator_expr =
                          field.name
                          |> E.field_of_collection_validator(
                               ~validators="validators",
                               ~collection,
                               ~loc,
                             );
                        let set_status_expr =
                          field.name
                          |> E.update_field_of_collection2(
                               ~in_=("state", "fieldsStatuses"),
                               ~collection,
                               ~with_=[%expr status],
                               ~loc,
                             );

                        %expr
                        {
                          let nextInput = nextInputFn(state.input);

                          switch%e (field.validator) {
                          | SyncValidator(validator) =>
                            Form_UseFormFn_UpdateActions_SyncField.ast(
                              ~loc,
                              ~kind=`FieldOfCollection,
                              ~validator,
                              ~field_status_expr,
                              ~field_input_expr,
                              ~validator_expr,
                              ~set_status_expr,
                            )
                          | AsyncValidator({mode: OnBlur, optionality}) =>
                            Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast(
                              ~loc,
                              ~kind=`FieldOfCollection,
                              ~optionality,
                              ~field_status_expr,
                              ~validator_expr,
                              ~set_status_expr,
                            )
                          | AsyncValidator({mode: OnChange, optionality}) =>
                            Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast(
                              ~loc,
                              ~field,
                              ~kind=`FieldOfCollection,
                              ~optionality,
                              ~field_status_expr,
                              ~validator_expr,
                              ~set_status_expr,
                            )
                          };
                        };

                      | [dep, ...deps] =>
                        %expr
                        {
                          let nextInput = nextInputFn(state.input);
                          let nextFieldsStatuses = ref(state.fieldsStatuses);

                          %e
                          {
                            scheme
                            |> Form_UseFormFn_DependentFields.ast(
                                 ~loc,
                                 ~dep,
                                 ~deps,
                                 ~trigger=
                                   `FieldOfCollection((
                                     collection,
                                     field.name,
                                   )),
                               );
                          };

                          %e
                          {
                            let field_status_expr =
                              field.name
                              |> E.ref_field_of_collection(
                                   ~in_="nextFieldsStatuses",
                                   ~collection,
                                   ~loc,
                                 );
                            let field_input_expr =
                              field.name
                              |> E.field_of_collection(
                                   ~in_="nextInput",
                                   ~collection,
                                   ~loc,
                                 );
                            let validator_expr =
                              field.name
                              |> E.field_of_collection_validator(
                                   ~validators="validators",
                                   ~collection,
                                   ~loc,
                                 );
                            let set_status_expr =
                              field.name
                              |> E.update_ref_field_of_collection(
                                   ~in_="nextFieldsStatuses",
                                   ~collection,
                                   ~with_=[%expr status],
                                   ~loc,
                                 );

                            switch (field.validator) {
                            | SyncValidator(validator) =>
                              Form_UseFormFn_UpdateActions_SyncField.ast(
                                ~loc,
                                ~kind=`FieldOfCollection,
                                ~validator,
                                ~field_status_expr,
                                ~field_input_expr,
                                ~validator_expr,
                                ~set_status_expr,
                              )
                            | AsyncValidator({mode: OnBlur, optionality}) =>
                              Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast(
                                ~loc,
                                ~kind=`FieldOfCollection,
                                ~optionality,
                                ~field_status_expr,
                                ~validator_expr,
                                ~set_status_expr,
                              )
                            | AsyncValidator({mode: OnChange, optionality}) =>
                              Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast(
                                ~loc,
                                ~field,
                                ~kind=`FieldOfCollection,
                                ~optionality,
                                ~field_status_expr,
                                ~validator_expr,
                                ~set_status_expr,
                              )
                            };
                          };
                        }
                      },
                    ),
                    ...acc,
                  ],
                acc,
              )
         },
       [],
     );
