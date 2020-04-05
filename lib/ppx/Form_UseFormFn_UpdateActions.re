open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

type trigger = [
         | `Field(string)
         | `Collection(Collection.t)
         | `FieldOfCollection(Collection.t, string)
       ];

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
                     %expr
                     Update({
                       ...state,
                       input: nextInput,
                       fieldsStatuses:
                         switch%e (validator) {
                         | Ok(Required | Optional(Some(_)))
                         | Error () =>
                           %expr
                           {
                             validateFieldOnChangeWithValidator(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Ok(Optional(None)) =>
                           %expr
                           validateFieldOnChangeWithoutValidator(
                             ~fieldInput=[%e field_input_expr],
                             ~setStatus=[%e
                               [%expr status => [%e set_status_expr]]
                             ],
                           )
                         },
                     })
                   | AsyncValidator({mode: OnBlur, optionality}) =>
                     %expr
                     Update({
                       ...state,
                       input: nextInput,
                       fieldsStatuses:
                         switch%e (optionality) {
                         | None =>
                           %expr
                           {
                             Async.validateFieldOnChangeInOnBlurMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(OptionType) =>
                           %expr
                           {
                             Async.validateFieldOfOptionTypeOnChangeInOnBlurMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(StringType) =>
                           %expr
                           {
                             Async.validateFieldOfStringTypeOnChangeInOnBlurMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(OptionStringType) =>
                           %expr
                           {
                             Async.validateFieldOfOptionStringTypeOnChangeInOnBlurMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         },
                     })
                   | AsyncValidator({mode: OnChange, optionality}) =>
                     %expr
                     {
                       let nextFieldsStatuses =
                         switch%e (optionality) {
                         | None =>
                           %expr
                           {
                             Async.validateFieldOnChangeInOnChangeMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(OptionType) =>
                           %expr
                           {
                             Async.validateFieldOfOptionTypeOnChangeInOnChangeMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(StringType) =>
                           %expr
                           {
                             Async.validateFieldOfStringTypeOnChangeInOnChangeMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         | Some(OptionStringType) =>
                           %expr
                           {
                             Async.validateFieldOfOptionStringTypeOnChangeInOnChangeMode(
                               ~input=nextInput,
                               ~fieldStatus=[%e field_status_expr],
                               ~submissionStatus=state.submissionStatus,
                               ~validator=[%e validator_expr],
                               ~setStatus=[%e
                                 [%expr status => [%e set_status_expr]]
                               ],
                             );
                           }
                         };
                       switch (
                         [%e
                           field.name
                           |> E.field(~in_="nextFieldsStatuses", ~loc)
                         ]
                       ) {
                       | Validating(value) =>
                         UpdateWithSideEffects(
                           {
                             ...state,
                             input: nextInput,
                             fieldsStatuses: nextFieldsStatuses,
                           },
                           ({state: _, dispatch}) => {
                             %e
                             E.apply_field2(
                               ~in_=("validators", field.name),
                               ~fn="validateAsync",
                               ~args=[(Nolabel, [%expr (value, dispatch)])],
                               ~loc,
                             )
                           },
                         )
                       | Pristine
                       | Dirty(_, Shown | Hidden) =>
                         Update({
                           ...state,
                           input: nextInput,
                           fieldsStatuses: nextFieldsStatuses,
                         })
                       };
                     }
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
                     |> {
                       let trigger: trigger = `Field(field.name);
                       let validate_dep = (dep: FieldDep.t) => {
                         switch (
                           scheme
                           |> List.fold_left(
                                (res, entry: Scheme.entry) =>
                                  switch (res, entry, dep) {
                                  | (Some(_), _, _) => res
                                  | (None, Field(field), DepField(dep)) =>
                                    field.name == dep
                                      ? Some(`DepField(field)) : None
                                  | (
                                      None,
                                      Collection({collection, fields}),
                                      DepFieldOfCollection({
                                        collection: dep_collection,
                                        field: dep_field,
                                      }),
                                    ) =>
                                    if (collection.plural
                                        != dep_collection.plural) {
                                      None;
                                    } else {
                                      Some(
                                        `DepFieldOfCollection((
                                          collection,
                                          fields
                                          |> List.find((field: Scheme.field) =>
                                               field.name == dep_field
                                             ),
                                        )),
                                      );
                                    }
                                  | (None, Collection(_), DepField(_))
                                  | (None, Field(_), DepFieldOfCollection(_)) => res
                                  },
                                None,
                              )
                         ) {
                         | None =>
                           failwith(
                             "Dep is not found in scheme. Please, file an issue with your use-case.",
                           )
                         | Some(`DepField(field)) =>
                           let field_status_expr =
                             field.name
                             |> E.ref_field(~in_="nextFieldsStatuses", ~loc);
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
                           | SyncValidator(
                               Ok(Required | Optional(Some(_))) | Error (),
                             ) =>
                             switch%expr (
                               validateDependentFieldOnChange(
                                 ~input=nextInput,
                                 ~fieldStatus=[%e field_status_expr],
                                 ~validator=[%e validator_expr],
                                 ~setStatus=[%e
                                   [%expr status => [%e set_status_expr]]
                                 ],
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
                               Async.validateDependentFieldOnChange(
                                 ~input=nextInput,
                                 ~fieldStatus=[%e field_status_expr],
                                 ~validator=[%e validator_expr],
                                 ~setStatus=[%e
                                   [%expr status => [%e set_status_expr]]
                                 ],
                               )
                             ) {
                             | Some(result) => nextFieldsStatuses := result
                             | None => ()
                             }
                           };
                         | Some(`DepFieldOfCollection(collection, field)) =>
                           let collection_statuses_expr =
                             collection.plural
                             |> E.ref_field(~in_="nextFieldsStatuses", ~loc);
                           let field_status_expr =
                             field.name |> E.field(~in_="item", ~loc);
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
                                  ~index_token="index'",
                                  ~loc,
                                );

                           switch (trigger) {
                           | `FieldOfCollection(collection', field')
                               when
                                 collection.plural == collection'.plural
                                 && field.name == field' =>
                             switch (field.validator) {
                             | SyncValidator(
                                 Ok(Required | Optional(Some(_))) | Error (),
                               ) =>
                               %expr
                               {
                                 Belt.Array.forEachWithIndex(
                                   [%e collection_statuses_expr],
                                   (index', item) =>
                                   if (index != index') {
                                     switch (
                                       validateDependentFieldOfCollectionOnChange(
                                         ~input=nextInput,
                                         ~index=index',
                                         ~fieldStatus=[%e field_status_expr],
                                         ~validator=[%e validator_expr],
                                         ~setStatus=[%e
                                           [%expr
                                             status => [%e set_status_expr]
                                           ]
                                         ],
                                       )
                                     ) {
                                     | Some(result) =>
                                       nextFieldsStatuses := result
                                     | None => ()
                                     };
                                   } else {
                                     ();
                                   }
                                 );
                               }

                             | SyncValidator(Ok(Optional(None))) =>
                               %expr
                               ()
                             // Should we trigger async validator of dependency?
                             | AsyncValidator({mode: OnChange | OnBlur}) =>
                               %expr
                               {
                                 Belt.Array.forEachWithIndex(
                                   [%e collection_statuses_expr],
                                   (index', item) =>
                                   if (index != index') {
                                     switch (
                                       Async.validateDependentFieldOfCollectionOnChange(
                                         ~input=nextInput,
                                         ~index=index',
                                         ~fieldStatus=[%e field_status_expr],
                                         ~validator=[%e validator_expr],
                                         ~setStatus=[%e
                                           [%expr
                                             status => [%e set_status_expr]
                                           ]
                                         ],
                                       )
                                     ) {
                                     | Some(result) =>
                                       nextFieldsStatuses := result
                                     | None => ()
                                     };
                                   } else {
                                     ();
                                   }
                                 );
                               }
                             }
                           | `Field(_)
                           | `Collection(_)
                           | `FieldOfCollection(_, _) =>
                             switch (field.validator) {
                             | SyncValidator(
                                 Ok(Required | Optional(Some(_))) | Error (),
                               ) =>
                               %expr
                               {
                                 Belt.Array.forEachWithIndex(
                                   [%e collection_statuses_expr],
                                   (index', item) =>
                                   switch (
                                     validateDependentFieldOfCollectionOnChange(
                                       ~input=nextInput,
                                       ~index=index',
                                       ~fieldStatus=[%e field_status_expr],
                                       ~validator=[%e validator_expr],
                                       ~setStatus=[%e
                                         [%expr status => [%e set_status_expr]]
                                       ],
                                     )
                                   ) {
                                   | Some(result) =>
                                     nextFieldsStatuses := result
                                   | None => ()
                                   }
                                 );
                               }
                             | SyncValidator(Ok(Optional(None))) =>
                               %expr
                               ()
                             // Should we trigger async validator of dependency?
                             | AsyncValidator({mode: OnChange | OnBlur}) =>
                               %expr
                               {
                                 Belt.Array.forEachWithIndex(
                                   [%e collection_statuses_expr],
                                   (index', item) =>
                                   switch (
                                     Async.validateDependentFieldOfCollectionOnChange(
                                       ~input=nextInput,
                                       ~index=index',
                                       ~fieldStatus=[%e field_status_expr],
                                       ~validator=[%e validator_expr],
                                       ~setStatus=[%e
                                         [%expr status => [%e set_status_expr]]
                                       ],
                                     )
                                   ) {
                                   | Some(result) =>
                                     nextFieldsStatuses := result
                                   | None => ()
                                   }
                                 );
                               }
                             }
                           };
                         };
                       };

                       deps
                       |> E.seq(~exp=dep |> validate_dep, ~make=validate_dep);
                     };
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
                         ~validator,
                         ~field_status_expr,
                         ~field_input_expr,
                         ~validator_expr,
                         ~set_status_expr,
                       )
                     | AsyncValidator({mode: OnBlur, optionality}) =>
                       Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast(
                         ~loc,
                         ~optionality,
                         ~field_status_expr,
                         ~validator_expr,
                         ~set_status_expr,
                       )
                     | AsyncValidator({mode: OnChange, optionality}) =>
                       Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast(
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
                            Form_UseFormFn_UpdateActions_SyncFieldOfCollection.ast(
                              ~loc,
                              ~validator,
                              ~field_status_expr,
                              ~field_input_expr,
                              ~validator_expr,
                              ~set_status_expr,
                            )
                          | AsyncValidator({mode: OnBlur, optionality}) =>
                            Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnBlurMode.ast(
                              ~loc,
                              ~optionality,
                              ~field_status_expr,
                              ~validator_expr,
                              ~set_status_expr,
                            )
                          | AsyncValidator({mode: OnChange, optionality}) =>
                            Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnChangeMode.ast(
                              ~loc,
                              ~field,
                              ~collection,
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
                              Form_UseFormFn_UpdateActions_SyncFieldOfCollection.ast(
                                ~loc,
                                ~validator,
                                ~field_status_expr,
                                ~field_input_expr,
                                ~validator_expr,
                                ~set_status_expr,
                              )
                            | AsyncValidator({mode: OnBlur, optionality}) =>
                              Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnBlurMode.ast(
                                ~loc,
                                ~optionality,
                                ~field_status_expr,
                                ~validator_expr,
                                ~set_status_expr,
                              )
                            | AsyncValidator({mode: OnChange, optionality}) =>
                              Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnChangeMode.ast(
                                ~loc,
                                ~field,
                                ~collection,
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
