open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  let collections = scheme |> Scheme.collections;
  collections
  |> List.fold_left(
       (acc, {collection, validator, fields}: Scheme.collection) => {
         let deps =
           fields
           |> List.fold_left(
                (acc, field: Scheme.field) => acc |> List.append(field.deps),
                [],
              );

         let add_action_pat =
           Pat.construct(
             ~attrs=[explicit_arity(~loc)],
             Lident(collection |> CollectionPrinter.add_action) |> lid(~loc),
             Some(Pat.tuple([Pat.var("entry" |> str(~loc))])),
           );

         let add_entry_to_input_exp =
           collection.plural
           |> E.update_field2(
                ~in_=("state", "input"),
                ~with_=[%expr
                  Belt.Array.concat(
                    [%e
                      collection.plural
                      |> E.field2(~in_=("state", "input"), ~loc)
                    ],
                    [|entry|],
                  )
                ],
                ~loc,
              );

         let add_entry_to_fields_statuses_exp =
           collection.plural
           |> E.update_field2(
                ~in_=("state", "fieldsStatuses"),
                ~with_=[%expr
                  Belt.Array.concat(
                    [%e
                      collection.plural
                      |> E.field2(~in_=("state", "fieldsStatuses"), ~loc)
                    ],
                    [|
                      [%e
                        Exp.record(
                          fields
                          |> List.map((field: Scheme.field) =>
                               (
                                 Lident(field.name) |> lid(~loc),
                                 [%expr Pristine],
                               )
                             ),
                          None,
                        )
                      ],
                    |],
                  )
                ],
                ~loc,
              );

         let remove_action_pat =
           Pat.construct(
             ~attrs=[explicit_arity(~loc)],
             Lident(collection |> CollectionPrinter.remove_action)
             |> lid(~loc),
             Some(Pat.tuple([Pat.var("index" |> str(~loc))])),
           );

         let remove_entry_from_input_exp =
           collection.plural
           |> E.update_field2(
                ~in_=("state", "input"),
                ~with_=[%expr
                  Belt.Array.keepWithIndex(
                    [%e
                      collection.plural
                      |> E.field2(~in_=("state", "input"), ~loc)
                    ],
                    (_, i) =>
                    i != index
                  )
                ],
                ~loc,
              );

         let remove_entry_from_fields_statuses_exp =
           collection.plural
           |> E.update_field2(
                ~in_=("state", "fieldsStatuses"),
                ~with_=[%expr
                  Belt.Array.keepWithIndex(
                    [%e
                      collection.plural
                      |> E.field2(~in_=("state", "fieldsStatuses"), ~loc)
                    ],
                    (_, i) =>
                    i != index
                  )
                ],
                ~loc,
              );

         let update_collections_statuses =
           Exp.record(
             [
               (
                 Lident(collection.plural) |> lid(~loc),
                 [%expr
                   Some(
                     [%e
                       E.apply_field2(
                         ~in_=("validators", collection.plural),
                         ~fn="collection",
                         ~args=[(Nolabel, [%expr nextInput])],
                         ~loc,
                       )
                     ],
                   )
                 ],
               ),
             ],
             switch (collections) {
             | [] => None
             | [x] => None
             | _ => Some([%expr state.collectionsStatuses])
             },
           );

         [
           Exp.case(
             add_action_pat,
             switch (deps) {
             | [] =>
               %expr
               {
                 let nextInput = [%e add_entry_to_input_exp];
                 let nextFieldsStatuses = [%e
                   add_entry_to_fields_statuses_exp
                 ];
                 switch%e (validator) {
                 | Ok(Some ())
                 | Error () =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses,
                     collectionsStatuses: [%e update_collections_statuses],
                   })
                 | Ok(None) =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses,
                   })
                 };
               }
             | [dep, ...deps] =>
               %expr
               {
                 let nextInput = [%e add_entry_to_input_exp];
                 let nextFieldsStatuses =
                   ref([%e add_entry_to_fields_statuses_exp]);

                 %e
                 {
                   scheme
                   |> Form_UseFormFn_DependentFields.ast(
                        ~loc,
                        ~dep,
                        ~deps,
                        ~trigger=`Collection(collection),
                      );
                 };

                 switch%e (validator) {
                 | Ok(Some ())
                 | Error () =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses^,
                     collectionsStatuses: [%e update_collections_statuses],
                   })
                 | Ok(None) =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses^,
                   })
                 };
               }
             },
           ),
           Exp.case(
             remove_action_pat,
             switch (deps) {
             | [] =>
               %expr
               {
                 let nextInput = [%e remove_entry_from_input_exp];
                 let nextFieldsStatuses = [%e
                   remove_entry_from_fields_statuses_exp
                 ];
                 switch%e (validator) {
                 | Ok(Some ())
                 | Error () =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses,
                     collectionsStatuses: [%e update_collections_statuses],
                   })
                 | Ok(None) =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses,
                   })
                 };
               }
             | [dep, ...deps] =>
               %expr
               {
                 let nextInput = [%e remove_entry_from_input_exp];
                 let nextFieldsStatuses =
                   ref([%e remove_entry_from_fields_statuses_exp]);

                 %e
                 {
                   scheme
                   |> Form_UseFormFn_DependentFields.ast(
                        ~loc,
                        ~dep,
                        ~deps,
                        ~trigger=`Collection(collection),
                      );
                 };

                 switch%e (validator) {
                 | Ok(Some ())
                 | Error () =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses^,
                     collectionsStatuses: [%e update_collections_statuses],
                   })
                 | Ok(None) =>
                   %expr
                   Update({
                     ...state,
                     input: nextInput,
                     fieldsStatuses: nextFieldsStatuses^,
                   })
                 };
               }
             },
           ),
           ...acc,
         ];
       },
       [],
     );
};
