open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ensure_eq = (~loc, fields) =>
  if (fields
      |> List.exists((({txt: lid}, _)) =>
           switch (lid) {
           | Lident("eq") => true
           | _ => false
           }
         )) {
    fields;
  } else {
    [(Lident("eq") |> lid(~loc), [%expr (==)]), ...fields];
  };

let update_async_validator_of_field =
    (
      ~field: string,
      ~output_type: ItemType.t,
      ~async_mode: AsyncMode.t,
      ~validator_loc: Location.t,
      fields,
    ) =>
  fields
  |> ensure_eq(~loc=validator_loc)
  |> List.rev
  |> List.rev_map(((v_lid, {pexp_loc: loc} as expr)) =>
       switch (v_lid) {
       | {txt: Lident("validateAsync")} =>
         let fn = [%expr
           (
             ((value, dispatch)) => {
               let validate:
                 Async.validateAsyncFn(
                   [%t output_type |> ItemType.unpack],
                   message,
                 ) = [%e
                 expr
               ];
               Async.validateAsync(~value, ~validate, ~andThen=res => {
                 dispatch(
                   [%e
                     Exp.construct(
                       Lident(FieldPrinter.apply_async_result_action(~field))
                       |> lid(~loc),
                       Some(
                         Exp.tuple([
                           Exp.ident(Lident("value") |> lid(~loc)),
                           Exp.ident(Lident("res") |> lid(~loc)),
                         ]),
                       ),
                     )
                   ],
                 )
               });
             }
           )
         ];
         (
           v_lid,
           switch (async_mode) {
           | OnBlur => fn
           | OnChange =>
             %expr
             Debouncer.make(~wait=debounceInterval, [%e fn])
           },
         );
       | _ => (v_lid, expr)
       }
     );

let update_async_validator_of_field_of_collection =
    (
      ~field: string,
      ~collection: Collection.t,
      ~output_type: ItemType.t,
      ~async_mode: AsyncMode.t,
      ~validator_loc: Location.t,
      fields,
    ) =>
  fields
  |> ensure_eq(~loc=validator_loc)
  |> List.rev
  |> List.rev_map(((v_lid, {pexp_loc: loc} as expr)) =>
       switch (v_lid) {
       | {txt: Lident("validateAsync")} =>
         let fn = [%expr
           (
             ((value, index, dispatch)) => {
               let validate:
                 Async.validateAsyncFn(
                   [%t output_type |> ItemType.unpack],
                   message,
                 ) = [%e
                 expr
               ];
               Async.validateAsync(~value, ~validate, ~andThen=res => {
                 dispatch(
                   [%e
                     Exp.construct(
                       Lident(
                         FieldOfCollectionPrinter.apply_async_result_action(
                           ~collection,
                           ~field,
                         ),
                       )
                       |> lid(~loc),
                       Some(
                         Exp.tuple([
                           Exp.ident(Lident("value") |> lid(~loc)),
                           Exp.ident(Lident("index") |> lid(~loc)),
                           Exp.ident(Lident("res") |> lid(~loc)),
                         ]),
                       ),
                     )
                   ],
                 )
               });
             }
           )
         ];
         (
           v_lid,
           switch (async_mode) {
           | OnBlur => fn
           | OnChange =>
             %expr
             Debouncer.make(~wait=debounceInterval, [%e fn])
           },
         );
       | _ => (v_lid, expr)
       }
     );

// What we need to do here:
// 1. Update values of optional validators: set them to () instead of None
// 2. Wrap async validators so each dispatches appropriate action
// 3. Debounce async validators that run on change
// 4. Don't touch unknown, let compiler do its job
let ast =
    (
      scheme: Scheme.t,
      validators_record: ValidatorsRecord.t,
      value_binding: value_binding,
    ) => {
  let fields =
    validators_record.fields
    |> List.rev
    |> List.rev_map(((f_lid, expr)) =>
         switch (f_lid) {
         | {txt: Lident(key)} =>
           let entry =
             scheme
             |> List.find_opt(
                  fun
                  | Scheme.Field(field) => field.name == key
                  | Scheme.Collection({collection}) =>
                    collection.plural == key,
                );
           switch (entry) {
           | None => (f_lid, expr)
           | Some(Field(field)) =>
             switch (field.validator) {
             | SyncValidator(Ok(Required)) => (f_lid, expr)
             | SyncValidator(Ok(Optional(Some ()))) => (f_lid, expr)
             | SyncValidator(Ok(Optional(None))) =>
               let loc = expr.pexp_loc;
               (f_lid, [%expr ()]);
             | SyncValidator(Error ()) => (f_lid, expr)
             | AsyncValidator({mode: async_mode}) => (
                 f_lid,
                 switch (expr) {
                 | {
                     pexp_desc: Pexp_record(fields, None),
                     pexp_loc,
                     pexp_loc_stack,
                     pexp_attributes,
                   } => {
                     pexp_desc:
                       Pexp_record(
                         fields
                         |> update_async_validator_of_field(
                              ~field=field.name,
                              ~output_type=field.output_type,
                              ~async_mode,
                              ~validator_loc=pexp_loc,
                            ),
                         None,
                       ),
                     pexp_loc,
                     pexp_loc_stack,
                     pexp_attributes,
                   }
                 | _ => expr
                 },
               )
             }
           | Some(
               Collection({
                 collection,
                 fields: collection_fields,
                 validator: collection_validator,
               }),
             ) => (
               f_lid,
               switch (expr) {
               | {
                   pexp_desc: Pexp_record(collection_validator_fields, None),
                   pexp_loc,
                   pexp_loc_stack,
                   pexp_attributes,
                 } =>
                 let fields =
                   collection_validator_fields
                   |> List.rev
                   |> List.rev_map(((c_lid, expr)) =>
                        switch (c_lid) {
                        | {txt: Lident("collection")} => (
                            c_lid,
                            switch (collection_validator) {
                            | Ok(Some ())
                            | Error () => expr
                            | Ok(None) =>
                              let loc = expr.pexp_loc;
                              %expr
                              ();
                            },
                          )
                        | {txt: Lident("fields")} => (
                            c_lid,
                            switch (expr) {
                            | {
                                pexp_desc:
                                  Pexp_record(field_validator_fields, None),
                                pexp_loc,
                                pexp_loc_stack,
                                pexp_attributes,
                              } =>
                              let fields =
                                field_validator_fields
                                |> List.rev
                                |> List.rev_map(((f_lid, expr)) =>
                                     switch (f_lid) {
                                     | {txt: Lident(key)} =>
                                       let field =
                                         collection_fields
                                         |> List.find_opt(
                                              (field: Scheme.field) =>
                                              field.name == key
                                            );
                                       switch (field) {
                                       | None => (f_lid, expr)
                                       | Some({
                                           validator:
                                             SyncValidator(Ok(Required)),
                                         }) => (
                                           f_lid,
                                           expr,
                                         )
                                       | Some({
                                           validator:
                                             SyncValidator(
                                               Ok(Optional(Some ())),
                                             ),
                                         }) => (
                                           f_lid,
                                           expr,
                                         )
                                       | Some({
                                           validator:
                                             SyncValidator(
                                               Ok(Optional(None)),
                                             ),
                                         }) =>
                                         let loc = expr.pexp_loc;
                                         (f_lid, [%expr ()]);
                                       | Some({
                                           validator: SyncValidator(Error ()),
                                         }) => (
                                           f_lid,
                                           expr,
                                         )
                                       | Some(
                                           {
                                             validator:
                                               AsyncValidator({
                                                 mode: async_mode,
                                               }),
                                           } as field,
                                         ) => (
                                           f_lid,
                                           switch (expr) {
                                           | {
                                               pexp_desc:
                                                 Pexp_record(fields, None),
                                               pexp_loc,
                                               pexp_loc_stack,
                                               pexp_attributes,
                                             } => {
                                               pexp_desc:
                                                 Pexp_record(
                                                   fields
                                                   |> update_async_validator_of_field_of_collection(
                                                        ~field=field.name,
                                                        ~collection,
                                                        ~output_type=
                                                          field.output_type,
                                                        ~async_mode,
                                                        ~validator_loc=pexp_loc,
                                                      ),
                                                   None,
                                                 ),
                                               pexp_loc,
                                               pexp_loc_stack,
                                               pexp_attributes,
                                             }
                                           | _ => expr
                                           },
                                         )
                                       };
                                     | {txt: _} => (f_lid, expr)
                                     }
                                   );
                              {
                                pexp_desc: Pexp_record(fields, None),
                                pexp_loc,
                                pexp_loc_stack,
                                pexp_attributes,
                              };
                            | _ => expr
                            },
                          )
                        | _ => (c_lid, expr)
                        }
                      );

                 {
                   pexp_desc: Pexp_record(fields, None),
                   pexp_loc,
                   pexp_loc_stack,
                   pexp_attributes,
                 };
               | _ => expr
               },
             )
           };
         | _ => (f_lid, expr)
         }
       );
  {
    ...value_binding,
    pvb_expr: {
      pexp_desc:
        Pexp_constraint(
          {
            pexp_desc: Pexp_record(fields, None),
            pexp_loc: validators_record.record_metadata.pexp_loc,
            pexp_loc_stack: validators_record.record_metadata.pexp_loc_stack,
            pexp_attributes: validators_record.record_metadata.pexp_attributes,
          },
          validators_record.annotation,
        ),
      pexp_loc: validators_record.constraint_metadata.pexp_loc,
      pexp_loc_stack: validators_record.constraint_metadata.pexp_loc_stack,
      pexp_attributes: validators_record.constraint_metadata.pexp_attributes,
    },
  };
};
