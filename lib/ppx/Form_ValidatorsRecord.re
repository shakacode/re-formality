open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

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
    |> List.map(((flid, expr)) =>
         switch (flid) {
         | {txt: Lident(field)} =>
           let entry =
             scheme
             |> List.find_opt(
                  fun
                  | Scheme.Field({name}) => name == field,
                );
           switch (entry) {
           | Some(Field({name, validator, output_type})) =>
             switch (validator) {
             | SyncValidator(Ok(Required)) => (flid, expr)
             | SyncValidator(Ok(Optional(Some ()))) => (flid, expr)
             | SyncValidator(Ok(Optional(None))) =>
               let loc = expr.pexp_loc;
               (flid, [%expr ()]);
             | SyncValidator(Error ()) => (flid, expr)
             | AsyncValidator({mode: async_mode}) => (
                 flid,
                 switch (expr) {
                 | {
                     pexp_desc: Pexp_record(fields, None),
                     pexp_loc,
                     pexp_loc_stack,
                     pexp_attributes,
                   } =>
                   let fields_with_eq =
                     if (fields
                         |> List.exists((({txt: lid}, _)) =>
                              switch (lid) {
                              | Lident("eq") => true
                              | _ => false
                              }
                            )) {
                       fields;
                     } else {
                       let loc = pexp_loc;
                       [
                         (Lident("eq") |> lid(~loc), [%expr (==)]),
                         ...fields,
                       ];
                     };
                   let fields_with_eq_and_wrapped_async_validator =
                     fields_with_eq
                     |> List.map(((vlid, {pexp_loc: loc} as expr)) =>
                          switch (vlid) {
                          | {txt: Lident("validateAsync")} =>
                            let fn = [%expr
                              (
                                ((value, dispatch)) => {
                                  Js.log2(
                                    "Executed async validator with value:",
                                    value,
                                  );
                                  let validate:
                                    Async.validateAsyncFn(
                                      [%t output_type |> FieldType.unpack],
                                      message,
                                    ) = [%e
                                    expr
                                  ];
                                  Async.validateAsync(
                                    ~value, ~validate, ~andThen=res => {
                                    dispatch(
                                      [%e
                                        Exp.construct(
                                          Lident(
                                            Field.Field(name)
                                            |> Field.apply_async_result_action,
                                          )
                                          |> lid(~loc),
                                          Some(
                                            Exp.tuple([
                                              Exp.ident(
                                                Lident("value") |> lid(~loc),
                                              ),
                                              Exp.ident(
                                                Lident("res") |> lid(~loc),
                                              ),
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
                              vlid,
                              switch (async_mode) {
                              | OnBlur => fn
                              | OnChange =>
                                %expr
                                Debouncer.make(
                                  ~wait=debounceInterval,
                                  [%e fn],
                                )
                              },
                            );
                          | _ => (vlid, expr)
                          }
                        );
                   {
                     pexp_desc:
                       Pexp_record(
                         fields_with_eq_and_wrapped_async_validator,
                         None,
                       ),
                     pexp_loc,
                     pexp_loc_stack,
                     pexp_attributes,
                   };
                 | _ => expr
                 },
               )
             }
           | None => (flid, expr)
           };
         | _ => (flid, expr)
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
