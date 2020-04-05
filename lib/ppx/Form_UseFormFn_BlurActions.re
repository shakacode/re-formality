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
                 Lident(FieldPrinter.blur_action(~field=field.name))
                 |> lid(~loc),
                 None,
               ),
               {
                 let field_status_expr =
                   field.name
                   |> E.field2(~in_=("state", "fieldsStatuses"), ~loc);
                 let field_input_expr =
                   field.name |> E.field2(~in_=("state", "input"), ~loc);
                 let validator_expr =
                   field.name |> E.field(~in_="validators", ~loc);
                 let set_status_expr =
                   field.name
                   |> E.update_field2(
                        ~in_=("state", "fieldsStatuses"),
                        ~with_=[%expr status],
                        ~loc,
                      );

                 switch (field.validator) {
                 | SyncValidator(validator) =>
                   %expr
                   {
                     let result =
                       switch%e (validator) {
                       | Ok(Required | Optional(Some(_)))
                       | Error () =>
                         %expr
                         validateFieldOnBlurWithValidator(
                           ~input=state.input,
                           ~fieldStatus=[%e field_status_expr],
                           ~validator=[%e validator_expr],
                           ~setStatus=[%e
                             [%expr status => [%e set_status_expr]]
                           ],
                         )
                       | Ok(Optional(None)) =>
                         %expr
                         validateFieldOnBlurWithoutValidator(
                           ~fieldInput=[%e field_input_expr],
                           ~fieldStatus=[%e field_status_expr],
                           ~setStatus=[%e
                             [%expr status => [%e set_status_expr]]
                           ],
                         )
                       };

                     switch (result) {
                     | Some(fieldsStatuses) =>
                       Update({...state, fieldsStatuses})
                     | None => NoUpdate
                     };
                   }
                 | AsyncValidator({optionality}) =>
                   %expr
                   {
                     let result =
                       switch%e (optionality) {
                       | None =>
                         %expr
                         {
                           Async.validateFieldOnBlur(
                             ~input=state.input,
                             ~fieldStatus=[%e field_status_expr],
                             ~validator=[%e validator_expr],
                             ~setStatus=[%e
                               [%expr status => [%e set_status_expr]]
                             ],
                           );
                         }
                       | Some(OptionType) =>
                         %expr
                         {
                           Async.validateFieldOfOptionTypeOnBlur(
                             ~input=state.input,
                             ~fieldStatus=[%e field_status_expr],
                             ~validator=[%e validator_expr],
                             ~setStatus=[%e
                               [%expr status => [%e set_status_expr]]
                             ],
                           );
                         }
                       | Some(StringType) =>
                         %expr
                         {
                           Async.validateFieldOfStringTypeOnBlur(
                             ~input=state.input,
                             ~fieldStatus=[%e field_status_expr],
                             ~validator=[%e validator_expr],
                             ~setStatus=[%e
                               [%expr status => [%e set_status_expr]]
                             ],
                           );
                         }
                       | Some(OptionStringType) =>
                         %expr
                         {
                           Async.validateFieldOfOptionStringTypeOnBlur(
                             ~input=state.input,
                             ~fieldStatus=[%e field_status_expr],
                             ~validator=[%e validator_expr],
                             ~setStatus=[%e
                               [%expr status => [%e set_status_expr]]
                             ],
                           );
                         }
                       };

                     switch (result) {
                     | None => NoUpdate
                     | Some(fieldsStatuses) =>
                       switch (
                         [%e
                           field.name |> E.field(~in_="fieldsStatuses", ~loc)
                         ]
                       ) {
                       | Validating(value) =>
                         UpdateWithSideEffects(
                           {...state, fieldsStatuses},
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
                         Update({...state, fieldsStatuses})
                       }
                     };
                   }
                 };
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
                          FieldOfCollectionPrinter.blur_action(
                            ~collection,
                            ~field=field.name,
                          ),
                        )
                        |> lid(~loc),
                        Some(Pat.tuple([Pat.var("index" |> str(~loc))])),
                      ),
                      {
                        let field_status_expr =
                          field.name
                          |> E.field_of_collection2(
                               ~in_=("state", "fieldsStatuses"),
                               ~collection,
                               ~loc,
                             );
                        let field_input_expr =
                          field.name
                          |> E.field_of_collection2(
                               ~in_=("state", "input"),
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

                        switch (field.validator) {
                        | SyncValidator(validator) =>
                          %expr
                          {
                            let result =
                              switch%e (validator) {
                              | Ok(Required | Optional(Some(_)))
                              | Error () =>
                                %expr
                                validateFieldOfCollectionOnBlurWithValidator(
                                  ~input=state.input,
                                  ~index,
                                  ~fieldStatus=[%e field_status_expr],
                                  ~validator=[%e validator_expr],
                                  ~setStatus=[%e
                                    [%expr status => [%e set_status_expr]]
                                  ],
                                )
                              | Ok(Optional(None)) =>
                                %expr
                                validateFieldOnBlurWithoutValidator(
                                  ~fieldInput=[%e field_input_expr],
                                  ~fieldStatus=[%e field_status_expr],
                                  ~setStatus=[%e
                                    [%expr status => [%e set_status_expr]]
                                  ],
                                )
                              };

                            switch (result) {
                            | Some(fieldsStatuses) =>
                              Update({...state, fieldsStatuses})
                            | None => NoUpdate
                            };
                          }
                        | AsyncValidator({optionality}) =>
                          %expr
                          {
                            let result =
                              switch%e (optionality) {
                              | None =>
                                %expr
                                {
                                  Async.validateFieldOfCollectionOnBlur(
                                    ~input=state.input,
                                    ~index,
                                    ~fieldStatus=[%e field_status_expr],
                                    ~validator=[%e validator_expr],
                                    ~setStatus=[%e
                                      [%expr status => [%e set_status_expr]]
                                    ],
                                  );
                                }
                              | Some(OptionType) =>
                                %expr
                                {
                                  Async.validateFieldOfCollectionOfOptionTypeOnBlur(
                                    ~input=state.input,
                                    ~index,
                                    ~fieldStatus=[%e field_status_expr],
                                    ~validator=[%e validator_expr],
                                    ~setStatus=[%e
                                      [%expr status => [%e set_status_expr]]
                                    ],
                                  );
                                }
                              | Some(StringType) =>
                                %expr
                                {
                                  Async.validateFieldOfCollectionOfStringTypeOnBlur(
                                    ~input=state.input,
                                    ~index,
                                    ~fieldStatus=[%e field_status_expr],
                                    ~validator=[%e validator_expr],
                                    ~setStatus=[%e
                                      [%expr status => [%e set_status_expr]]
                                    ],
                                  );
                                }
                              | Some(OptionStringType) =>
                                %expr
                                {
                                  Async.validateFieldOfCollectionOfOptionStringTypeOnBlur(
                                    ~input=state.input,
                                    ~index,
                                    ~fieldStatus=[%e field_status_expr],
                                    ~validator=[%e validator_expr],
                                    ~setStatus=[%e
                                      [%expr status => [%e set_status_expr]]
                                    ],
                                  );
                                }
                              };

                            switch (result) {
                            | None => NoUpdate
                            | Some(fieldsStatuses) =>
                              switch (
                                [%e
                                  field.name
                                  |> E.field_of_collection(
                                       ~in_="fieldsStatuses",
                                       ~collection,
                                       ~loc,
                                     )
                                ]
                              ) {
                              | Validating(value) =>
                                UpdateWithSideEffects(
                                  {...state, fieldsStatuses},
                                  ({state: _, dispatch}) => {
                                    %e
                                    E.apply_field4(
                                      ~in_=(
                                        "validators",
                                        collection.plural,
                                        "fields",
                                        field.name,
                                      ),
                                      ~fn="validateAsync",
                                      ~args=[
                                        (
                                          Nolabel,
                                          [%expr (value, index, dispatch)],
                                        ),
                                      ],
                                      ~loc,
                                    )
                                  },
                                )
                              | Pristine
                              | Dirty(_, Shown | Hidden) =>
                                Update({...state, fieldsStatuses})
                              }
                            };
                          }
                        };
                      },
                    ),
                    ...acc,
                  ],
                acc,
              )
         },
       [],
     );
