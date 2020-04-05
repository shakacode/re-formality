open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~target: Target.t, ~async: bool, ~loc) => [%stri
  let useForm =
      (
        ~initialInput: input,
        ~onSubmit:
           (output, submissionCallbacks(input, submissionError)) => unit,
      ) => {
    let memoizedInitialState =
      React.useMemo1(() => initialInput->initialState, [|initialInput|]);

    let (state, dispatch) =
      ReactUpdate.(
        memoizedInitialState->useReducer((state, action) => {
          %e
          {
            Exp.match(
              [%expr action],
              [
                if (async) {
                  Exp.case(
                    [%pat? Submit],
                    switch%expr (state.formStatus) {
                    | Submitting(_) => NoUpdate
                    | Editing
                    | Submitted
                    | SubmissionFailed(_) =>
                      switch (
                        state.input
                        ->validateForm(
                            ~validators,
                            ~fieldsStatuses=state.fieldsStatuses,
                          )
                      ) {
                      | Validating({fieldsStatuses, collectionsStatuses}) =>
                        Update({
                          ...state,
                          fieldsStatuses,
                          collectionsStatuses,
                        })
                      | Valid({output, fieldsStatuses, collectionsStatuses}) =>
                        UpdateWithSideEffects(
                          {
                            ...state,
                            fieldsStatuses,
                            collectionsStatuses,
                            formStatus:
                              Submitting(
                                switch (state.formStatus) {
                                | SubmissionFailed(error) => Some(error)
                                | Editing
                                | Submitted
                                | Submitting(_) => None
                                },
                              ),
                            submissionStatus: AttemptedToSubmit,
                          },
                          ({state: _, dispatch}) =>
                            output->onSubmit({
                              notifyOnSuccess: input =>
                                SetSubmittedStatus(input)->dispatch,
                              notifyOnFailure: error =>
                                SetSubmissionFailedStatus(error)->dispatch,
                              reset: () => Reset->dispatch,
                              dismissSubmissionResult: () =>
                                DismissSubmissionResult->dispatch,
                            }),
                        )
                      | Invalid({fieldsStatuses, collectionsStatuses}) =>
                        Update({
                          ...state,
                          fieldsStatuses,
                          collectionsStatuses,
                          formStatus: Editing,
                          submissionStatus: AttemptedToSubmit,
                        })
                      }
                    },
                  );
                } else {
                  Exp.case(
                    [%pat? Submit],
                    switch%expr (state.formStatus) {
                    | Submitting(_) => NoUpdate
                    | Editing
                    | Submitted
                    | SubmissionFailed(_) =>
                      switch (
                        state.input
                        ->validateForm(
                            ~validators,
                            ~fieldsStatuses=state.fieldsStatuses,
                          )
                      ) {
                      | Valid({output, fieldsStatuses, collectionsStatuses}) =>
                        UpdateWithSideEffects(
                          {
                            ...state,
                            fieldsStatuses,
                            collectionsStatuses,
                            formStatus:
                              Submitting(
                                switch (state.formStatus) {
                                | SubmissionFailed(error) => Some(error)
                                | Editing
                                | Submitted
                                | Submitting(_) => None
                                },
                              ),
                            submissionStatus: AttemptedToSubmit,
                          },
                          ({state: _, dispatch}) =>
                            output->onSubmit({
                              notifyOnSuccess: input =>
                                SetSubmittedStatus(input)->dispatch,
                              notifyOnFailure: error =>
                                SetSubmissionFailedStatus(error)->dispatch,
                              reset: () => Reset->dispatch,
                              dismissSubmissionResult: () =>
                                DismissSubmissionResult->dispatch,
                            }),
                        )
                      | Invalid({fieldsStatuses, collectionsStatuses}) =>
                        Update({
                          ...state,
                          fieldsStatuses,
                          collectionsStatuses,
                          formStatus: Editing,
                          submissionStatus: AttemptedToSubmit,
                        })
                      }
                    },
                  );
                },
                Exp.case(
                  [%pat? SetSubmittedStatus(input)],
                  switch%expr (input) {
                  | Some(input) =>
                    Update({
                      ...state,
                      input,
                      formStatus: Submitted,
                      fieldsStatuses: input->initialFieldsStatuses,
                    })
                  | None =>
                    Update({
                      ...state,
                      formStatus: Submitted,
                      fieldsStatuses: state.input->initialFieldsStatuses,
                    })
                  },
                ),
                Exp.case(
                  [%pat? SetSubmissionFailedStatus(error)],
                  [%expr
                    Update({...state, formStatus: SubmissionFailed(error)})
                  ],
                ),
                Exp.case(
                  [%pat? MapSubmissionError(map)],
                  switch%expr (state.formStatus) {
                  | Submitting(Some(error)) =>
                    Update({
                      ...state,
                      formStatus: Submitting(Some(error->map)),
                    })
                  | SubmissionFailed(error) =>
                    Update({
                      ...state,
                      formStatus: SubmissionFailed(error->map),
                    })
                  | Editing
                  | Submitting(None)
                  | Submitted => NoUpdate
                  },
                ),
                Exp.case(
                  [%pat? DismissSubmissionError],
                  switch%expr (state.formStatus) {
                  | Editing
                  | Submitting(_)
                  | Submitted => NoUpdate
                  | SubmissionFailed(_) =>
                    Update({...state, formStatus: Editing})
                  },
                ),
                Exp.case(
                  [%pat? DismissSubmissionResult],
                  switch%expr (state.formStatus) {
                  | Editing
                  | Submitting(_) => NoUpdate
                  | Submitted
                  | SubmissionFailed(_) =>
                    Update({...state, formStatus: Editing})
                  },
                ),
                Exp.case(
                  [%pat? Reset],
                  [%expr Update(initialInput->initialState)],
                ),
              ]
              |> List.append(
                   {
                     let collections = scheme |> Scheme.collections;
                     collections
                     |> List.fold_left(
                          (
                            acc,
                            {collection, validator, fields}: Scheme.collection,
                          ) => {
                            let deps =
                              fields
                              |> List.fold_left(
                                   (acc, field: Scheme.field) =>
                                     acc |> List.append(field.deps),
                                   [],
                                 );

                            let add_action_pat =
                              Pat.construct(
                                ~attrs=[explicit_arity(~loc)],
                                Lident(
                                  collection |> CollectionPrinter.add_action,
                                )
                                |> lid(~loc),
                                Some(
                                  Pat.tuple([
                                    Pat.var("entry" |> str(~loc)),
                                  ]),
                                ),
                              );

                            let add_entry_to_input_exp =
                              collection.plural
                              |> E.update_field2(
                                   ~in_=("state", "input"),
                                   ~with_=[%expr
                                     Belt.Array.concat(
                                       [%e
                                         collection.plural
                                         |> E.field2(
                                              ~in_=("state", "input"),
                                              ~loc,
                                            )
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
                                         |> E.field2(
                                              ~in_=(
                                                "state",
                                                "fieldsStatuses",
                                              ),
                                              ~loc,
                                            )
                                       ],
                                       [|
                                         [%e
                                           Exp.record(
                                             fields
                                             |> List.map(
                                                  (field: Scheme.field) =>
                                                  (
                                                    Lident(field.name)
                                                    |> lid(~loc),
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
                                Lident(
                                  collection |> CollectionPrinter.remove_action,
                                )
                                |> lid(~loc),
                                Some(
                                  Pat.tuple([
                                    Pat.var("index" |> str(~loc)),
                                  ]),
                                ),
                              );

                            let remove_entry_from_input_exp =
                              collection.plural
                              |> E.update_field2(
                                   ~in_=("state", "input"),
                                   ~with_=[%expr
                                     Belt.Array.keepWithIndex(
                                       [%e
                                         collection.plural
                                         |> E.field2(
                                              ~in_=("state", "input"),
                                              ~loc,
                                            )
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
                                         |> E.field2(
                                              ~in_=(
                                                "state",
                                                "fieldsStatuses",
                                              ),
                                              ~loc,
                                            )
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
                                            ~in_=(
                                              "validators",
                                              collection.plural,
                                            ),
                                            ~fn="collection",
                                            ~args=[
                                              (Nolabel, [%expr nextInput]),
                                            ],
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
                                        collectionsStatuses: [%e
                                          update_collections_statuses
                                        ],
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
                                      ref(
                                        [%e add_entry_to_fields_statuses_exp],
                                      );

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
                                        collectionsStatuses: [%e
                                          update_collections_statuses
                                        ],
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
                                    let nextInput = [%e
                                      remove_entry_from_input_exp
                                    ];
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
                                        collectionsStatuses: [%e
                                          update_collections_statuses
                                        ],
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
                                    let nextInput = [%e
                                      remove_entry_from_input_exp
                                    ];
                                    let nextFieldsStatuses =
                                      ref(
                                        [%e
                                          remove_entry_from_fields_statuses_exp
                                        ],
                                      );

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
                                        collectionsStatuses: [%e
                                          update_collections_statuses
                                        ],
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
                   },
                 )
              |> List.append(
                   scheme
                   |> List.fold_left(
                        (acc, entry: Scheme.entry) =>
                          switch (entry) {
                          | Field({validator: SyncValidator(_)}) => acc
                          | Field({validator: AsyncValidator(_)} as field) => [
                              Exp.case(
                                Pat.construct(
                                  Lident(
                                    FieldPrinter.apply_async_result_action(
                                      ~field=field.name,
                                    ),
                                  )
                                  |> lid(~loc),
                                  Some(
                                    Pat.tuple([
                                      Pat.var("value" |> str(~loc)),
                                      Pat.var("result" |> str(~loc)),
                                    ]),
                                  ),
                                ),
                                {
                                  %expr
                                  {
                                    let validator = [%e
                                      field.name
                                      |> E.field(~in_="validators", ~loc)
                                    ];
                                    switch (
                                      [%e
                                        field.name
                                        |> E.field2(
                                             ~in_=("state", "fieldsStatuses"),
                                             ~loc,
                                           )
                                      ]
                                    ) {
                                    | Validating(x)
                                        when validator.eq(x, value) =>
                                      Update({
                                        ...state,
                                        fieldsStatuses: [%e
                                          field.name
                                          |> E.update_field2(
                                               ~in_=(
                                                 "state",
                                                 "fieldsStatuses",
                                               ),
                                               ~with_=[%expr
                                                 Dirty(result, Shown)
                                               ],
                                               ~loc,
                                             )
                                        ],
                                      })
                                    | Validating(_)
                                    | Pristine
                                    | Dirty(_, Shown | Hidden) => NoUpdate
                                    };
                                  };
                                },
                              ),
                              ...acc,
                            ]
                          | Collection({collection, fields}) =>
                            fields
                            |> List.fold_left(
                                 (acc, field: Scheme.field) =>
                                   switch (field.validator) {
                                   | SyncValidator(_) => acc
                                   | AsyncValidator(_) => [
                                       Exp.case(
                                         Pat.construct(
                                           Lident(
                                             FieldOfCollectionPrinter.apply_async_result_action(
                                               ~collection,
                                               ~field=field.name,
                                             ),
                                           )
                                           |> lid(~loc),
                                           Some(
                                             Pat.tuple([
                                               Pat.var("value" |> str(~loc)),
                                               Pat.var("index" |> str(~loc)),
                                               Pat.var(
                                                 "result" |> str(~loc),
                                               ),
                                             ]),
                                           ),
                                         ),
                                         {
                                           %expr
                                           {
                                             let validator = [%e
                                               field.name
                                               |> E.field_of_collection_validator(
                                                    ~validators="validators",
                                                    ~collection,
                                                    ~loc,
                                                  )
                                             ];
                                             switch (
                                               [%e
                                                 field.name
                                                 |> E.field_of_collection2(
                                                      ~in_=(
                                                        "state",
                                                        "fieldsStatuses",
                                                      ),
                                                      ~collection,
                                                      ~loc,
                                                    )
                                               ]
                                             ) {
                                             | Validating(x)
                                                 when validator.eq(x, value) =>
                                               Update({
                                                 ...state,
                                                 fieldsStatuses: [%e
                                                   field.name
                                                   |> E.update_field_of_collection2(
                                                        ~in_=(
                                                          "state",
                                                          "fieldsStatuses",
                                                        ),
                                                        ~collection,
                                                        ~with_=[%expr
                                                          Dirty(result, Shown)
                                                        ],
                                                        ~loc,
                                                      )
                                                 ],
                                               })
                                             | Validating(_)
                                             | Pristine
                                             | Dirty(_, Shown | Hidden) =>
                                               NoUpdate
                                             };
                                           };
                                         },
                                       ),
                                       ...acc,
                                     ]
                                   },
                                 acc,
                               )
                          },
                        [],
                      ),
                 )
              |> List.append(
                   scheme
                   |> List.fold_left(
                        (acc, entry: Scheme.entry) =>
                          switch (entry) {
                          | Field(field) => [
                              Exp.case(
                                Pat.construct(
                                  Lident(
                                    FieldPrinter.blur_action(
                                      ~field=field.name,
                                    ),
                                  )
                                  |> lid(~loc),
                                  None,
                                ),
                                {
                                  let field_status_expr =
                                    field.name
                                    |> E.field2(
                                         ~in_=("state", "fieldsStatuses"),
                                         ~loc,
                                       );
                                  let field_input_expr =
                                    field.name
                                    |> E.field2(
                                         ~in_=("state", "input"),
                                         ~loc,
                                       );
                                  let validator_expr =
                                    field.name
                                    |> E.field(~in_="validators", ~loc);
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
                                            ~fieldStatus=[%e
                                              field_status_expr
                                            ],
                                            ~validator=[%e validator_expr],
                                            ~setStatus=[%e
                                              [%expr
                                                status => [%e set_status_expr]
                                              ]
                                            ],
                                          )
                                        | Ok(Optional(None)) =>
                                          %expr
                                          validateFieldOnBlurWithoutValidator(
                                            ~fieldInput=[%e field_input_expr],
                                            ~fieldStatus=[%e
                                              field_status_expr
                                            ],
                                            ~setStatus=[%e
                                              [%expr
                                                status => [%e set_status_expr]
                                              ]
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
                                              ~fieldStatus=[%e
                                                field_status_expr
                                              ],
                                              ~validator=[%e validator_expr],
                                              ~setStatus=[%e
                                                [%expr
                                                  status => [%e
                                                    set_status_expr
                                                  ]
                                                ]
                                              ],
                                            );
                                          }
                                        | Some(OptionType) =>
                                          %expr
                                          {
                                            Async.validateFieldOfOptionTypeOnBlur(
                                              ~input=state.input,
                                              ~fieldStatus=[%e
                                                field_status_expr
                                              ],
                                              ~validator=[%e validator_expr],
                                              ~setStatus=[%e
                                                [%expr
                                                  status => [%e
                                                    set_status_expr
                                                  ]
                                                ]
                                              ],
                                            );
                                          }
                                        | Some(StringType) =>
                                          %expr
                                          {
                                            Async.validateFieldOfStringTypeOnBlur(
                                              ~input=state.input,
                                              ~fieldStatus=[%e
                                                field_status_expr
                                              ],
                                              ~validator=[%e validator_expr],
                                              ~setStatus=[%e
                                                [%expr
                                                  status => [%e
                                                    set_status_expr
                                                  ]
                                                ]
                                              ],
                                            );
                                          }
                                        | Some(OptionStringType) =>
                                          %expr
                                          {
                                            Async.validateFieldOfOptionStringTypeOnBlur(
                                              ~input=state.input,
                                              ~fieldStatus=[%e
                                                field_status_expr
                                              ],
                                              ~validator=[%e validator_expr],
                                              ~setStatus=[%e
                                                [%expr
                                                  status => [%e
                                                    set_status_expr
                                                  ]
                                                ]
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
                                            |> E.field(
                                                 ~in_="fieldsStatuses",
                                                 ~loc,
                                               )
                                          ]
                                        ) {
                                        | Validating(value) =>
                                          UpdateWithSideEffects(
                                            {...state, fieldsStatuses},
                                            ({state: _, dispatch}) => {
                                              %e
                                              E.apply_field2(
                                                ~in_=(
                                                  "validators",
                                                  field.name,
                                                ),
                                                ~fn="validateAsync",
                                                ~args=[
                                                  (
                                                    Nolabel,
                                                    [%expr (value, dispatch)],
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
                                         Some(
                                           Pat.tuple([
                                             Pat.var("index" |> str(~loc)),
                                           ]),
                                         ),
                                       ),
                                       {
                                         let field_status_expr =
                                           field.name
                                           |> E.field_of_collection2(
                                                ~in_=(
                                                  "state",
                                                  "fieldsStatuses",
                                                ),
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
                                                ~in_=(
                                                  "state",
                                                  "fieldsStatuses",
                                                ),
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
                                               | Ok(
                                                   Required |
                                                   Optional(Some(_)),
                                                 )
                                               | Error () =>
                                                 %expr
                                                 validateFieldOfCollectionOnBlurWithValidator(
                                                   ~input=state.input,
                                                   ~index,
                                                   ~fieldStatus=[%e
                                                     field_status_expr
                                                   ],
                                                   ~validator=[%e
                                                     validator_expr
                                                   ],
                                                   ~setStatus=[%e
                                                     [%expr
                                                       status => [%e
                                                         set_status_expr
                                                       ]
                                                     ]
                                                   ],
                                                 )
                                               | Ok(Optional(None)) =>
                                                 %expr
                                                 validateFieldOnBlurWithoutValidator(
                                                   ~fieldInput=[%e
                                                     field_input_expr
                                                   ],
                                                   ~fieldStatus=[%e
                                                     field_status_expr
                                                   ],
                                                   ~setStatus=[%e
                                                     [%expr
                                                       status => [%e
                                                         set_status_expr
                                                       ]
                                                     ]
                                                   ],
                                                 )
                                               };

                                             switch (result) {
                                             | Some(fieldsStatuses) =>
                                               Update({
                                                 ...state,
                                                 fieldsStatuses,
                                               })
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
                                                     ~fieldStatus=[%e
                                                       field_status_expr
                                                     ],
                                                     ~validator=[%e
                                                       validator_expr
                                                     ],
                                                     ~setStatus=[%e
                                                       [%expr
                                                         status => [%e
                                                           set_status_expr
                                                         ]
                                                       ]
                                                     ],
                                                   );
                                                 }
                                               | Some(OptionType) =>
                                                 %expr
                                                 {
                                                   Async.validateFieldOfCollectionOfOptionTypeOnBlur(
                                                     ~input=state.input,
                                                     ~index,
                                                     ~fieldStatus=[%e
                                                       field_status_expr
                                                     ],
                                                     ~validator=[%e
                                                       validator_expr
                                                     ],
                                                     ~setStatus=[%e
                                                       [%expr
                                                         status => [%e
                                                           set_status_expr
                                                         ]
                                                       ]
                                                     ],
                                                   );
                                                 }
                                               | Some(StringType) =>
                                                 %expr
                                                 {
                                                   Async.validateFieldOfCollectionOfStringTypeOnBlur(
                                                     ~input=state.input,
                                                     ~index,
                                                     ~fieldStatus=[%e
                                                       field_status_expr
                                                     ],
                                                     ~validator=[%e
                                                       validator_expr
                                                     ],
                                                     ~setStatus=[%e
                                                       [%expr
                                                         status => [%e
                                                           set_status_expr
                                                         ]
                                                       ]
                                                     ],
                                                   );
                                                 }
                                               | Some(OptionStringType) =>
                                                 %expr
                                                 {
                                                   Async.validateFieldOfCollectionOfOptionStringTypeOnBlur(
                                                     ~input=state.input,
                                                     ~index,
                                                     ~fieldStatus=[%e
                                                       field_status_expr
                                                     ],
                                                     ~validator=[%e
                                                       validator_expr
                                                     ],
                                                     ~setStatus=[%e
                                                       [%expr
                                                         status => [%e
                                                           set_status_expr
                                                         ]
                                                       ]
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
                                                           [%expr
                                                             (
                                                               value,
                                                               index,
                                                               dispatch,
                                                             )
                                                           ],
                                                         ),
                                                       ],
                                                       ~loc,
                                                     )
                                                   },
                                                 )
                                               | Pristine
                                               | Dirty(_, Shown | Hidden) =>
                                                 Update({
                                                   ...state,
                                                   fieldsStatuses,
                                                 })
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
                      ),
                 )
              |> List.append(Form_UseFormFn_UpdateActions.ast(~loc, scheme)),
            );
          }
        })
      );

    %e
    {
      Form_UseFormFn_Interface.ast(~scheme, ~target, ~async, ~loc);
    };
  }
];
