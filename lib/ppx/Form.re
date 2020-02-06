open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let open_formality = (~loc) => [%stri open Formality];

let input_type = (input_type: InputType.t) => {
  input_type |> InputType.structure_item;
};

let output_type = (output_type: OutputType.t) => {
  output_type |> OutputType.structure_item;
};

let message_type = (message_type: MessageType.t) =>
  message_type |> MessageType.structure_item;

let submission_error_type = (submission_error_type: SubmissionErrorType.t) =>
  submission_error_type |> SubmissionErrorType.structure_item;

let validators_type = (~loc, fields: list(FieldSpec.t)) => {
  fields
  |> T.record_of_fields(
       ~name="validators",
       ~loc,
       ~typ=field => {
         let typ =
           Typ.constr(
             Lident("singleValueValidator") |> lid(~loc),
             [
               Typ.constr(Lident("input") |> lid(~loc), []),
               field.output_type |> FieldType.unpack,
               Typ.constr(Lident("message") |> lid(~loc), []),
             ],
           );
         switch (field.validator) {
         | `Required => typ
         | `Optional => Typ.constr(Lident("option") |> lid(~loc), [typ])
         };
       },
     );
};

let fields_statuses_type = (~loc, fields: list(FieldSpec.t)) => {
  fields
  |> T.record_of_fields(~name="fieldsStatuses", ~loc, ~typ=field =>
       Typ.constr(
         Lident("fieldStatus") |> lid(~loc),
         [
           field.output_type |> FieldType.unpack,
           Typ.constr(Lident("message") |> lid(~loc), []),
         ],
       )
     );
};

let state_type = (~loc) => [%stri
  type state = {
    input,
    fieldsStatuses,
    formStatus: formStatus(submissionError),
    submissionStatus,
  }
];

let action_type = (~loc, fields: list(FieldSpec.t)) => {
  let update_actions =
    fields
    |> List.map((field: FieldSpec.t) =>
         field.id
         |> Field.update_action
         |> T.constructor(~args=[[%type: input]], ~loc)
       );
  let blur_actions =
    fields
    |> List.map((field: FieldSpec.t) =>
         field.id |> Field.blur_action |> T.constructor(~loc)
       );
  let rest_actions = [
    "Submit" |> T.constructor(~loc),
    "SetSubmittedStatus"
    |> T.constructor(~args=[[%type: option(input)]], ~loc),
    "SetSubmissionFailedStatus"
    |> T.constructor(~args=[[%type: submissionError]], ~loc),
    "MapSubmissionError"
    |> T.constructor(
         ~args=[[%type: submissionError => submissionError]],
         ~loc,
       ),
    "DismissSubmissionError" |> T.constructor(~loc),
    "DismissSubmissionResult" |> T.constructor(~loc),
    "Reset" |> T.constructor(~loc),
  ];

  "action"
  |> str(~loc)
  |> Type.mk(
       ~kind=
         Ptype_variant(
           rest_actions
           |> List.append(blur_actions)
           |> List.append(update_actions),
         ),
     )
  |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
};

let interface_type = (~loc, fields: list(FieldSpec.t)) => {
  let f = (x, t) => t |> Type.field(x |> str(~loc));

  let base = [
    f("input", [%type: input]),
    f("status", [%type: formStatus(submissionError)]),
    f("dirty", [%type: unit => bool]),
    f("valid", [%type: unit => bool]),
    f("submitting", [%type: bool]),
    f("submit", [%type: unit => unit]),
    f("dismissSubmissionError", [%type: unit => unit]),
    f("dismissSubmissionResult", [%type: unit => unit]),
    f(
      "mapSubmissionError",
      [%type: (submissionError => submissionError) => unit],
    ),
    f("reset", [%type: unit => unit]),
  ];

  let update_fns =
    fields
    |> List.map((field: FieldSpec.t) => {
         f(field.id |> Field.update_fn, [%type: input => unit])
       });

  let blur_fns =
    fields
    |> List.map((field: FieldSpec.t) => {
         f(field.id |> Field.blur_fn, [%type: unit => unit])
       });

  let result_fns =
    fields
    |> List.map((field: FieldSpec.t) => {
         f(
           field.id |> Field.result_fn,
           [%type:
             unit =>
             option(
               result([%t field.output_type |> FieldType.unpack], message),
             )
           ],
         )
       });

  "interface"
  |> str(~loc)
  |> Type.mk(
       ~kind=
         Ptype_record(
           base
           |> List.append(result_fns)
           |> List.append(blur_fns)
           |> List.append(update_fns),
         ),
     )
  |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
};

let initial_fields_statuses_fn = (~loc, fields: list(FieldSpec.t)) => {
  [%stri
    let initialFieldsStatuses = (_input: input) => [%e
      Exp.record(
        fields
        |> List.map((field: FieldSpec.t) =>
             (
               Lident(field.id |> Field.to_string) |> lid(~loc),
               [%expr Pristine],
             )
           ),
        None,
      )
    ]
  ];
};

let initial_state_fn = (~loc) => [%stri
  let initialState = input => {
    input,
    fieldsStatuses: input->initialFieldsStatuses,
    formStatus: Editing,
    submissionStatus: NeverSubmitted,
  }
];

let validate_form_fn = (~loc, fields: list(FieldSpec.t)) => {
  let field_result = x => (x |> Field.to_string) ++ "Result";
  let field_result_visibility = x =>
    (x |> Field.to_string) ++ "ResultVisibility";

  [%stri
    let validateForm =
        (input: input, ~validators: validators)
        : formValidationResult(output, fieldsStatuses) => [%e
      Exp.match(
        Exp.tuple(
          fields
          |> List.map((field: FieldSpec.t) =>
               switch (field.validator) {
               | `Required =>
                 %expr
                 {
                   let validator = [%e
                     field.id |> E.field(~of_="validators", ~loc)
                   ];
                   (validator.validate(input), Shown);
                 }
               | `Optional =>
                 switch%expr (
                   [%e field.id |> E.field(~of_="validators", ~loc)]
                 ) {
                 | Some(validator) => (validator.validate(input), Shown)
                 | None => (
                     Ok([%e field.id |> E.field(~of_="input", ~loc)]),
                     Hidden,
                   )
                 }
               }
             ),
        ),
        [
          // ((Ok(value), visibility), ...) => Ok(...)
          Exp.case(
            Pat.tuple(
              fields
              |> List.map((field: FieldSpec.t) =>
                   Pat.tuple([
                     Pat.alias(
                       Pat.construct(
                         ~attrs=[explicit_arity(~loc)],
                         Lident("Ok") |> lid(~loc),
                         Some(
                           Pat.tuple([
                             Pat.var(
                               field.id |> Field.to_string |> str(~loc),
                             ),
                           ]),
                         ),
                       ),
                       field.id |> field_result |> str(~loc),
                     ),
                     Pat.var(
                       field.id |> field_result_visibility |> str(~loc),
                     ),
                   ])
                 ),
            ),
            [%expr
              Ok({
                output: [%e
                  Exp.record(
                    fields
                    |> List.map((field: FieldSpec.t) =>
                         (
                           Lident(field.id |> Field.to_string) |> lid(~loc),
                           Exp.ident(
                             Lident(field.id |> Field.to_string) |> lid(~loc),
                           ),
                         )
                       ),
                    None,
                  )
                ],
                fieldsStatuses: [%e
                  Exp.record(
                    fields
                    |> List.map((field: FieldSpec.t) =>
                         (
                           Lident(field.id |> Field.to_string) |> lid(~loc),
                           [%expr
                             Dirty(
                               [%e
                                 Exp.ident(
                                   Lident(field.id |> field_result)
                                   |> lid(~loc),
                                 )
                               ],
                               [%e
                                 Exp.ident(
                                   Lident(field.id |> field_result_visibility)
                                   |> lid(~loc),
                                 )
                               ],
                             )
                           ],
                         )
                       ),
                    None,
                  )
                ],
              })
            ],
          ),
          // ((_, visibility), ...) => Error(...)
          Exp.case(
            Pat.tuple(
              fields
              |> List.map((field: FieldSpec.t) =>
                   Pat.tuple([
                     Pat.var(field.id |> field_result |> str(~loc)),
                     Pat.var(
                       field.id |> field_result_visibility |> str(~loc),
                     ),
                   ])
                 ),
            ),
            [%expr
              Error({
                fieldsStatuses: [%e
                  Exp.record(
                    fields
                    |> List.map((field: FieldSpec.t) =>
                         (
                           Lident(field.id |> Field.to_string) |> lid(~loc),
                           [%expr
                             Dirty(
                               [%e
                                 Exp.ident(
                                   Lident(field.id |> field_result)
                                   |> lid(~loc),
                                 )
                               ],
                               [%e
                                 Exp.ident(
                                   Lident(field.id |> field_result_visibility)
                                   |> lid(~loc),
                                 )
                               ],
                             )
                           ],
                         )
                       ),
                    None,
                  )
                ],
              })
            ],
          ),
        ],
      )
    ]
  ];
};

let use_form_fn = (~loc, fields: list(FieldSpec.t)) => [%stri
  let useForm =
      (
        ~initialInput: input,
        ~validators: validators,
        ~onSubmit:
           (output, submissionCallbacks(input, submissionError)) => unit,
      ) => {
    // Reducer
    let memoizedInitialState =
      React.useMemo1(() => initialInput->initialState, [|initialInput|]);

    let (state, dispatch) =
      memoizedInitialState->ReactUpdate.useReducer((state, action) => {
        %e
        {
          let update_actions =
            fields
            |> List.map((field: FieldSpec.t) =>
                 Exp.case(
                   Pat.construct(
                     ~attrs=[explicit_arity(~loc)],
                     Lident(field.id |> Field.update_action) |> lid(~loc),
                     Some(Pat.tuple([Pat.var("input" |> str(~loc))])),
                   ),
                   switch (field.deps) {
                   | [] =>
                     %expr
                     {
                       let {fieldsStatuses, submissionStatus} = state;
                       Update({
                         ...state,
                         input,
                         fieldsStatuses:
                           switch%e (field.validator) {
                           | `Required =>
                             %expr
                             {
                               validateFieldOnChangeWithValidator(
                                 ~input,
                                 ~fieldStatus=[%e
                                   field.id
                                   |> E.field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~submissionStatus,
                                 ~validator=[%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               );
                             }
                           | `Optional =>
                             switch%expr (
                               [%e
                                 field.id |> E.field(~of_="validators", ~loc)
                               ]
                             ) {
                             | Some(validator) =>
                               validateFieldOnChangeWithValidator(
                                 ~input,
                                 ~fieldStatus=[%e
                                   field.id
                                   |> E.field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~submissionStatus,
                                 ~validator,
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             | None =>
                               validateFieldOnChangeWithoutValidator(
                                 ~fieldInput=[%e
                                   field.id |> E.field(~of_="input", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             }
                           },
                       });
                     }
                   | [dep, ...deps] =>
                     %expr
                     {
                       let fieldsStatuses = ref(state.fieldsStatuses);
                       let {submissionStatus} = state;

                       %e
                       {
                         let validate_dep = dep => {
                           let field =
                             fields
                             |> List.find((field: FieldSpec.t) =>
                                  field.id |> Field.eq(dep)
                                );
                           switch (field.validator) {
                           | `Required =>
                             switch%expr (
                               validateFieldDependencyOnChange(
                                 ~input,
                                 ~fieldStatus=[%e
                                   field.id
                                   |> E.ref_field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~validator=[%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_ref_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             ) {
                             | Some(result) => fieldsStatuses := result
                             | None => ()
                             }
                           | `Optional =>
                             switch%expr (
                               [%e
                                 field.id |> E.field(~of_="validators", ~loc)
                               ]
                             ) {
                             | None => ()
                             | Some(validator) =>
                               switch (
                                 validateFieldDependencyOnChange(
                                   ~input,
                                   ~fieldStatus=[%e
                                     field.id
                                     |> E.ref_field(
                                          ~of_="fieldsStatuses",
                                          ~loc,
                                        )
                                   ],
                                   ~validator,
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_ref_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               ) {
                               | Some(result) => fieldsStatuses := result
                               | None => ()
                               }
                             }
                           };
                         };
                         deps
                         |> E.seq(
                              ~exp=dep |> validate_dep,
                              ~make=validate_dep,
                            );
                       };

                       Update({
                         ...state,
                         input,
                         fieldsStatuses:
                           switch%e (field.validator) {
                           | `Required =>
                             %expr
                             {
                               validateFieldOnChangeWithValidator(
                                 ~input,
                                 ~fieldStatus=[%e
                                   field.id
                                   |> E.ref_field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~submissionStatus,
                                 ~validator=[%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_ref_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               );
                             }
                           | `Optional =>
                             switch%expr (
                               [%e
                                 field.id |> E.field(~of_="validators", ~loc)
                               ]
                             ) {
                             | Some(validator) =>
                               validateFieldOnChangeWithValidator(
                                 ~input,
                                 ~fieldStatus=[%e
                                   field.id
                                   |> E.ref_field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~submissionStatus,
                                 ~validator,
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_ref_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             | None =>
                               validateFieldOnChangeWithoutValidator(
                                 ~fieldInput=[%e
                                   field.id |> E.field(~of_="input", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_ref_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             }
                           },
                       });
                     }
                   },
                 )
               );

          let blur_actions =
            fields
            |> List.map((field: FieldSpec.t) =>
                 Exp.case(
                   Pat.construct(
                     Lident(field.id |> Field.blur_action) |> lid(~loc),
                     None,
                   ),
                   {
                     %expr
                     {
                       let {input, fieldsStatuses} = state;
                       let result =
                         switch%e (field.validator) {
                         | `Required =>
                           %expr
                           validateFieldOnBlurWithValidator(
                             ~input,
                             ~fieldStatus=[%e
                               field.id
                               |> E.field(~of_="fieldsStatuses", ~loc)
                             ],
                             ~validator=[%e
                               field.id |> E.field(~of_="validators", ~loc)
                             ],
                             ~setStatus=[%e
                               [%expr
                                 status => [%e
                                   field.id
                                   |> E.update_field(
                                        ~of_="fieldsStatuses",
                                        ~with_=[%expr status],
                                        ~loc,
                                      )
                                 ]
                               ]
                             ],
                           )
                         | `Optional =>
                           switch%expr (
                             [%e field.id |> E.field(~of_="validators", ~loc)]
                           ) {
                           | Some(validator) =>
                             validateFieldOnBlurWithValidator(
                               ~input,
                               ~fieldStatus=[%e
                                 field.id
                                 |> E.field(~of_="fieldsStatuses", ~loc)
                               ],
                               ~validator,
                               ~setStatus=[%e
                                 [%expr
                                   status => [%e
                                     field.id
                                     |> E.update_field(
                                          ~of_="fieldsStatuses",
                                          ~with_=[%expr status],
                                          ~loc,
                                        )
                                   ]
                                 ]
                               ],
                             )
                           | None =>
                             validateFieldOnBlurWithoutValidator(
                               ~fieldInput=[%e
                                 field.id |> E.field(~of_="input", ~loc)
                               ],
                               ~fieldStatus=[%e
                                 field.id
                                 |> E.field(~of_="fieldsStatuses", ~loc)
                               ],
                               ~setStatus=[%e
                                 [%expr
                                   status => [%e
                                     field.id
                                     |> E.update_field(
                                          ~of_="fieldsStatuses",
                                          ~with_=[%expr status],
                                          ~loc,
                                        )
                                   ]
                                 ]
                               ],
                             )
                           }
                         };
                       switch (result) {
                       | Some(fieldsStatuses) =>
                         Update({...state, fieldsStatuses})
                       | None => NoUpdate
                       };
                     };
                   },
                 )
               );
          let rest_actions = [
            Exp.case(
              [%pat? Submit],
              switch%expr (state.formStatus) {
              | Submitting(_) => NoUpdate
              | Editing
              | Submitted
              | SubmissionFailed(_) =>
                switch (state.input->validateForm(~validators)) {
                | Ok({output, fieldsStatuses}) =>
                  UpdateWithSideEffects(
                    {
                      ...state,
                      fieldsStatuses,
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
                    ({dispatch}) =>
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
                | Error({fieldsStatuses}) =>
                  Update({
                    ...state,
                    fieldsStatuses,
                    formStatus: Editing,
                    submissionStatus: AttemptedToSubmit,
                  })
                }
              },
            ),
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
                Update({...state, formStatus: Submitting(Some(error->map))})
              | SubmissionFailed(error) =>
                Update({...state, formStatus: SubmissionFailed(error->map)})
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
              | SubmissionFailed(_) => Update({...state, formStatus: Editing})
              },
            ),
            Exp.case(
              [%pat? DismissSubmissionResult],
              switch%expr (state.formStatus) {
              | Editing
              | Submitting(_) => NoUpdate
              | Submitted
              | SubmissionFailed(_) => Update({...state, formStatus: Editing})
              },
            ),
            Exp.case(
              [%pat? Reset],
              [%expr Update(initialInput->initialState)],
            ),
          ];
          Exp.match(
            [%expr action],
            rest_actions
            |> List.append(blur_actions)
            |> List.append(update_actions),
          );
        }
      });

    // Interface
    %e
    {
      let base = [
        ("input", [%expr state.input]),
        ("status", [%expr state.formStatus]),
        (
          "dirty",
          [%expr
            () => [%e
              Exp.match(
                [%expr state.fieldsStatuses],
                [
                  Exp.case(
                    Pat.record(
                      fields
                      |> List.map((field: FieldSpec.t) =>
                           (
                             Lident(field.id |> Field.to_string) |> lid(~loc),
                             [%pat? Pristine],
                           )
                         ),
                      Closed,
                    ),
                    [%expr false],
                  ),
                  Exp.case([%pat? _], [%expr true]),
                ],
              )
            ]
          ],
        ),
        (
          "valid",
          [%expr
            () =>
              switch (state.input->validateForm(~validators)) {
              | Ok(_) => true
              | Error(_) => false
              }
          ],
        ),
        (
          "submitting",
          switch%expr (state.formStatus) {
          | Submitting(_) => true
          | Editing
          | Submitted
          | SubmissionFailed(_) => false
          },
        ),
        ("submit", [%expr () => Submit->dispatch]),
        (
          "mapSubmissionError",
          [%expr map => MapSubmissionError(map)->dispatch],
        ),
        (
          "dismissSubmissionError",
          [%expr () => DismissSubmissionError->dispatch],
        ),
        (
          "dismissSubmissionResult",
          [%expr () => DismissSubmissionResult->dispatch],
        ),
        ("reset", [%expr () => Reset->dispatch]),
      ];
      let update_fns =
        fields
        |> List.map((field: FieldSpec.t) => {
             (
               field.id |> Field.update_fn,
               [%expr
                 input =>
                   [%e
                     Exp.construct(
                       Lident(field.id |> Field.update_action) |> lid(~loc),
                       Some([%expr input]),
                     )
                   ]
                   ->dispatch
               ],
             )
           });
      let blur_fns =
        fields
        |> List.map((field: FieldSpec.t) => {
             (
               field.id |> Field.blur_fn,
               [%expr
                 () =>
                   [%e
                     Exp.construct(
                       Lident(field.id |> Field.blur_action) |> lid(~loc),
                       None,
                     )
                   ]
                   ->dispatch
               ],
             )
           });
      let result_fns =
        fields
        |> List.map((field: FieldSpec.t) => {
             (
               field.id |> Field.result_fn,
               [%expr
                 () => {
                   exposeFieldResult(
                     [%e
                       field.id
                       |> E.field2(~of_=("state", "fieldsStatuses"), ~loc)
                     ],
                   );
                 }
               ],
             )
           });

      E.record(
        ~loc,
        result_fns
        |> List.append(blur_fns)
        |> List.append(update_fns)
        |> List.append(base),
      );
    };
  }
];

let ext =
  Extension.declare(
    "form",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) => {
    switch (expr) {
    | PStr(structure) =>
      switch (structure |> Data.make) {
      | Ok(data) =>
        Mod.mk(
          Pmod_structure([
            open_formality(~loc),
            input_type(data.input_type),
            output_type(data.output_type),
            message_type(data.message_type),
            submission_error_type(data.submission_error_type),
            validators_type(~loc, data.fields),
            fields_statuses_type(~loc, data.fields),
            state_type(~loc),
            action_type(~loc, data.fields),
            interface_type(~loc, data.fields),
            initial_fields_statuses_fn(~loc, data.fields),
            initial_state_fn(~loc),
            validate_form_fn(~loc, data.fields),
            use_form_fn(~loc, data.fields),
          ]),
        )
      | Error(InputTypeParseError(NotFound)) =>
        Location.raise_errorf(~loc, "`input` type not found")
      | Error(InputTypeParseError(NotRecord(loc))) =>
        Location.raise_errorf(~loc, "`input` must be of record type")
      | Error(InputTypeParseError(InvalidFieldDeps(DepsParseError(loc)))) =>
        Location.raise_errorf(
          ~loc,
          "[@field.deps] attribute must contain field or tuple of fields",
        )
      | Error(
          InputTypeParseError(
            InvalidFieldDeps(DepNotFound(`Field(dep, loc))),
          ),
        ) =>
        Location.raise_errorf(~loc, "Field %s doesn't exist in input", dep)
      | Error(
          InputTypeParseError(
            InvalidFieldDeps(DepOfItself(`Field(dep, loc))),
          ),
        ) =>
        Location.raise_errorf(~loc, "Field can't depend on itself")
      | Error(
          InputTypeParseError(
            InvalidFieldDeps(DepDuplicate(`Field(dep, loc))),
          ),
        ) =>
        Location.raise_errorf(
          ~loc,
          "Field %s is already declared as a dependency for this field",
          dep,
        )
      | Error(OutputTypeParseError(NotFound)) =>
        Location.raise_errorf(~loc, "`output` type not found")
      | Error(OutputTypeParseError(NotRecord(loc))) =>
        Location.raise_errorf(
          ~loc,
          "`output` must be of record type or an alias of `input`",
        )
      | Error(OutputTypeParseError(BadTypeAlias({alias, loc}))) =>
        Location.raise_errorf(
          ~loc,
          "`output` can only be an alias of `input` type or a record",
        )
      | Error(IOMismatch(OutputFieldsNotInInput({fields}))) =>
        switch (fields) {
        | [] =>
          failwith(
            "Empty list of non-matched fields in IOMatchError(OutputFieldsNotInInput)",
          )
        | [(field, loc)]
        | [(field, loc), ..._] =>
          Location.raise_errorf(
            ~loc,
            "`output` field `%s` doesn't exist in `input` type",
            field |> Field.to_string,
          )
        }
      | Error(IOMismatch(InputFieldsNotInOutput({fields, loc})))
      | Error(
          IOMismatch(
            Both({
              input_fields_not_in_output: fields,
              output_fields_not_in_input: _,
              loc,
            }),
          ),
        ) =>
        switch (fields) {
        | [] =>
          failwith(
            "Empty list of non-matched fields in IOMatchError(Both)",
          )
        | [field] =>
          Location.raise_errorf(
            ~loc,
            "`input` field `%s` doesn't exist in `output` type",
            field |> Field.to_string,
          )
        | fields =>
          Location.raise_errorf(
            ~loc,
            "Some `input` fields don't exist in `output` type: %s",
            fields |> List.map(Field.to_string) |> String.concat(", "),
          )
        }
      }
    | _ => Location.raise_errorf(~loc, "Must be a structure")
    }
  });
