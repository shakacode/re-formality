open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  let field_result = x => (x |> Field.to_camelized_string) ++ "Result";
  let field_result_visibility = x =>
    (x |> Field.to_camelized_string) ++ "ResultVisibility";

  // We are going to pattern match against validation results of each field
  [%stri
    let validateForm =
        (
          input: input,
          ~validators: validators,
          ~fieldsStatuses: fieldsStatuses,
        )
        : option(formValidationResult(output, fieldsStatuses)) => {
      %e
      {
        let match_values =
          Exp.tuple(
            scheme
            |> List.map((entry: Scheme.entry) =>
                 switch (entry) {
                 | Field({
                     name,
                     validator:
                       SyncValidator(
                         Ok(Required | Optional(Some(_))) | Error (),
                       ),
                   }) =>
                   %expr
                   {
                     (
                       switch (
                         [%e
                           Field.Field(name)
                           |> E.field(~of_="fieldsStatuses", ~loc)
                         ]
                       ) {
                       | Pristine =>
                         let validator = [%e
                           Field.Field(name)
                           |> E.field(~of_="validators", ~loc)
                         ];
                         validator.validate(input);
                       | Dirty(result, _) => result
                       },
                       Shown,
                     );
                   }
                 | Field({
                     name,
                     validator: SyncValidator(Ok(Optional(None))),
                   }) =>
                   %expr
                   (
                     Ok(
                       [%e Field.Field(name) |> E.field(~of_="input", ~loc)],
                     ),
                     Hidden,
                   )
                 | Field({name, validator: AsyncValidator(_)}) =>
                   %expr
                   {
                     (
                       switch (
                         [%e
                           Field.Field(name)
                           |> E.field(~of_="fieldsStatuses", ~loc)
                         ]
                       ) {
                       | Validating(x) => `Validating(x)
                       | Pristine =>
                         // If field is not touched, it either "empty" or has initial input
                         // If async field optional, then empty state is valid
                         // If it has initial value, in general it's from a server, hence valid
                         // If it's not from server and sync validator returned OK() but value is invalid,
                         // it should be rejected by the server on submit anyway
                         // So it doesn't worth to do 2+ requests on submission
                         let validator = [%e
                           Field.Field(name)
                           |> E.field(~of_="validators", ~loc)
                         ];
                         `Result(validator.validate(input));
                       | Dirty(result, _) =>
                         // This field was updated by user so all its validators already run
                         `Result(result)
                       },
                       Shown,
                     );
                   }
                 }
               ),
          );
        let validating_cases =
          scheme
          |> List.fold_left(
               (acc, entry: Scheme.entry) =>
                 switch (entry) {
                 | Field({validator: SyncValidator(_)}) => acc
                 | Field({name, validator: AsyncValidator(_)}) => [
                     Exp.case(
                       Pat.tuple(
                         scheme
                         |> List.map((entry: Scheme.entry) =>
                              switch (entry) {
                              | Field({name: name'}) when name == name' => [%pat?
                                  (`Validating(_), _)
                                ]
                              | Field(_) => [%pat? (_, _)]
                              }
                            ),
                       ),
                       [%expr None],
                     ),
                     ...acc,
                   ]
                 },
               [],
             );

        let ok_case =
          Exp.case(
            Pat.tuple(
              scheme
              |> List.map((entry: Scheme.entry) =>
                   switch (entry) {
                   | Field({name, validator: SyncValidator(_)}) => [%pat?
                       (
                         [%p
                           Pat.alias(
                             Pat.construct(
                               ~attrs=[explicit_arity(~loc)],
                               Lident("Ok") |> lid(~loc),
                               Some(
                                 Pat.tuple([Pat.var(name |> str(~loc))]),
                               ),
                             ),
                             Field.Field(name) |> field_result |> str(~loc),
                           )
                         ],
                         [%p
                           Pat.var(
                             Field.Field(name)
                             |> field_result_visibility
                             |> str(~loc),
                           )
                         ],
                       )
                     ]
                   | Field({name, validator: AsyncValidator(_)}) => [%pat?
                       (
                         [%p
                           Pat.variant(
                             ~attrs=[explicit_arity(~loc)],
                             "Result",
                             Some(
                               Pat.alias(
                                 Pat.construct(
                                   ~attrs=[explicit_arity(~loc)],
                                   Lident("Ok") |> lid(~loc),
                                   Some(
                                     Pat.tuple([
                                       Pat.var(name |> str(~loc)),
                                     ]),
                                   ),
                                 ),
                                 Field.Field(name)
                                 |> field_result
                                 |> str(~loc),
                               ),
                             ),
                           )
                         ],
                         [%p
                           Pat.var(
                             Field.Field(name)
                             |> field_result_visibility
                             |> str(~loc),
                           )
                         ],
                       )
                     ]
                   }
                 ),
            ),
            [%expr
              Some(
                Valid({
                  output: [%e
                    Exp.record(
                      scheme
                      |> List.map((entry: Scheme.entry) =>
                           switch (entry) {
                           | Field({name}) => (
                               Lident(name) |> lid(~loc),
                               Exp.ident(Lident(name) |> lid(~loc)),
                             )
                           }
                         ),
                      None,
                    )
                  ],
                  fieldsStatuses: [%e
                    Exp.record(
                      scheme
                      |> List.map((entry: Scheme.entry) =>
                           switch (entry) {
                           | Field({name}) => (
                               Lident(name) |> lid(~loc),
                               [%expr
                                 Dirty(
                                   [%e
                                     Exp.ident(
                                       Lident(
                                         Field.Field(name) |> field_result,
                                       )
                                       |> lid(~loc),
                                     )
                                   ],
                                   [%e
                                     Exp.ident(
                                       Lident(
                                         Field.Field(name)
                                         |> field_result_visibility,
                                       )
                                       |> lid(~loc),
                                     )
                                   ],
                                 )
                               ],
                             )
                           }
                         ),
                      None,
                    )
                  ],
                }),
              )
            ],
          );

        let error_case =
          Exp.case(
            Pat.tuple(
              scheme
              |> List.map((entry: Scheme.entry) =>
                   switch (entry) {
                   | Field({name}) =>
                     Pat.tuple([
                       Pat.var(
                         Field.Field(name) |> field_result |> str(~loc),
                       ),
                       Pat.var(
                         Field.Field(name)
                         |> field_result_visibility
                         |> str(~loc),
                       ),
                     ])
                   }
                 ),
            ),
            [%expr
              Some(
                Invalid({
                  fieldsStatuses: [%e
                    Exp.record(
                      scheme
                      |> List.map((entry: Scheme.entry) =>
                           switch (entry) {
                           | Field({name, validator: SyncValidator(_)}) => (
                               Lident(name) |> lid(~loc),
                               [%expr
                                 Dirty(
                                   [%e
                                     Exp.ident(
                                       Lident(
                                         Field.Field(name) |> field_result,
                                       )
                                       |> lid(~loc),
                                     )
                                   ],
                                   [%e
                                     Exp.ident(
                                       Lident(
                                         Field.Field(name)
                                         |> field_result_visibility,
                                       )
                                       |> lid(~loc),
                                     )
                                   ],
                                 )
                               ],
                             )
                           | Field({name, validator: AsyncValidator(_)}) => (
                               Lident(name) |> lid(~loc),
                               switch%expr (
                                 [%e
                                   Exp.ident(
                                     Lident(
                                       Field.Field(name) |> field_result,
                                     )
                                     |> lid(~loc),
                                   )
                                 ]
                               ) {
                               | `Validating(x) => Validating(x)
                               | `Result(result) =>
                                 Dirty(
                                   result,
                                   [%e
                                     Exp.ident(
                                       Lident(
                                         Field.Field(name)
                                         |> field_result_visibility,
                                       )
                                       |> lid(~loc),
                                     )
                                   ],
                                 )
                               },
                             )
                           }
                         ),
                      None,
                    )
                  ],
                }),
              )
            ],
          );

        Exp.match(
          match_values,
          List.append(validating_cases, [ok_case, error_case]),
        );
      };
    }
  ];
};
