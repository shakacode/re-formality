open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  let field_result = x => (x |> Field.to_camelized_string) ++ "Result";
  let field_result_visibility = x =>
    (x |> Field.to_camelized_string) ++ "ResultVisibility";

  [%stri
    let validateForm =
        (
          input: input,
          ~validators: validators,
          ~fieldsStatuses: fieldsStatuses,
        )
        : formValidationResult(output, fieldsStatuses) => {
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
                   failwith(
                     "Form that supposed to be without async validators has one. Please, file an issue with yoour use-case.",
                   )
                 }
               ),
          );

        let ok_case =
          Exp.case(
            Pat.tuple(
              scheme
              |> List.map((entry: Scheme.entry) =>
                   switch (entry) {
                   | Field({name}) =>
                     Pat.tuple([
                       Pat.alias(
                         Pat.construct(
                           ~attrs=[explicit_arity(~loc)],
                           Lident("Ok") |> lid(~loc),
                           Some(Pat.tuple([Pat.var(name |> str(~loc))])),
                         ),
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
              })
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
              Invalid({
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
              })
            ],
          );

        Exp.match(match_values, [ok_case, error_case]);
      };
    }
  ];
};
