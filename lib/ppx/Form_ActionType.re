open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~loc) => {
  let update_actions =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               FieldPrinter.update_action(~field=field.name)
               |> T.constructor(~args=[[%type: input => input]], ~loc),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      FieldOfCollectionPrinter.update_action(
                        ~collection,
                        ~field=field.name,
                      )
                      |> T.constructor(
                           ~args=[[%type: input => input], [%type: index]],
                           ~loc,
                         ),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let blur_actions =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               FieldPrinter.blur_action(~field=field.name)
               |> T.constructor(~loc),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      FieldOfCollectionPrinter.blur_action(
                        ~collection,
                        ~field=field.name,
                      )
                      |> T.constructor(~args=[[%type: index]], ~loc),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let apply_async_result_actions =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field({validator: SyncValidator(_)}) => acc
           | Field({validator: AsyncValidator(_)} as field) => [
               FieldPrinter.apply_async_result_action(~field=field.name)
               |> T.constructor(
                    ~args=[
                      field.output_type |> ItemType.unpack,
                      Typ.constr(
                        Lident("result") |> lid(~loc),
                        [
                          field.output_type |> ItemType.unpack,
                          Typ.constr(Lident("message") |> lid(~loc), []),
                        ],
                      ),
                    ],
                    ~loc,
                  ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    switch (field) {
                    | {validator: SyncValidator(_)} => acc
                    | {validator: AsyncValidator(_)} as field => [
                        FieldOfCollectionPrinter.apply_async_result_action(
                          ~collection,
                          ~field=field.name,
                        )
                        |> T.constructor(
                             ~args=[
                               field.output_type |> ItemType.unpack,
                               [%type: index],
                               Typ.constr(
                                 Lident("result") |> lid(~loc),
                                 [
                                   field.output_type |> ItemType.unpack,
                                   Typ.constr(
                                     Lident("message") |> lid(~loc),
                                     [],
                                   ),
                                 ],
                               ),
                             ],
                             ~loc,
                           ),
                        ...acc,
                      ]
                    },
                  acc,
                )
           },
         [],
       );

  let collections_actions =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(_) => acc
           | Collection({collection, input_type}) => [
               collection
               |> CollectionPrinter.remove_action
               |> T.constructor(~args=[[%type: index]], ~loc),
               collection
               |> CollectionPrinter.add_action
               |> T.constructor(~args=[input_type |> ItemType.unpack], ~loc),
               ...acc,
             ]
           },
         [],
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

  Str.type_(
    ~loc,
    Recursive,
    [
      "action"
      |> str(~loc)
      |> Type.mk(
           ~kind=
             Ptype_variant(
               rest_actions
               |> List.rev_append(collections_actions)
               |> List.rev_append(apply_async_result_actions)
               |> List.rev_append(blur_actions)
               |> List.rev_append(update_actions),
             ),
         ),
    ],
  );
};
