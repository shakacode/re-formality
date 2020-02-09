open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  let update_actions =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) =>
           Field.Field(name)
           |> Field.update_action
           |> T.constructor(~args=[[%type: input]], ~loc)
         }
       );

  let blur_actions =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) =>
           Field.Field(name) |> Field.blur_action |> T.constructor(~loc)
         }
       );

  let apply_async_result_actions =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field({validator: SyncValidator(_)}) => acc
           | Field({name, validator: AsyncValidator(_), output_type}) => [
               Field.Field(name)
               |> Field.apply_async_result_action
               |> T.constructor(
                    ~args=[
                      output_type |> FieldType.unpack,
                      Typ.constr(
                        Lident("result") |> lid(~loc),
                        [
                          output_type |> FieldType.unpack,
                          Typ.constr(Lident("message") |> lid(~loc), []),
                        ],
                      ),
                    ],
                    ~loc,
                  ),
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

  "action"
  |> str(~loc)
  |> Type.mk(
       ~kind=
         Ptype_variant(
           rest_actions
           |> List.append(apply_async_result_actions)
           |> List.append(blur_actions)
           |> List.append(update_actions),
         ),
     )
  |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
};
