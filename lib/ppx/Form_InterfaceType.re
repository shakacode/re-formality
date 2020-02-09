open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, ~async: bool, scheme: Scheme.t) => {
  let f = (x, t) => t |> Type.field(x |> str(~loc));

  let base = [
    f("input", [%type: input]),
    f("status", [%type: formStatus(submissionError)]),
    f("dirty", [%type: unit => bool]),
    f(
      "valid",
      async ? [%type: unit => option(bool)] : [%type: unit => bool],
    ),
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
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) =>
           f(Field.(Field(name) |> update_fn), [%type: input => unit])
         }
       );

  let blur_fns =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) =>
           f(Field.(Field(name) |> blur_fn), [%type: unit => unit])
         }
       );

  let result_fns =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name, validator, output_type}) =>
           f(
             Field.(Field(name) |> result_fn),
             switch (validator) {
             | SyncValidator(_) => [%type:
                 unit =>
                 option(
                   result([%t output_type |> FieldType.unpack], message),
                 )
               ]
             | AsyncValidator(_) => [%type:
                 unit =>
                 option(
                   Async.exposedFieldStatus(
                     [%t output_type |> FieldType.unpack],
                     message,
                   ),
                 )
               ]
             },
           )
         }
       );

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
