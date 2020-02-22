open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~async: bool, ~loc) => {
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
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               f(
                 FieldPrinter.update_fn(~field=field.name),
                 [%type: input => unit],
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      f(
                        FieldOfCollectionPrinter.update_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        [%type: (input, ~at: index) => unit],
                      ),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let blur_fns =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               f(
                 FieldPrinter.blur_fn(~field=field.name),
                 [%type: unit => unit],
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      f(
                        FieldOfCollectionPrinter.blur_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        [%type: (~at: index) => unit],
                      ),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let result_fns =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               f(
                 FieldPrinter.result_fn(~field=field.name),
                 switch (field.validator) {
                 | SyncValidator(_) => [%type:
                     unit =>
                     option(
                       result(
                         [%t field.output_type |> ItemType.unpack],
                         message,
                       ),
                     )
                   ]
                 | AsyncValidator(_) => [%type:
                     unit =>
                     option(
                       Async.exposedFieldStatus(
                         [%t field.output_type |> ItemType.unpack],
                         message,
                       ),
                     )
                   ]
                 },
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      f(
                        FieldOfCollectionPrinter.result_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        switch (field.validator) {
                        | SyncValidator(_) => [%type:
                            (~at: index) =>
                            option(
                              result(
                                [%t field.output_type |> ItemType.unpack],
                                message,
                              ),
                            )
                          ]
                        | AsyncValidator(_) => [%type:
                            (~at: index) =>
                            option(
                              Async.exposedFieldStatus(
                                [%t field.output_type |> ItemType.unpack],
                                message,
                              ),
                            )
                          ]
                        },
                      ),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let collection_entries =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(_) => acc
           | Collection({collection, input_type}) => [
               f(
                 collection |> CollectionPrinter.add_fn,
                 [%type: [%t input_type |> ItemType.unpack] => unit],
               ),
               f(
                 collection |> CollectionPrinter.remove_fn,
                 [%type: (~at: index) => unit],
               ),
               f(
                 collection |> CollectionPrinter.result_value,
                 [%type: option(collectionStatus(message))],
               ),
               ...acc,
             ]
           },
         [],
       );

  Str.type_(
    ~loc,
    Recursive,
    [
      "interface"
      |> str(~loc)
      |> Type.mk(
           ~kind=
             Ptype_record(
               base
               |> List.append(collection_entries)
               |> List.append(result_fns)
               |> List.append(blur_fns)
               |> List.append(update_fns),
             ),
         ),
    ],
  );
};
