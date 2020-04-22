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
                 [%type:
                   (
                     (input, [%t field.input_type |> ItemType.unpack]) => input,
                     [%t field.input_type |> ItemType.unpack]
                   ) =>
                   unit
                 ],
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
                        [%type:
                          (
                            ~at: index,
                            (
                              input,
                              [%t field.input_type |> ItemType.unpack]
                            ) =>
                            input,
                            [%t field.input_type |> ItemType.unpack]
                          ) =>
                          unit
                        ],
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

  let result_entries =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               f(
                 FieldPrinter.result_value(~field=field.name),
                 switch (field.validator) {
                 | SyncValidator(_) => [%type:
                     option(
                       result(
                         [%t field.output_type |> ItemType.unpack],
                         message,
                       ),
                     )
                   ]
                 | AsyncValidator(_) => [%type:
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
           | Collection({collection, validator, input_type}) =>
             let add_fn =
               f(
                 collection |> CollectionPrinter.add_fn,
                 [%type: [%t input_type |> ItemType.unpack] => unit],
               );
             let remove_fn =
               f(
                 collection |> CollectionPrinter.remove_fn,
                 [%type: (~at: index) => unit],
               );

             let result_value =
               switch (validator) {
               | Ok(Some ())
               | Error () =>
                 Some(
                   f(
                     collection |> CollectionPrinter.result_value,
                     [%type: option(collectionStatus(message))],
                   ),
                 )
               | Ok(None) => None
               };

             switch (result_value) {
             | Some(result_value) => [
                 add_fn,
                 remove_fn,
                 result_value,
                 ...acc,
               ]
             | None => [add_fn, remove_fn, ...acc]
             };
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
               |> List.append(result_entries)
               |> List.append(blur_fns)
               |> List.append(update_fns),
             ),
         ),
    ],
  );
};
