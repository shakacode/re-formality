open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

module Dirty = {
  type context =
    | FieldsOnly
    | CollectionsOnly({collections_cond: expression})
    | FieldsAndCollections({collections_cond: expression});

  let collection_cond = (~loc, {collection, fields}: Scheme.collection) => [%expr
    Belt.Array.every(
      [%e
        E.field2(~in_=("state", "fieldsStatuses"), ~loc, collection.plural)
      ],
      item => {
      %e
      Exp.match(
        [%expr item],
        [
          Exp.case(
            Pat.record(
              fields
              |> List.rev_map((field: Scheme.field) =>
                   (Lident(field.name) |> lid(~loc), [%pat? Pristine])
                 ),
              Closed,
            ),
            [%expr false],
          ),
          Exp.case(
            Pat.record(
              fields
              |> List.rev_map((field: Scheme.field) =>
                   (
                     Lident(field.name) |> lid(~loc),
                     switch (fields, field.validator) {
                     | ([_x], SyncValidator(_)) => [%pat? Dirty(_)]
                     | ([_x], AsyncValidator(_)) => [%pat?
                         Dirty(_) | Validating(_)
                       ]
                     | (_, SyncValidator(_)) => [%pat? Pristine | Dirty(_)]
                     | (_, AsyncValidator(_)) => [%pat?
                         Pristine | Dirty(_) | Validating(_)
                       ]
                     },
                   )
                 ),
              Closed,
            ),
            [%expr true],
          ),
        ],
      )
    })
  ];
};

let ast = (~scheme: Scheme.t, ~async: bool, ~loc) => {
  // NOTE: Temporary disabled due to bug in compiler
  // let dirty = {
  //   let fields = scheme |> Scheme.fields;
  //   let collections = scheme |> Scheme.collections;
  //
  //   let context =
  //     switch (fields, collections) {
  //     | ([_fields, ..._], []) => Dirty.FieldsOnly
  //     | ([], [collection, ...collections]) =>
  //       Dirty.CollectionsOnly({
  //         collections_cond:
  //           collections
  //           |> E.conj(
  //                ~loc,
  //                ~exp=Dirty.collection_cond(~loc, collection),
  //                ~make=Dirty.collection_cond,
  //              ),
  //       })
  //     | ([_fields, ..._], [collection, ...collections]) =>
  //       Dirty.FieldsAndCollections({
  //         collections_cond:
  //           collections
  //           |> E.conj(
  //                ~loc,
  //                ~exp=Dirty.collection_cond(~loc, collection),
  //                ~make=Dirty.collection_cond,
  //              ),
  //       })
  //     | ([], []) =>
  //       failwith(
  //         "No fields and no collections in the schema. Please, file an issue with your use-case.",
  //       )
  //     };
  //
  //   let no_case = {
  //     Exp.case(
  //       Pat.record(
  //         scheme
  //         |> List.rev
  //         |> List.rev_map((entry: Scheme.entry) =>
  //              switch (entry) {
  //              | Field(field) => (
  //                  Lident(field.name) |> lid(~loc),
  //                  [%pat? Pristine],
  //                )
  //              | Collection({collection}) => (
  //                  Lident(collection.plural) |> lid(~loc),
  //                  [%pat? _],
  //                )
  //              }
  //            ),
  //         Closed,
  //       ),
  //       [%expr false],
  //     );
  //   };
  //
  //   let yes_case = {
  //     Exp.case(
  //       Pat.record(
  //         scheme
  //         |> List.rev
  //         |> List.rev_map((entry: Scheme.entry) =>
  //              switch (entry) {
  //              | Field(field) => (
  //                  Lident(field.name) |> lid(~loc),
  //                  switch (
  //                    scheme
  //                    |> List.filter((entry: Scheme.entry) =>
  //                         switch (entry) {
  //                         | Field(_) => true
  //                         | Collection(_) => false
  //                         }
  //                       ),
  //                    field.validator,
  //                  ) {
  //                  | ([_x], SyncValidator(_)) => [%pat? Dirty(_)]
  //                  | ([_x], AsyncValidator(_)) => [%pat?
  //                      Dirty(_) | Validating(_)
  //                    ]
  //                  | (_, SyncValidator(_)) => [%pat? Pristine | Dirty(_)]
  //                  | (_, AsyncValidator(_)) => [%pat?
  //                      Pristine | Dirty(_) | Validating(_)
  //                    ]
  //                  },
  //                )
  //              | Collection({collection}) => (
  //                  Lident(collection.plural) |> lid(~loc),
  //                  [%pat? _],
  //                )
  //              }
  //            ),
  //         Closed,
  //       ),
  //       [%expr true],
  //     );
  //   };
  //
  //   let match_exp =
  //     Exp.match([%expr state.fieldsStatuses], [no_case, yes_case]);
  //
  //   %expr
  //   () =>
  //     switch%e (context) {
  //     | FieldsOnly => match_exp
  //     | CollectionsOnly({collections_cond}) => collections_cond
  //     | FieldsAndCollections({collections_cond}) =>
  //       if%expr ([%e collections_cond]) {
  //         true;
  //       } else {
  //         %e
  //         match_exp;
  //       }
  //     };
  // };

  let valid =
    if (async) {
      %expr
      () =>
        switch (
          state.input
          ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
        ) {
        | Validating(_) => None
        | Valid(_) => Some(true)
        | Invalid(_) => Some(false)
        };
    } else {
      %expr
      () =>
        switch (
          state.input
          ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
        ) {
        | Valid(_) => true
        | Invalid(_) => false
        };
    };

  let base = [
    ("input", [%expr state.input]),
    ("status", [%expr state.formStatus]),
    // ("dirty", dirty),
    ("valid", valid),
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
    ("mapSubmissionError", [%expr map => MapSubmissionError(map)->dispatch]),
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
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(field) => [
               (
                 FieldPrinter.update_fn(~field=field.name),
                 [%expr
                   (
                     (nextInputFn, nextValue) => {
                       [%e
                         Exp.construct(
                           Lident(
                             FieldPrinter.update_action(~field=field.name),
                           )
                           |> lid(~loc),
                           Some([%expr nextInputFn(_, nextValue)]),
                         )
                       ]
                       ->dispatch;
                     }
                   )
                 ],
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      (
                        FieldOfCollectionPrinter.update_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        [%expr
                          (~at as index, nextInputFn, nextValue) => {
                            [%e
                              Exp.construct(
                                Lident(
                                  FieldOfCollectionPrinter.update_action(
                                    ~collection,
                                    ~field=field.name,
                                  ),
                                )
                                |> lid(~loc),
                                Some(
                                  [%expr (nextInputFn(_, nextValue), index)],
                                ),
                              )
                            ]
                            ->dispatch;
                          }
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
               (
                 FieldPrinter.blur_fn(~field=field.name),
                 [%expr
                   (
                     () =>
                       [%e
                         Exp.construct(
                           Lident(
                             FieldPrinter.blur_action(~field=field.name),
                           )
                           |> lid(~loc),
                           None,
                         )
                       ]
                       ->dispatch
                   )
                 ],
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      (
                        FieldOfCollectionPrinter.blur_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        [%expr
                          (~at as index) =>
                            [%e
                              Exp.construct(
                                Lident(
                                  FieldOfCollectionPrinter.blur_action(
                                    ~collection,
                                    ~field=field.name,
                                  ),
                                )
                                |> lid(~loc),
                                Some([%expr index]),
                              )
                            ]
                            ->dispatch
                        ],
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
               (
                 FieldPrinter.result_value(~field=field.name),
                 switch (field.validator) {
                 | SyncValidator(_) =>
                   %expr
                   exposeFieldResult(
                     [%e
                       field.name
                       |> E.field2(~in_=("state", "fieldsStatuses"), ~loc)
                     ],
                   )
                 | AsyncValidator(_) =>
                   %expr
                   Async.exposeFieldResult(
                     [%e
                       field.name
                       |> E.field2(~in_=("state", "fieldsStatuses"), ~loc)
                     ],
                   )
                 },
               ),
               ...acc,
             ]
           | Collection({collection, fields}) =>
             fields
             |> List.fold_left(
                  (acc, field: Scheme.field) =>
                    [
                      (
                        FieldOfCollectionPrinter.result_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        switch (field.validator) {
                        | SyncValidator(_) =>
                          %expr
                          (
                            (~at as index) => {
                              exposeFieldResult(
                                [%e
                                  field.name
                                  |> E.field_of_collection2(
                                       ~in_=("state", "fieldsStatuses"),
                                       ~collection,
                                       ~loc,
                                     )
                                ],
                              );
                            }
                          )
                        | AsyncValidator(_) =>
                          %expr
                          (
                            (~at as index) => {
                              Async.exposeFieldResult(
                                [%e
                                  field.name
                                  |> E.field_of_collection2(
                                       ~in_=("state", "fieldsStatuses"),
                                       ~collection,
                                       ~loc,
                                     )
                                ],
                              );
                            }
                          )
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
           | Collection({collection, validator}) =>
             let add_fn = (
               collection |> CollectionPrinter.add_fn,
               [%expr
                 (
                   entry =>
                     [%e
                       Exp.construct(
                         Lident(collection |> CollectionPrinter.add_action)
                         |> lid(~loc),
                         Some([%expr entry]),
                       )
                     ]
                     ->dispatch
                 )
               ],
             );
             let remove_fn = (
               collection |> CollectionPrinter.remove_fn,
               [%expr
                 (
                   (~at as index) =>
                     [%e
                       Exp.construct(
                         Lident(collection |> CollectionPrinter.remove_action)
                         |> lid(~loc),
                         Some([%expr index]),
                       )
                     ]
                     ->dispatch
                 )
               ],
             );
             let result_value =
               switch (validator) {
               | Ok(Some ())
               | Error () =>
                 Some((
                   collection |> CollectionPrinter.result_value,
                   collection.plural
                   |> E.field2(~in_=("state", "collectionsStatuses"), ~loc),
                 ))
               | Ok(None) => None
               };

             switch (result_value) {
             | Some(result_value) => [
                 result_value,
                 remove_fn,
                 add_fn,
                 ...acc,
               ]
             | None => [remove_fn, add_fn, ...acc]
             };
           },
         [],
       );

  E.record(
    ~loc,
    base
    |> List.rev_append(collection_entries)
    |> List.rev_append(result_entries)
    |> List.rev_append(blur_fns)
    |> List.rev_append(update_fns),
  );
};
