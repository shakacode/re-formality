open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let dirty_collection_guard = (~loc, {collection, fields}: Scheme.collection) => [%expr
  Belt.Array.every(
    [%e Exp.ident(Lident(collection.plural) |> lid(~loc))], item => {
    %e
    Exp.match(
      [%expr item],
      [
        Exp.case(
          Pat.record(
            fields
            |> List.rev
            |> List.rev_map((field: Scheme.field) =>
                 (Lident(field.name) |> lid(~loc), [%pat? Pristine])
               ),
            Closed,
          ),
          [%expr true],
        ),
        Exp.case(
          Pat.record(
            fields
            |> List.rev
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
          [%expr false],
        ),
      ],
    )
  })
];

let ast = (~scheme: Scheme.t, ~async: bool, ~loc) => {
  let collections = scheme |> Scheme.collections;

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
                  scheme
                  |> List.rev
                  |> List.rev_map((entry: Scheme.entry) =>
                       switch (entry) {
                       | Field(field) => (
                           Lident(field.name) |> lid(~loc),
                           [%pat? Pristine],
                         )
                       | Collection({collection}) => (
                           Lident(collection.plural) |> lid(~loc),
                           Pat.var(collection.plural |> str(~loc)),
                         )
                       }
                     ),
                  Closed,
                ),
                ~guard=?{
                  switch (collections) {
                  | [] => None
                  | [collection] =>
                    Some(dirty_collection_guard(~loc, collection))
                  | [collection, ...collections] =>
                    Some(
                      collections
                      |> E.conj(
                           ~loc,
                           ~exp=dirty_collection_guard(~loc, collection),
                           ~make=dirty_collection_guard,
                         ),
                    )
                  };
                },
                [%expr false],
              ),
              Exp.case(
                Pat.record(
                  scheme
                  |> List.rev
                  |> List.rev_map((entry: Scheme.entry) =>
                       switch (entry) {
                       | Field(field) => (
                           Lident(field.name) |> lid(~loc),
                           switch (scheme, field.validator) {
                           | ([_x], SyncValidator(_)) => [%pat? Dirty(_)]
                           | ([_x], AsyncValidator(_)) => [%pat?
                               Dirty(_) | Validating(_)
                             ]
                           | (_, SyncValidator(_)) => [%pat?
                               Pristine | Dirty(_)
                             ]
                           | (_, AsyncValidator(_)) => [%pat?
                               Pristine | Dirty(_) | Validating(_)
                             ]
                           },
                         )
                       | Collection({collection}) => (
                           Lident(collection.plural) |> lid(~loc),
                           [%pat? _],
                         )
                       }
                     ),
                  Closed,
                ),
                [%expr true],
              ),
            ],
          )
        ]
      ],
    ),
    (
      "valid",
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
      },
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
