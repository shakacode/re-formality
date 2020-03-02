open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let dirty_collection_guard =
    (~loc, (collection: Collection.t, fields: list(Scheme.field))) => [%expr
  Belt.Array.every(
    [%e Exp.ident(Lident(collection.plural) |> lid(~loc))], item => {
    %e
    Exp.match(
      [%expr item],
      [
        Exp.case(
          Pat.record(
            fields
            |> List.map((field: Scheme.field) =>
                 (Lident(field.name) |> lid(~loc), [%pat? Pristine])
               ),
            Closed,
          ),
          [%expr true],
        ),
        Exp.case([%pat? _], [%expr false]),
      ],
    )
  })
];

let ast = (~loc, ~async: bool, scheme: Scheme.t) => {
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
                  |> List.map((entry: Scheme.entry) =>
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
                  switch (
                    scheme
                    |> List.fold_left(
                         (acc, entry: Scheme.entry) =>
                           switch (entry) {
                           | Field(_) => acc
                           | Collection({collection, fields}) => [
                               (collection, fields),
                               ...acc,
                             ]
                           },
                         [],
                       )
                  ) {
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
              Exp.case([%pat? _], [%expr true]),
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
                     nextInputFn =>
                       [%e
                         Exp.construct(
                           Lident(
                             FieldPrinter.update_action(~field=field.name),
                           )
                           |> lid(~loc),
                           Some([%expr nextInputFn]),
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
                        FieldOfCollectionPrinter.update_fn(
                          ~collection,
                          ~field=field.name,
                        ),
                        [%expr
                          (nextInputFn, ~at as index) =>
                            [%e
                              Exp.construct(
                                Lident(
                                  FieldOfCollectionPrinter.update_action(
                                    ~collection,
                                    ~field=field.name,
                                  ),
                                )
                                |> lid(~loc),
                                Some([%expr (nextInputFn, index)]),
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
           | Collection({collection}) => [
               (
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
               ),
               (
                 collection |> CollectionPrinter.remove_fn,
                 [%expr
                   (
                     (~at as index) =>
                       [%e
                         Exp.construct(
                           Lident(
                             collection |> CollectionPrinter.remove_action,
                           )
                           |> lid(~loc),
                           Some([%expr index]),
                         )
                       ]
                       ->dispatch
                   )
                 ],
               ),
               (
                 collection |> CollectionPrinter.result_value,
                 collection.plural
                 |> E.field2(~in_=("state", "collectionsStatuses"), ~loc),
               ),
               ...acc,
             ]
           },
         [],
       );

  E.record(
    ~loc,
    base
    |> List.append(collection_entries)
    |> List.append(result_entries)
    |> List.append(blur_fns)
    |> List.append(update_fns),
  );
};
