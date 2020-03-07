open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let field_result_var = (~field: string) => field ++ "Result";
let field_result_visibility_var = (~field: string) =>
  field ++ "ResultVisibility";
let fields_of_collection_result_var = (collection: Collection.t) =>
  collection.plural ++ "CollectionFieldsResult";
let whole_collection_result_var = (collection: Collection.t) =>
  collection.plural ++ "CollectionResult";
let collection_fields_statuses_var = (collection: Collection.t) =>
  collection.plural ++ "CollectionFieldsStatuses";

let validate_field_without_validator = (~field: Scheme.field, ~loc) => [%expr
  (Ok([%e field.name |> E.field(~in_="input", ~loc)]), Hidden)
];

let validate_field_of_collection_without_validator =
    (~collection: Collection.t, ~field: Scheme.field, ~loc) => [%expr
  (
    Ok(
      [%e
        Exp.field(
          [%expr
            Array.getUnsafe(
              [%e collection.plural |> E.field(~in_="input", ~loc)],
              index,
            )
          ],
          Lident(field.name) |> lid(~loc),
        )
      ],
    ),
    Hidden,
  )
];

let validate_field_with_sync_validator = (~field: Scheme.field, ~loc) => [%expr
  (
    switch ([%e field.name |> E.field(~in_="fieldsStatuses", ~loc)]) {
    | Pristine =>
      %e
      E.apply_field2(
        ~in_=("validators", field.name),
        ~fn="validate",
        ~args=[(Nolabel, [%expr input])],
        ~loc,
      )
    | Dirty(result, _) => result
    },
    Shown,
  )
];

let validate_field_of_collection_with_sync_validator =
    (~field: Scheme.field, ~collection: Collection.t, ~loc) => [%expr
  (
    switch ([%e field.name |> E.field(~in_="fieldStatus", ~loc)]) {
    | Pristine =>
      %e
      E.apply_field4(
        ~in_=("validators", collection.plural, "fields", field.name),
        ~fn="validate",
        ~args=[(Nolabel, [%expr input]), (Labelled("at"), [%expr index])],
        ~loc,
      )
    | Dirty(result, _) => result
    },
    Shown,
  )
];

let validate_field_with_async_validator = (~field: Scheme.field, ~loc) => [%expr
  (
    switch ([%e field.name |> E.field(~in_="fieldsStatuses", ~loc)]) {
    | Validating(value) => `Validating(value)
    | Pristine =>
      // If field is not touched, it either "empty" or has initial input
      // If async field optional, then empty state is valid
      // If it has initial value, in general it's from a server, hence valid
      // If it's not from server and sync validator returned OK() but value is invalid,
      // it should be rejected by the server on submit anyway
      // So it doesn't worth to do 2+ requests on submission
      `Result(
        [%e
          E.apply_field2(
            ~in_=("validators", field.name),
            ~fn="validate",
            ~args=[(Nolabel, [%expr input])],
            ~loc,
          )
        ],
      )
    | Dirty(result, _) =>
      // This field was updated by user so all its validators already run
      `Result(result)
    },
    Shown,
  )
];

let validate_field_of_collection_with_async_validator =
    (~field: Scheme.field, ~collection: Collection.t, ~loc) => [%expr
  (
    switch ([%e field.name |> E.field(~in_="fieldStatus", ~loc)]) {
    | Validating(value) => `Validating(value)
    | Dirty(result, _) => `Result(result)
    | Pristine =>
      `Result(
        [%e
          E.apply_field4(
            ~in_=("validators", collection.plural, "fields", field.name),
            ~fn="validate",
            ~args=[
              (Nolabel, [%expr input]),
              (Labelled("at"), [%expr index]),
            ],
            ~loc,
          )
        ],
      )
    },
    Shown,
  )
];

let validate_whole_collection = (~collection: Collection.t, ~loc) =>
  E.apply_field2(
    ~in_=("validators", collection.plural),
    ~fn="collection",
    ~args=[(Nolabel, [%expr input])],
    ~loc,
  );

let ok_pat_for_sync_field = (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.alias(
      Pat.construct(
        ~attrs=[explicit_arity(~loc)],
        Lident("Ok") |> lid(~loc),
        Some(Pat.tuple([Pat.var(field.name |> str(~loc))])),
      ),
      field_result_var(~field=field.name) |> str(~loc),
    ),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let ok_pat_for_async_field = (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.variant(
      "Result",
      Some(
        Pat.alias(
          Pat.construct(
            ~attrs=[explicit_arity(~loc)],
            Lident("Ok") |> lid(~loc),
            Some(Pat.tuple([Pat.var(field.name |> str(~loc))])),
          ),
          field_result_var(~field=field.name) |> str(~loc),
        ),
      ),
    ),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let ok_pat_for_fields_of_async_collection = (~loc, collection: Collection.t) =>
  Pat.variant(
    "FieldsOfCollectionResult",
    Some(
      Pat.tuple([
        Pat.alias(
          Pat.construct(
            ~attrs=[explicit_arity(~loc)],
            Lident("Ok") |> lid(~loc),
            Some(Pat.tuple([Pat.var(collection.plural |> str(~loc))])),
          ),
          collection |> fields_of_collection_result_var |> str(~loc),
        ),
        Pat.var(collection |> collection_fields_statuses_var |> str(~loc)),
      ]),
    ),
  );

let ok_pat_for_collection = (~loc, collection: Collection.t) =>
  Pat.alias(
    Pat.construct(
      ~attrs=[explicit_arity(~loc)],
      Lident("Ok") |> lid(~loc),
      Some([%pat? ()]),
    ),
    collection |> whole_collection_result_var |> str(~loc),
  );

let ok_pat_for_fields_of_collection = (~loc, collection: Collection.t) =>
  Pat.tuple([
    Pat.construct(
      ~attrs=[explicit_arity(~loc)],
      Lident("Ok") |> lid(~loc),
      Some(Pat.tuple([Pat.var(collection.plural |> str(~loc))])),
    ),
    Pat.var(collection |> collection_fields_statuses_var |> str(~loc)),
  ]);

let result_pat_for_collection = (~loc, collection: Collection.t) =>
  Pat.var(collection |> whole_collection_result_var |> str(~loc));

let error_pat_for_fields_of_collection = (~loc, collection: Collection.t) =>
  Pat.tuple([
    Pat.any(),
    Pat.var(collection |> collection_fields_statuses_var |> str(~loc)),
  ]);

let error_pat_for_sync_field_in_single_field_form =
    (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.alias(
      Pat.construct(
        ~attrs=[explicit_arity(~loc)],
        Lident("Error") |> lid(~loc),
        Some([%pat? _]),
      ),
      field_result_var(~field=field.name) |> str(~loc),
    ),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let error_pat_for_sync_field_in_multi_field_form = (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.or_(
      Pat.alias(
        Pat.construct(
          ~attrs=[explicit_arity(~loc)],
          Lident("Ok") |> lid(~loc),
          Some([%pat? _]),
        ),
        field_result_var(~field=field.name) |> str(~loc),
      ),
      Pat.alias(
        Pat.construct(
          ~attrs=[explicit_arity(~loc)],
          Lident("Error") |> lid(~loc),
          Some([%pat? _]),
        ),
        field_result_var(~field=field.name) |> str(~loc),
      ),
    ),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let result_and_visibility_pat_for_field = (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.var(field_result_var(~field=field.name) |> str(~loc)),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let result_and_visibility_pat_for_async_field = (~loc, field: Scheme.field) =>
  Pat.tuple([
    Pat.variant(
      "Result",
      Some(Pat.var(field_result_var(~field=field.name) |> str(~loc))),
    ),
    Pat.var(field_result_visibility_var(~field=field.name) |> str(~loc)),
  ]);

let result_pat_for_fields_of_collection = (~loc, collection: Collection.t) =>
  Pat.var(collection |> fields_of_collection_result_var |> str(~loc));

let fields_statuses_pat_for_async_collection =
    (~loc, collection: Collection.t) =>
  Pat.variant(
    "FieldsOfCollectionResult",
    Some(
      Pat.tuple([
        Pat.any(),
        Pat.var(collection |> collection_fields_statuses_var |> str(~loc)),
      ]),
    ),
  );

let output_field_record_field = (~loc, field: Scheme.field) => (
  Lident(field.name) |> lid(~loc),
  Exp.ident(Lident(field.name) |> lid(~loc)),
);

let output_collection_record_field = (~loc, collection: Collection.t) => (
  Lident(collection.plural) |> lid(~loc),
  Exp.ident(Lident(collection.plural) |> lid(~loc)),
);

let field_dirty_status_record_field = (~loc, field: Scheme.field) => (
  Lident(field.name) |> lid(~loc),
  [%expr
    Dirty(
      [%e
        Exp.ident(Lident(field_result_var(~field=field.name)) |> lid(~loc))
      ],
      [%e
        Exp.ident(
          Lident(field_result_visibility_var(~field=field.name))
          |> lid(~loc),
        )
      ],
    )
  ],
);

let async_field_dirty_or_validating_status_record_field =
    (~loc, field: Scheme.field) => (
  Lident(field.name) |> lid(~loc),
  switch%expr (
    [%e
      Exp.ident(Lident(field_result_var(~field=field.name)) |> lid(~loc))
    ]
  ) {
  | `Validating(value) => Validating(value)
  | `Result(result) =>
    Dirty(
      result,
      [%e
        Exp.ident(
          Lident(field_result_visibility_var(~field=field.name))
          |> lid(~loc),
        )
      ],
    )
  },
);

let collection_that_might_be_in_validating_state_status_record_field =
    (~loc, collection: Collection.t) => (
  Lident(collection.plural) |> lid(~loc),
  switch%expr (
    [%e
      Exp.ident(
        Lident(collection |> fields_of_collection_result_var) |> lid(~loc),
      )
    ]
  ) {
  | `ValidatingFieldsOfCollection(statuses) => statuses
  | `FieldsOfCollectionResult(_, statuses) => statuses
  },
);

let collection_statuses_record_field = (~loc, collection: Collection.t) => (
  Lident(collection.plural) |> lid(~loc),
  Exp.ident(
    Lident(collection |> collection_fields_statuses_var) |> lid(~loc),
  ),
);

let collections_statuses_record =
    (~loc, collections: list(Scheme.collection)) =>
  Exp.record(
    collections
    |> List.map(({collection, validator}: Scheme.collection) =>
         (
           Lident(collection.plural) |> lid(~loc),
           switch (validator) {
           | Ok(Some ())
           | Error () =>
             %expr
             Some(
               [%e
                 Exp.ident(
                   Lident(collection |> whole_collection_result_var)
                   |> lid(~loc),
                 )
               ],
             )
           | Ok(None) =>
             %expr
             ()
           },
         )
       ),
    None,
  );

let validate_fields_of_collection_in_sync_form =
    (
      ~collection: Collection.t,
      ~fields: list(Scheme.field),
      ~output_type: ItemType.t,
      ~loc: Location.t,
    ) => {
  let match_values =
    Exp.tuple([
      [%expr output],
      ...fields
         |> List.map((field: Scheme.field) =>
              switch (field.validator) {
              | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
                validate_field_of_collection_with_sync_validator(
                  ~collection,
                  ~field,
                  ~loc,
                )
              | SyncValidator(Ok(Optional(None))) =>
                validate_field_of_collection_without_validator(
                  ~collection,
                  ~field,
                  ~loc,
                )
              | AsyncValidator(_) =>
                failwith(
                  "Form that supposed to be without async validators has one. Please, file an issue with yoour use-case.",
                )
              }
            ),
    ]);

  let ok_case =
    Exp.case(
      Pat.tuple([
        Pat.construct(
          ~attrs=[explicit_arity(~loc)],
          Lident("Ok") |> lid(~loc),
          Some(Pat.tuple([Pat.var("output" |> str(~loc))])),
        ),
        ...fields |> List.map(ok_pat_for_sync_field(~loc)),
      ]),
      [%expr
        {
          ignore(
            Js.Array2.push(
              output,
              [%e
                Exp.record(
                  fields |> List.map(output_field_record_field(~loc)),
                  None,
                )
              ],
            ),
          );
          ignore(
            Js.Array2.push(
              statuses,
              [%e
                Exp.record(
                  fields |> List.map(field_dirty_status_record_field(~loc)),
                  None,
                )
              ],
            ),
          );
          (Ok(output), statuses);
        }
      ],
    );

  let error_case =
    Exp.case(
      Pat.tuple([
        [%pat? Ok(_) | Error(_)],
        ...fields |> List.map(result_and_visibility_pat_for_field(~loc)),
      ]),
      [%expr
        {
          ignore(
            Js.Array2.push(
              statuses,
              [%e
                Exp.record(
                  fields |> List.map(field_dirty_status_record_field(~loc)),
                  None,
                )
              ],
            ),
          );
          (Error(), statuses);
        }
      ],
    );

  %expr
  {
    Belt.Array.reduceWithIndex(
      [%e collection.plural |> E.field(~in_="fieldsStatuses", ~loc)],
      (Ok([||]), [||]),
      (
        (
          output: result(array([%t output_type |> ItemType.unpack]), unit),
          statuses:
            array(
              [%t
                Typ.constr(
                  Lident(collection |> CollectionPrinter.fields_statuses_type)
                  |> lid(~loc),
                  [],
                )
              ],
            ),
        ),
        fieldStatus,
        index,
      ) => {
      %e
      Exp.match(match_values, [ok_case, error_case])
    });
  };
};

let validate_fields_of_collection_in_async_form =
    (
      ~collection: Collection.t,
      ~fields: list(Scheme.field),
      ~output_type: ItemType.t,
      ~loc: Location.t,
    ) => {
  let fields_statuses_type =
    Typ.constr(
      Lident(collection |> CollectionPrinter.fields_statuses_type)
      |> lid(~loc),
      [],
    );

  let match_values =
    Exp.tuple([
      [%expr result],
      ...fields
         |> List.map((field: Scheme.field) =>
              switch (field.validator) {
              | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
                validate_field_of_collection_with_sync_validator(
                  ~collection,
                  ~field,
                  ~loc,
                )
              | SyncValidator(Ok(Optional(None))) =>
                validate_field_of_collection_without_validator(
                  ~collection,
                  ~field,
                  ~loc,
                )
              | AsyncValidator(_) =>
                validate_field_of_collection_with_async_validator(
                  ~collection,
                  ~field,
                  ~loc,
                )
              }
            ),
    ]);

  let validating_case =
    Exp.case(
      P.or_(
        ~pat=
          Pat.tuple([
            [%pat? `ValidatingFieldsOfCollection(statuses)],
            ...fields |> List.map(result_and_visibility_pat_for_field(~loc)),
          ]),
        ~make=
          (field: Scheme.field) =>
            Pat.tuple([
              [%pat? `FieldsOfCollectionResult(_, statuses)],
              ...fields
                 |> List.map((field': Scheme.field) =>
                      if (field'.name == field.name) {
                        Pat.tuple([
                          Pat.alias(
                            Pat.variant("Validating", Some(Pat.any())),
                            field_result_var(~field=field.name) |> str(~loc),
                          ),
                          Pat.var(
                            field_result_visibility_var(~field=field.name)
                            |> str(~loc),
                          ),
                        ]);
                      } else {
                        field' |> result_and_visibility_pat_for_field(~loc);
                      }
                    ),
            ]),
        fields
        |> List.filter((field: Scheme.field) =>
             switch (field.validator) {
             | SyncValidator(_) => false
             | AsyncValidator(_) => true
             }
           ),
      ),
      [%expr
        {
          ignore(
            Js.Array2.push(
              statuses,
              [%e
                Exp.record(
                  fields
                  |> List.map((field: Scheme.field) =>
                       switch (field.validator) {
                       | SyncValidator(_) =>
                         field |> field_dirty_status_record_field(~loc)
                       | AsyncValidator(_) =>
                         field
                         |> async_field_dirty_or_validating_status_record_field(
                              ~loc,
                            )
                       }
                     ),
                  None,
                )
              ],
            ),
          );
          `ValidatingFieldsOfCollection(statuses);
        }
      ],
    );

  let ok_case =
    Exp.case(
      Pat.tuple([
        [%pat? `FieldsOfCollectionResult(Ok(output), statuses)],
        ...fields
           |> List.map((field: Scheme.field) =>
                switch (field.validator) {
                | SyncValidator(_) => field |> ok_pat_for_sync_field(~loc)
                | AsyncValidator(_) => field |> ok_pat_for_async_field(~loc)
                }
              ),
      ]),
      [%expr
        {
          ignore(
            Js.Array2.push(
              output,
              [%e
                Exp.record(
                  fields |> List.map(output_field_record_field(~loc)),
                  None,
                )
              ],
            ),
          );
          ignore(
            Js.Array2.push(
              statuses,
              [%e
                Exp.record(
                  fields |> List.map(field_dirty_status_record_field(~loc)),
                  None,
                )
              ],
            ),
          );
          `FieldsOfCollectionResult((Ok(output), statuses));
        }
      ],
    );

  let error_case =
    Exp.case(
      Pat.tuple([
        [%pat? `FieldsOfCollectionResult(_, statuses)],
        ...fields |> List.map(result_and_visibility_pat_for_field(~loc)),
      ]),
      [%expr
        {
          ignore(
            Js.Array2.push(
              statuses,
              [%e
                Exp.record(
                  fields
                  |> List.map((field: Scheme.field) =>
                       switch (field.validator) {
                       | SyncValidator(_) =>
                         field |> field_dirty_status_record_field(~loc)
                       | AsyncValidator(_) =>
                         field
                         |> async_field_dirty_or_validating_status_record_field(
                              ~loc,
                            )
                       }
                     ),
                  None,
                )
              ],
            ),
          );
          `FieldsOfCollectionResult((Error(), statuses));
        }
      ],
    );

  %expr
  {
    Belt.Array.reduceWithIndex(
      [%e collection.plural |> E.field(~in_="fieldsStatuses", ~loc)],
      `FieldsOfCollectionResult((Ok([||]), [||])),
      (
        result: [
          | `ValidatingFieldsOfCollection(array([%t fields_statuses_type]))
          | `FieldsOfCollectionResult(
              result(array([%t output_type |> ItemType.unpack]), unit),
              array([%t fields_statuses_type]),
            )
        ],
        fieldStatus,
        index,
      ) => {
      %e
      Exp.match(match_values, [validating_case, ok_case, error_case])
    });
  };
};

module Sync = {
  let ast = (~scheme: Scheme.t, ~loc) => {
    [%stri
      let validateForm =
          (
            input: input,
            ~validators: validators,
            ~fieldsStatuses: fieldsStatuses,
          )
          : formValidationResult(output, fieldsStatuses, collectionsStatuses) => {
        %e
        {
          let collections = scheme |> Scheme.collections;

          let match_values = {
            let value = (entry: Scheme.entry) =>
              switch (entry) {
              | Field(
                  {
                    validator:
                      SyncValidator(
                        Ok(Required | Optional(Some(_))) | Error (),
                      ),
                  } as field,
                ) =>
                validate_field_with_sync_validator(~field, ~loc)
              | Field(
                  {validator: SyncValidator(Ok(Optional(None)))} as field,
                ) =>
                validate_field_without_validator(~field, ~loc)
              | Field({name, validator: AsyncValidator(_)}) =>
                failwith(
                  "Form that supposed to be without async validators has one. Please, file an issue with yoour use-case.",
                )
              | Collection({collection, fields, validator, output_type}) =>
                switch (validator) {
                | Ok(Some ())
                | Error () =>
                  %expr
                  (
                    [%e validate_whole_collection(~collection, ~loc)],
                    [%e
                      validate_fields_of_collection_in_sync_form(
                        ~collection,
                        ~fields,
                        ~output_type,
                        ~loc,
                      )
                    ],
                  )
                | Ok(None) =>
                  validate_fields_of_collection_in_sync_form(
                    ~collection,
                    ~fields,
                    ~output_type,
                    ~loc,
                  )
                }
              };
            switch (scheme) {
            | [x] => x |> value
            | _ => Exp.tuple(scheme |> List.map(value))
            };
          };

          let ok_case = {
            let pat = (entry: Scheme.entry) =>
              switch (entry) {
              | Field(field) => field |> ok_pat_for_sync_field(~loc)
              | Collection({collection, validator}) =>
                switch (validator) {
                | Ok(Some ())
                | Error () => [%pat?
                    (
                      [%p collection |> ok_pat_for_collection(~loc)],
                      [%p
                        collection |> ok_pat_for_fields_of_collection(~loc)
                      ],
                    )
                  ]
                | Ok(None) =>
                  collection |> ok_pat_for_fields_of_collection(~loc)
                }
              };
            Exp.case(
              switch (scheme) {
              | [x] => x |> pat
              | _ => Pat.tuple(scheme |> List.map(pat))
              },
              {
                let output =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> output_field_record_field(~loc)
                         | Collection({collection}) =>
                           collection |> output_collection_record_field(~loc)
                         }
                       ),
                    None,
                  );
                let fields_statuses =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> field_dirty_status_record_field(~loc)
                         | Collection({collection}) =>
                           collection
                           |> collection_statuses_record_field(~loc)
                         }
                       ),
                    None,
                  );

                switch (collections) {
                | [] =>
                  %expr
                  Valid({
                    output: [%e output],
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: (),
                  })
                | collections =>
                  %expr
                  Valid({
                    output: [%e output],
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: [%e
                      collections |> collections_statuses_record(~loc)
                    ],
                  })
                };
              },
            );
          };

          let error_case = {
            let entry = (entry: Scheme.entry) =>
              switch (entry) {
              | Field(field) =>
                switch (scheme) {
                | [_x] =>
                  field |> error_pat_for_sync_field_in_single_field_form(~loc)
                | _ =>
                  field |> error_pat_for_sync_field_in_multi_field_form(~loc)
                }
              | Collection({collection, validator}) =>
                switch (validator) {
                | Ok(Some ())
                | Error () => [%pat?
                    (
                      [%p collection |> result_pat_for_collection(~loc)],
                      [%p
                        collection |> error_pat_for_fields_of_collection(~loc)
                      ],
                    )
                  ]
                | Ok(None) =>
                  collection |> error_pat_for_fields_of_collection(~loc)
                }
              };
            Exp.case(
              switch (scheme) {
              | [x] => x |> entry
              | _ => Pat.tuple(scheme |> List.map(entry))
              },
              {
                let fields_statuses =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> field_dirty_status_record_field(~loc)
                         | Collection({collection}) =>
                           collection
                           |> collection_statuses_record_field(~loc)
                         }
                       ),
                    None,
                  );

                switch (collections) {
                | [] =>
                  %expr
                  Invalid({
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: (),
                  })
                | _ =>
                  %expr
                  Invalid({
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: [%e
                      collections |> collections_statuses_record(~loc)
                    ],
                  })
                };
              },
            );
          };

          Exp.match(match_values, [ok_case, error_case]);
        };
      }
    ];
  };
};

module Async = {
  type validating_entry = [
    | `AsyncField(Scheme.field)
    | `Collection(Collection.t)
  ];

  let ast = (~scheme: Scheme.t, ~loc) => {
    [%stri
      let validateForm =
          (
            input: input,
            ~validators: validators,
            ~fieldsStatuses: fieldsStatuses,
          )
          : Async.formValidationResult(
              output,
              fieldsStatuses,
              collectionsStatuses,
            ) => {
        %e
        {
          let collections = scheme |> Scheme.collections;

          let match_values = {
            let value = (entry: Scheme.entry) =>
              switch (entry) {
              | Field(
                  {
                    validator:
                      SyncValidator(
                        Ok(Required | Optional(Some(_))) | Error (),
                      ),
                  } as field,
                ) =>
                validate_field_with_sync_validator(~field, ~loc)
              | Field(
                  {validator: SyncValidator(Ok(Optional(None)))} as field,
                ) =>
                validate_field_without_validator(~field, ~loc)
              | Field({validator: AsyncValidator(_)} as field) =>
                validate_field_with_async_validator(~field, ~loc)
              | Collection({collection, fields, validator, output_type}) =>
                switch (validator) {
                | Ok(Some ())
                | Error () =>
                  %expr
                  (
                    [%e validate_whole_collection(~collection, ~loc)],
                    [%e
                      validate_fields_of_collection_in_async_form(
                        ~collection,
                        ~fields,
                        ~output_type,
                        ~loc,
                      )
                    ],
                  )
                | Ok(None) =>
                  validate_fields_of_collection_in_async_form(
                    ~collection,
                    ~fields,
                    ~output_type,
                    ~loc,
                  )
                }
              };
            switch (scheme) {
            | [x] => x |> value
            | _ => Exp.tuple(scheme |> List.map(value))
            };
          };

          let validating_case = {
            let entries_might_be_in_validating_state: list(validating_entry) =
              scheme
              |> List.fold_left(
                   (acc, entry: Scheme.entry) =>
                     switch (entry) {
                     | Field({validator: SyncValidator(_)}) => acc
                     | Field({validator: AsyncValidator(_)} as field) => [
                         `AsyncField(field),
                         ...acc,
                       ]
                     | Collection({collection}) => [
                         `Collection(collection),
                         ...acc,
                       ]
                     },
                   [],
                 );
            let make = (entry: validating_entry) =>
              Pat.tuple(
                switch (entry) {
                | `AsyncField(current_field) =>
                  scheme
                  |> List.map((entry: Scheme.entry) =>
                       switch (entry) {
                       | Field({validator: AsyncValidator(_)} as field)
                           when field.name == current_field.name =>
                         Pat.tuple([
                           Pat.alias(
                             Pat.variant("Validating", Some(Pat.any())),
                             field_result_var(~field=field.name) |> str(~loc),
                           ),
                           Pat.var(
                             field_result_visibility_var(~field=field.name)
                             |> str(~loc),
                           ),
                         ])
                       | Field(
                           {validator: SyncValidator(_) | AsyncValidator(_)} as field,
                         ) =>
                         field |> result_and_visibility_pat_for_field(~loc)
                       | Collection({collection, validator}) =>
                         switch (validator) {
                         | Ok(Some ())
                         | Error () => [%pat?
                             (
                               [%p
                                 collection |> result_pat_for_collection(~loc)
                               ],
                               [%p
                                 collection
                                 |> result_pat_for_fields_of_collection(~loc)
                               ],
                             )
                           ]
                         | Ok(None) =>
                           collection
                           |> result_pat_for_fields_of_collection(~loc)
                         }
                       }
                     )
                | `Collection(current_collection) =>
                  scheme
                  |> List.map((entry: Scheme.entry) =>
                       switch (entry) {
                       | Field(field) =>
                         field |> result_and_visibility_pat_for_field(~loc)
                       | Collection({collection, validator})
                           when collection.plural == current_collection.plural =>
                         switch (validator) {
                         | Ok(Some ())
                         | Error () => [%pat?
                             (
                               [%p
                                 collection |> result_pat_for_collection(~loc)
                               ],
                               [%p
                                 Pat.alias(
                                   Pat.variant(
                                     "ValidatingFieldsOfCollection",
                                     Some(Pat.any()),
                                   ),
                                   collection
                                   |> fields_of_collection_result_var
                                   |> str(~loc),
                                 )
                               ],
                             )
                           ]
                         | Ok(None) =>
                           Pat.alias(
                             Pat.variant(
                               "ValidatingFieldsOfCollection",
                               Some(Pat.any()),
                             ),
                             collection
                             |> fields_of_collection_result_var
                             |> str(~loc),
                           )
                         }

                       | Collection({collection, validator}) =>
                         switch (validator) {
                         | Ok(Some ())
                         | Error () => [%pat?
                             (
                               [%p
                                 collection |> result_pat_for_collection(~loc)
                               ],
                               [%p
                                 collection
                                 |> result_pat_for_fields_of_collection(~loc)
                               ],
                             )
                           ]
                         | Ok(None) =>
                           collection
                           |> result_pat_for_fields_of_collection(~loc)
                         }
                       }
                     )
                },
              );
            Exp.case(
              switch (entries_might_be_in_validating_state) {
              | [] =>
                failwith(
                  "No entries found that might be in validating state. Please, file an issue with your use-case.",
                )
              | [x] => x |> make
              | [x, ...rest] => P.or_(~pat=x |> make, ~make, rest)
              },
              {
                let fields_statuses =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field({validator: SyncValidator(_)} as field) =>
                           field |> field_dirty_status_record_field(~loc)
                         | Field({validator: AsyncValidator(_)} as field) =>
                           field
                           |> async_field_dirty_or_validating_status_record_field(
                                ~loc,
                              )
                         | Collection({collection}) =>
                           collection
                           |> collection_that_might_be_in_validating_state_status_record_field(
                                ~loc,
                              )
                         }
                       ),
                    None,
                  );
                switch (collections) {
                | [] =>
                  %expr
                  {
                    Validating({
                      fieldsStatuses: [%e fields_statuses],
                      collectionsStatuses: (),
                    });
                  }
                | collections =>
                  %expr
                  {
                    Validating({
                      fieldsStatuses: [%e fields_statuses],
                      collectionsStatuses: [%e
                        collections |> collections_statuses_record(~loc)
                      ],
                    });
                  }
                };
              },
            );
          };

          let ok_case =
            Exp.case(
              Pat.tuple(
                scheme
                |> List.map((entry: Scheme.entry) =>
                     switch (entry) {
                     | Field({validator: SyncValidator(_)} as field) =>
                       field |> ok_pat_for_sync_field(~loc)
                     | Field({validator: AsyncValidator(_)} as field) =>
                       field |> ok_pat_for_async_field(~loc)
                     | Collection({collection, validator}) =>
                       switch (validator) {
                       | Ok(Some ())
                       | Error () => [%pat?
                           (
                             [%p collection |> ok_pat_for_collection(~loc)],
                             [%p
                               collection
                               |> ok_pat_for_fields_of_async_collection(~loc)
                             ],
                           )
                         ]
                       | Ok(None) =>
                         collection
                         |> ok_pat_for_fields_of_async_collection(~loc)
                       }
                     }
                   ),
              ),
              {
                let output =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> output_field_record_field(~loc)
                         | Collection({collection}) =>
                           collection |> output_collection_record_field(~loc)
                         }
                       ),
                    None,
                  );

                let fields_statuses =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> field_dirty_status_record_field(~loc)
                         | Collection({collection}) =>
                           collection
                           |> collection_statuses_record_field(~loc)
                         }
                       ),
                    None,
                  );

                switch (collections) {
                | [] =>
                  %expr
                  Valid({
                    output: [%e output],
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: (),
                  })
                | collections =>
                  %expr
                  Valid({
                    output: [%e output],
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: [%e
                      collections |> collections_statuses_record(~loc)
                    ],
                  })
                };
              },
            );

          let error_case =
            Exp.case(
              Pat.tuple(
                scheme
                |> List.map((entry: Scheme.entry) =>
                     switch (entry) {
                     | Field({validator: SyncValidator(_)} as field) =>
                       field |> result_and_visibility_pat_for_field(~loc)
                     | Field({validator: AsyncValidator(_)} as field) =>
                       field
                       |> result_and_visibility_pat_for_async_field(~loc)
                     | Collection({collection, validator}) =>
                       switch (validator) {
                       | Ok(Some ())
                       | Error () => [%pat?
                           (
                             [%p
                               collection |> result_pat_for_collection(~loc)
                             ],
                             [%p
                               collection
                               |> fields_statuses_pat_for_async_collection(
                                    ~loc,
                                  )
                             ],
                           )
                         ]
                       | Ok(None) =>
                         collection
                         |> fields_statuses_pat_for_async_collection(~loc)
                       }
                     }
                   ),
              ),
              {
                let fields_statuses =
                  Exp.record(
                    scheme
                    |> List.map((entry: Scheme.entry) =>
                         switch (entry) {
                         | Field(field) =>
                           field |> field_dirty_status_record_field(~loc)
                         | Collection({collection}) =>
                           collection
                           |> collection_statuses_record_field(~loc)
                         }
                       ),
                    None,
                  );

                switch (collections) {
                | [] =>
                  %expr
                  Invalid({
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: (),
                  })
                | collections =>
                  %expr
                  Invalid({
                    fieldsStatuses: [%e fields_statuses],
                    collectionsStatuses: [%e
                      collections |> collections_statuses_record(~loc)
                    ],
                  })
                };
              },
            );

          Exp.match(match_values, [validating_case, ok_case, error_case]);
        };
      }
    ];
  };
};
