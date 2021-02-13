open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let field_type = (~loc, ~metadata, field: Scheme.field) =>
  Type.field(
    field.name |> str(~loc),
    switch (field.validator) {
    | SyncValidator(Ok(Required))
    | SyncValidator(Ok(Optional(Some(_))))
    | SyncValidator(Error ()) =>
      switch (metadata) {
      | None => [%type:
          singleValueValidator(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
          )
        ]
      | Some () => [%type:
          singleValueValidatorWithMetadata(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            metadata,
          )
        ]
      }
    | SyncValidator(Ok(Optional(None))) => [%type: unit]
    | AsyncValidator(_) =>
      switch (metadata) {
      | None => [%type:
          Async.singleValueValidator(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            action,
          )
        ]
      | Some () => [%type:
          Async.singleValueValidatorWithMetadata(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            metadata,
            action,
          )
        ]
      }
    },
  );

let collection_type =
    (
      ~loc,
      ~validator: CollectionValidator.t,
      ~metadata: option(unit),
      collection: Collection.t,
    ) =>
  Type.field(
    collection.plural |> str(~loc),
    switch (validator) {
    | Ok(Some ())
    | Error () =>
      switch (metadata) {
      | None => [%type:
          collectionValidatorWithWholeCollectionValidator(
            input,
            message,
            [%t
              Typ.constr(
                Lident(collection |> CollectionPrinter.validator_type)
                |> lid(~loc),
                [],
              )
            ],
          )
        ]
      | Some () => [%type:
          collectionValidatorWithWholeCollectionValidatorAndMetadata(
            input,
            message,
            [%t
              Typ.constr(
                Lident(collection |> CollectionPrinter.validator_type)
                |> lid(~loc),
                [],
              )
            ],
            metadata,
          )
        ]
      }

    | Ok(None) => [%type:
        collectionValidatorWithoutWholeCollectionValidator(
          [%t
            Typ.constr(
              Lident(collection |> CollectionPrinter.validator_type)
              |> lid(~loc),
              [],
            )
          ],
        )
      ]
    },
  );

let field_of_collection_type =
    (~loc, ~metadata: option(unit), field: Scheme.field) =>
  Type.field(
    field.name |> str(~loc),
    switch (field.validator) {
    | SyncValidator(Ok(Required))
    | SyncValidator(Ok(Optional(Some(_))))
    | SyncValidator(Error ()) =>
      switch (metadata) {
      | None => [%type:
          valueOfCollectionValidator(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
          )
        ]
      | Some () => [%type:
          valueOfCollectionValidatorWithMetadata(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            metadata,
          )
        ]
      }

    | SyncValidator(Ok(Optional(None))) => [%type: unit]
    | AsyncValidator(_) =>
      switch (metadata) {
      | None => [%type:
          Async.valueOfCollectionValidator(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            action,
          )
        ]
      | Some () => [%type:
          Async.valueOfCollectionValidatorWithMetadata(
            input,
            [%t field.output_type |> ItemType.unpack],
            message,
            metadata,
            action,
          )
        ]
      }
    },
  );

let ast = (~scheme: Scheme.t, ~metadata: option(unit), ~loc) => {
  let main_decl =
    "validators"
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_record(
             scheme
             |> List.rev
             |> List.rev_map((entry: Scheme.entry) =>
                  switch (entry) {
                  | Field(field) => field |> field_type(~loc, ~metadata)
                  | Collection({collection, validator}) =>
                    collection |> collection_type(~validator, ~metadata, ~loc)
                  }
                ),
           ),
       );

  let collections_decls =
    scheme
    |> List.fold_left(
         (acc, entry: Scheme.entry) =>
           switch (entry) {
           | Field(_) => acc
           | Collection({collection, fields}) => [
               collection
               |> CollectionPrinter.validator_type
               |> str(~loc)
               |> Type.mk(
                    ~kind=
                      Ptype_record(
                        fields
                        |> List.rev
                        |> List.rev_map(
                             field_of_collection_type(~loc, ~metadata),
                           ),
                      ),
                  ),
               ...acc,
             ]
           },
         [],
       );

  Str.type_(~loc, Recursive, [main_decl, ...collections_decls]);
};
