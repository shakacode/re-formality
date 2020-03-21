open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let field_type = (~loc, field: Scheme.field) =>
  Type.field(
    field.name |> str(~loc),
    switch (field.validator) {
    | SyncValidator(Ok(Required))
    | SyncValidator(Ok(Optional(Some(_))))
    | SyncValidator(Error ()) => [%type:
        singleValueValidator(
          input,
          [%t field.output_type |> ItemType.unpack],
          message,
        )
      ]
    | SyncValidator(Ok(Optional(None))) => [%type: unit]
    | AsyncValidator(_) => [%type:
        Async.singleValueValidator(
          input,
          [%t field.output_type |> ItemType.unpack],
          message,
          action,
        )
      ]
    },
  );

let collection_type =
    (~loc, ~validator: CollectionValidator.t, collection: Collection.t) =>
  Type.field(
    collection.plural |> str(~loc),
    switch (validator) {
    | Ok(Some ())
    | Error () => [%type:
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

let field_of_collection_type = (~loc, field: Scheme.field) =>
  Type.field(
    field.name |> str(~loc),
    switch (field.validator) {
    | SyncValidator(Ok(Required))
    | SyncValidator(Ok(Optional(Some(_))))
    | SyncValidator(Error ()) => [%type:
        valueOfCollectionValidator(
          input,
          [%t field.output_type |> ItemType.unpack],
          message,
        )
      ]
    | SyncValidator(Ok(Optional(None))) => [%type: unit]
    | AsyncValidator(_) => [%type:
        Async.valueOfCollectionValidator(
          input,
          [%t field.output_type |> ItemType.unpack],
          message,
          action,
        )
      ]
    },
  );

let ast = (~scheme: Scheme.t, ~loc) => {
  let main_decl =
    "validators"
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_record(
             scheme
             |> List.map((entry: Scheme.entry) =>
                  switch (entry) {
                  | Field(field) => field |> field_type(~loc)
                  | Collection({collection, validator}) =>
                    collection |> collection_type(~validator, ~loc)
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
                        fields |> List.map(field_of_collection_type(~loc)),
                      ),
                  ),
               ...acc,
             ]
           },
         [],
       );

  Str.type_(~loc, Recursive, [main_decl, ...collections_decls]);
};
