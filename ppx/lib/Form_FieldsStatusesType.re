open Meta;
open Ast;
open Printer;

open Ppxlib;
open Ast_helper;

let field_type = (~loc, field: Scheme.field) =>
  Type.field(
    field.name |> str(~loc),
    switch (field.validator) {
    | SyncValidator(_) => [%type:
        fieldStatus([%t field.output_type |> ItemType.unpack], message)
      ]
    | AsyncValidator(_) => [%type:
        Async.fieldStatus([%t field.output_type |> ItemType.unpack], message)
      ]
    },
  );

let collection_type = (~loc, collection: Collection.t) =>
  Type.field(
    collection.plural |> str(~loc),
    [%type:
      array(
        [%t
          Typ.constr(
            Lident(collection |> CollectionPrinter.fields_statuses_type)
            |> lid(~loc),
            [],
          )
        ],
      )
    ],
  );

let ast = (~scheme: Scheme.t, ~loc) => {
  let main_decl =
    "fieldsStatuses"
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_record(
             scheme
             |> List.rev
             |> List.rev_map((entry: Scheme.entry) =>
                  switch (entry) {
                  | Field(field) => field |> field_type(~loc)
                  | Collection({collection}) =>
                    collection |> collection_type(~loc)
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
               |> CollectionPrinter.fields_statuses_type
               |> str(~loc)
               |> Type.mk(
                    ~kind=
                      Ptype_record(
                        fields |> List.rev |> List.rev_map(field_type(~loc)),
                      ),
                  ),
               ...acc,
             ]
           },
         [],
       );

  Str.type_(~loc, Recursive, [main_decl, ...collections_decls]);
};
