open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~collections: list(Collection.t), ~loc) => {
  switch (collections) {
  | [] => [%stri type collectionsStatuses = unit]
  | _ =>
    Str.type_(
      ~loc,
      Recursive,
      [
        "collectionsStatuses"
        |> str(~loc)
        |> Type.mk(
             ~kind=
               Ptype_record(
                 collections
                 |> List.map((collection: Collection.t) =>
                      Type.field(
                        collection.plural |> str(~loc),
                        [%type: option(collectionStatus(message))],
                      )
                    ),
               ),
           ),
      ],
    )
  };
};
