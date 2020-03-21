open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~loc) => {
  switch (scheme |> Scheme.collections) {
  | [] => [%stri type collectionsStatuses = unit]
  | collections =>
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
                 |> List.map(({collection, validator}: Scheme.collection) =>
                      Type.field(
                        collection.plural |> str(~loc),
                        switch (validator) {
                        | Ok(Some(_))
                        | Error () => [%type:
                            option(collectionStatus(message))
                          ]
                        | Ok(None) => [%type: unit]
                        },
                      )
                    ),
               ),
           ),
      ],
    )
  };
};
