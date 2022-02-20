open Meta;
open Ast;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~loc) => {
  [%stri
    let initialCollectionsStatuses: collectionsStatuses =
      switch%e (scheme |> Scheme.collections) {
      | [] =>
        %expr
        ()
      | collections =>
        Exp.record(
          collections
          |> List.rev
          |> List.rev_map(({collection, validator}: Scheme.collection) =>
               (
                 Lident(collection.plural) |> lid(~loc),
                 switch (validator) {
                 | Ok(Some ())
                 | Error () =>
                   %expr
                   None
                 | Ok(None) =>
                   %expr
                   ()
                 },
               )
             ),
          None,
        )
      }
  ];
};
