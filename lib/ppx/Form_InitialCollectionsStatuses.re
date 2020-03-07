open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~loc) => {
  [%stri
    let initialCollectionsStatuses =
      switch%e (scheme |> Scheme.collections) {
      | [] =>
        %expr
        ()
      | collections =>
        Exp.record(
          collections
          |> List.map(({collection, validator}: Scheme.collection) =>
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
