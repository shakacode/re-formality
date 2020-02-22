open Meta;
open Ast;
open AstHelpers;
open Printer;

open Ppxlib;
open Ast_helper;

let ast = (~collections: list(Collection.t), ~loc) => {
  [%stri
    let initialCollectionsStatuses =
      switch%e (collections) {
      | [] =>
        %expr
        ()
      | _ =>
        Exp.record(
          collections
          |> List.map((collection: Collection.t) =>
               (Lident(collection.plural) |> lid(~loc), [%expr None])
             ),
          None,
        )
      }
  ];
};
