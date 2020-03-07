open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let warnings =
  [
    // Warns on spread in single field record:
    //   type t = {a: int};
    //   let inc = x => {...x, a: x.a + 1};
    // Handling this introduces more complexity for no profit
    // since it's purely stylistic thing and doesn't affect JS output
    "-23",
    // Legacy
    "-42",
  ]
  |> List.fold_left((acc, x) => acc ++ x, "");

let ast = (~loc) => [%stri
  [@ocaml.warning [%e warnings |> Const.string |> Exp.constant]]
];
