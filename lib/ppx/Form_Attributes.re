open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let warnings =
  [
    // What:
    //   Warns on spread in single field record:
    //     type t = {a: int};
    //     let inc = x => {...x, a: x.a + 1};
    // Why disabled:
    //   Handling this introduces more complexity for no profit
    //   since it's purely stylistic thing and doesn't affect JS output.
    "-23",
    // What:
    //   Warns if some type is not in scope and if type annotation
    //   would be removed, meaning of code might change.
    // Why disabled:
    //   We already have all type annotations in place
    //   so opening `Async` module everywhere where it's required
    //   is more work to do for little benefit. Disabling for now.
    "-40",
    // Legacy
    "-42",
  ]
  |> List.fold_left((acc, x) => acc ++ x, "");

let ast = (~loc) => [%stri
  [@ocaml.warning [%e warnings |> Const.string |> Exp.constant]]
];
