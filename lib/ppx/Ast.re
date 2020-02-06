open Ppxlib;
open Ast_helper;

let lid = (~loc, x: Longident.t) => {txt: x, loc};
let str = (~loc, x: string) => {txt: x, loc};

let explicit_arity = (~loc) => {
  attr_name: "explicit_arity" |> str(~loc),
  attr_payload: PStr([]),
  attr_loc: Location.none,
};

module StructureItem = {
  let from_type_declaration =
      (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    Str.type_(~loc, rec_flag, [decl]);
};
