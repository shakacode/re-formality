open Ppxlib
open Ast_helper

let warnings = [ "-23"; "-40"; "-42" ] |> List.fold_left (fun acc x -> acc ^ x) ""
let ast ~loc = [%stri [@@@ocaml.warning [%e warnings |> Const.string |> Exp.constant]]]
