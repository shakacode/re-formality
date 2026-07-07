open Ppxlib

let str ~loc (x : string) = { txt = x; loc }
let lid ~loc (x : Longident.t) = { txt = x; loc }

let explicit_arity ~loc =
  { attr_name = "explicit_arity" |> str ~loc
  ; attr_payload = PStr []
  ; attr_loc = Location.none
  }
;;
