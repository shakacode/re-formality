open Ppxlib
open Ast_helper

let fn ~loc ~arity fx =
  let attr =
    Attr.mk
      { txt = "res.arity"; loc }
      (PStr [ Str.eval (Exp.constant (Const.int arity)) ])
  in
  Exp.construct ~attrs:[ attr ] { txt = Lident "Function$"; loc } (Some fx)
;;

let ty ~loc ~arity t_arg =
  let t_arity =
    Typ.variant
      ~loc
      [ { prf_loc = loc
        ; prf_attributes = []
        ; prf_desc = Rtag ({ txt = "Has_arity" ^ string_of_int arity; loc }, true, [])
        }
      ]
      Closed
      None
  in
  Typ.constr ~loc { txt = Lident "function$"; loc } [ t_arg; t_arity ]
;;

let uapp : Parsetree.attribute =
  { attr_name = { txt = "res.uapp"; loc = Location.none }
  ; attr_payload = PStr []
  ; attr_loc = Location.none
  }
;;
