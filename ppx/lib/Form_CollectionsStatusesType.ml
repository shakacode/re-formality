open Meta
open Ast
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~loc =
  match scheme |> Scheme.collections with
  | [] -> [%stri type collectionsStatuses = unit]
  | collections ->
    Str.type_
      ~loc
      Recursive
      [ "collectionsStatuses"
        |> str ~loc
        |> Type.mk
             ~kind:
               (Ptype_record
                  (collections
                   |> List.rev
                   |> List.rev_map (fun ({ collection; validator } : Scheme.collection) ->
                     Type.field
                       (collection.plural |> str ~loc)
                       (match validator with
                        | Ok (Some _) | Error () ->
                          [%type: message collectionStatus option]
                        | Ok None -> [%type: unit]))))
      ]
;;
