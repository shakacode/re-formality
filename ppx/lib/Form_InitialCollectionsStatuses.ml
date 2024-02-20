open Meta
open Ast
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~loc =
  [%stri
    let initialCollectionsStatuses =
      ([%e
         match scheme |> Scheme.collections with
         | [] -> [%expr ()]
         | collections ->
           Exp.record
             (collections
              |> List.rev
              |> List.rev_map (fun ({ collection; validator } : Scheme.collection) ->
                ( Lident collection.plural |> lid ~loc
                , match validator with
                  | Ok (Some ()) | Error () -> [%expr None]
                  | Ok None -> [%expr ()] )))
             None]
        : collectionsStatuses)
    ;;]
;;
