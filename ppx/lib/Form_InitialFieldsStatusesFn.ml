open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~loc =
  [%stri
    let initialFieldsStatuses
      ([%p
         match scheme |> Scheme.collections with
         | [] -> [%pat? _input]
         | _ -> [%pat? input]] :
        input)
      : fieldsStatuses
      =
      [%e
        Exp.record
          (scheme
           |> List.rev
           |> List.rev_map (fun (entry : Scheme.entry) ->
             match entry with
             | Field field -> Lident field.name |> lid ~loc, [%expr Pristine]
             | Collection { collection; fields } ->
               ( Lident collection.plural |> lid ~loc
               , [%expr
                   Belt.Array.make
                     (Belt.Array.length
                        [%e collection.plural |> E.field ~in_:"input" ~loc])
                     [%e
                       Exp.constraint_
                         (Exp.record
                            (fields
                             |> List.rev
                             |> List.rev_map (fun (field : Scheme.field) ->
                               Lident field.name |> lid ~loc, [%expr Pristine]))
                            None)
                         (Typ.constr
                            (Lident (collection |> CollectionPrinter.fields_statuses_type)
                             |> lid ~loc)
                            [])]] )))
          None]
    ;;]
;;
