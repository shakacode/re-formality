open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let ast ~loc ~(metadata : unit option) (scheme : Scheme.t) =
  let collections = scheme |> Scheme.collections in
  collections
  |> List.fold_left
       (fun acc ({ collection; validator; fields } : Scheme.collection) ->
         let deps =
           fields
           |> List.fold_left
                (fun acc (field : Scheme.field) ->
                  acc |> List.rev_append (field.deps |> List.rev))
                []
         in
         let add_action_pat =
           Pat.construct
             ~attrs:[ explicit_arity ~loc ]
             (Lident (collection |> CollectionPrinter.add_action) |> lid ~loc)
             (Some (Pat.tuple [ Pat.var ("entry" |> str ~loc) ]))
         in
         let add_entry_to_input_exp =
           collection.plural
           |> E.update_field2
                ~in_:("state", "input")
                ~with_:
                  [%expr
                    Belt.Array.concat
                      [%e collection.plural |> E.field2 ~in_:("state", "input") ~loc]
                      [| entry |]]
                ~loc
         in
         let add_entry_to_fields_statuses_exp =
           collection.plural
           |> E.update_field2
                ~in_:("state", "fieldsStatuses")
                ~with_:
                  [%expr
                    Belt.Array.concat
                      [%e
                        collection.plural
                        |> E.field2 ~in_:("state", "fieldsStatuses") ~loc]
                      [| [%e
                           Exp.record
                             (fields
                              |> List.rev
                              |> List.rev_map (fun (field : Scheme.field) ->
                                Lident field.name |> lid ~loc, [%expr Pristine]))
                             None]
                      |]]
                ~loc
         in
         let remove_action_pat =
           Pat.construct
             ~attrs:[ explicit_arity ~loc ]
             (Lident (collection |> CollectionPrinter.remove_action) |> lid ~loc)
             (Some (Pat.tuple [ Pat.var ("index" |> str ~loc) ]))
         in
         let remove_entry_from_input_exp =
           collection.plural
           |> E.update_field2
                ~in_:("state", "input")
                ~with_:
                  [%expr
                    Belt.Array.keepWithIndex
                      [%e collection.plural |> E.field2 ~in_:("state", "input") ~loc]
                      (fun _ i -> i <> index)]
                ~loc
         in
         let remove_entry_from_fields_statuses_exp =
           collection.plural
           |> E.update_field2
                ~in_:("state", "fieldsStatuses")
                ~with_:
                  [%expr
                    Belt.Array.keepWithIndex
                      [%e
                        collection.plural
                        |> E.field2 ~in_:("state", "fieldsStatuses") ~loc]
                      (fun _ i -> i <> index)]
                ~loc
         in
         let update_collections_statuses =
           Exp.record
             [ ( Lident collection.plural |> lid ~loc
               , [%expr
                   Some
                     [%e
                       E.apply_field2
                         ~in_:("validators", collection.plural)
                         ~fn:"collection"
                         ~args:
                           (match metadata with
                            | None -> [ Nolabel, [%expr nextInput] ]
                            | Some () ->
                              [ Nolabel, [%expr nextInput]; Nolabel, [%expr metadata] ])
                         ~loc]] )
             ]
             (match collections with
              | [] -> None
              | _x :: [] -> None
              | _ -> Some [%expr state.collectionsStatuses])
         in
         Exp.case
           remove_action_pat
           (match deps with
            | [] ->
              [%expr
                let nextInput = [%e remove_entry_from_input_exp] in
                let nextFieldsStatuses = [%e remove_entry_from_fields_statuses_exp] in
                [%e
                  match validator with
                  | Ok (Some ()) | Error () ->
                    [%expr
                      Update
                        { state with
                          input = nextInput
                        ; fieldsStatuses = nextFieldsStatuses
                        ; collectionsStatuses = [%e update_collections_statuses]
                        }]
                  | Ok None ->
                    [%expr
                      Update
                        { state with
                          input = nextInput
                        ; fieldsStatuses = nextFieldsStatuses
                        }]]]
            | dep :: deps ->
              [%expr
                let nextInput = [%e remove_entry_from_input_exp] in
                let nextFieldsStatuses = ref [%e remove_entry_from_fields_statuses_exp] in
                [%e
                  scheme
                  |> Form_UseFormFn_DependentFields.ast
                       ~loc
                       ~dep
                       ~deps
                       ~trigger:(`Collection collection)
                       ~metadata];
                [%e
                  match validator with
                  | Ok (Some ()) | Error () ->
                    [%expr
                      Update
                        { state with
                          input = nextInput
                        ; fieldsStatuses = !nextFieldsStatuses
                        ; collectionsStatuses = [%e update_collections_statuses]
                        }]
                  | Ok None ->
                    [%expr
                      Update
                        { state with
                          input = nextInput
                        ; fieldsStatuses = !nextFieldsStatuses
                        }]]])
         :: Exp.case
              add_action_pat
              (match deps with
               | [] ->
                 [%expr
                   let nextInput = [%e add_entry_to_input_exp] in
                   let nextFieldsStatuses = [%e add_entry_to_fields_statuses_exp] in
                   [%e
                     match validator with
                     | Ok (Some ()) | Error () ->
                       [%expr
                         Update
                           { state with
                             input = nextInput
                           ; fieldsStatuses = nextFieldsStatuses
                           ; collectionsStatuses = [%e update_collections_statuses]
                           }]
                     | Ok None ->
                       [%expr
                         Update
                           { state with
                             input = nextInput
                           ; fieldsStatuses = nextFieldsStatuses
                           }]]]
               | dep :: deps ->
                 [%expr
                   let nextInput = [%e add_entry_to_input_exp] in
                   let nextFieldsStatuses = ref [%e add_entry_to_fields_statuses_exp] in
                   [%e
                     scheme
                     |> Form_UseFormFn_DependentFields.ast
                          ~loc
                          ~dep
                          ~deps
                          ~trigger:(`Collection collection)
                          ~metadata];
                   [%e
                     match validator with
                     | Ok (Some ()) | Error () ->
                       [%expr
                         Update
                           { state with
                             input = nextInput
                           ; fieldsStatuses = !nextFieldsStatuses
                           ; collectionsStatuses = [%e update_collections_statuses]
                           }]
                     | Ok None ->
                       [%expr
                         Update
                           { state with
                             input = nextInput
                           ; fieldsStatuses = !nextFieldsStatuses
                           }]]])
         :: acc)
       []
;;
