open Meta
open AstHelpers
open Ppxlib

let ast
  ~loc
  ~(dep : FieldDep.t)
  ~(deps : FieldDep.t list)
  ~(trigger :
     [ `Field of string
     | `Collection of Collection.t
     | `FieldOfCollection of Collection.t * string
     ])
  ~(metadata : unit option)
  (scheme : Scheme.t)
  =
  let validate_dep (dep : FieldDep.t) =
    match
      scheme
      |> List.fold_left
           (fun res (entry : Scheme.entry) ->
             match res, entry, dep with
             | Some _, _, _ -> res
             | None, Field field, DepField dep ->
               (match field.name = dep with
                | true -> Some (`DepField field)
                | false -> None)
             | ( None
               , Collection { collection; fields }
               , DepFieldOfCollection { collection = dep_collection; field = dep_field } )
               ->
               if collection.plural <> dep_collection.plural
               then None
               else
                 Some
                   (`DepFieldOfCollection
                     ( collection
                     , fields
                       |> List.find (fun (field : Scheme.field) -> field.name = dep_field)
                     ))
             | None, Collection _, DepField _ | None, Field _, DepFieldOfCollection _ ->
               res)
           None
    with
    | None ->
      failwith "Dep is not found in scheme. Please, file an issue with your use-case."
    | Some (`DepField field) ->
      let field_status_expr = field.name |> E.ref_field ~in_:"nextFieldsStatuses" ~loc in
      let validator_expr = field.name |> E.field ~in_:"validators" ~loc in
      let set_status_expr =
        field.name
        |> E.update_ref_field ~in_:"nextFieldsStatuses" ~with_:[%expr status] ~loc
      in
      (match field.validator with
       | SyncValidator (Ok (Required | Optional (Some _)) | Error ()) ->
         [%expr
           match
             [%e
               match metadata with
               | None ->
                 [%expr
                   validateDependentFieldOnChange
                     ~input:nextInput
                     ~fieldStatus:[%e field_status_expr]
                     ~validator:[%e validator_expr]
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
               | Some () ->
                 [%expr
                   validateDependentFieldOnChangeWithMetadata
                     ~input:nextInput
                     ~fieldStatus:[%e field_status_expr]
                     ~validator:[%e validator_expr]
                     ~metadata
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
           with
           | Some result -> nextFieldsStatuses := result
           | None -> ()]
       | SyncValidator (Ok (Optional None)) -> [%expr ()]
       | AsyncValidator { mode = OnChange | OnBlur } ->
         [%expr
           match
             [%e
               match metadata with
               | None ->
                 [%expr
                   Async.validateDependentFieldOnChange
                     ~input:nextInput
                     ~fieldStatus:[%e field_status_expr]
                     ~validator:[%e validator_expr]
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
               | Some () ->
                 [%expr
                   Async.validateDependentFieldOnChangeWithMetadata
                     ~input:nextInput
                     ~fieldStatus:[%e field_status_expr]
                     ~validator:[%e validator_expr]
                     ~metadata
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
           with
           | Some result -> nextFieldsStatuses := result
           | None -> ()])
    | Some (`DepFieldOfCollection (collection, field)) ->
      let collection_statuses_expr =
        collection.plural |> E.ref_field ~in_:"nextFieldsStatuses" ~loc
      in
      let field_status_expr = field.name |> E.field ~in_:"item" ~loc in
      let validator_expr =
        field.name
        |> E.field_of_collection_validator ~validators:"validators" ~collection ~loc
      in
      let set_status_expr =
        field.name
        |> E.update_ref_field_of_collection
             ~in_:"nextFieldsStatuses"
             ~collection
             ~with_:[%expr status]
             ~index_token:"index'"
             ~loc
      in
      (match trigger with
       | `FieldOfCollection (collection', field')
         when collection.plural = collection'.plural && field.name = field' ->
         (match field.validator with
          | SyncValidator (Ok (Required | Optional (Some _)) | Error ()) ->
            [%expr
              Belt.Array.forEachWithIndex
                [%e collection_statuses_expr]
                (fun index' item ->
                  if index <> index'
                  then (
                    match
                      [%e
                        match metadata with
                        | None ->
                          [%expr
                            validateDependentFieldOfCollectionOnChange
                              ~input:nextInput
                              ~index:index'
                              ~fieldStatus:[%e field_status_expr]
                              ~validator:[%e validator_expr]
                              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
                        | Some () ->
                          [%expr
                            validateDependentFieldOfCollectionOnChangeWithMetadata
                              ~input:nextInput
                              ~index:index'
                              ~fieldStatus:[%e field_status_expr]
                              ~validator:[%e validator_expr]
                              ~metadata
                              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
                    with
                    | Some result -> nextFieldsStatuses := result
                    | None -> ())
                  else ())]
          | SyncValidator (Ok (Optional None)) -> [%expr ()]
          | AsyncValidator { mode = OnChange | OnBlur } ->
            [%expr
              Belt.Array.forEachWithIndex
                [%e collection_statuses_expr]
                (fun index' item ->
                  if index <> index'
                  then (
                    match
                      [%e
                        match metadata with
                        | None ->
                          [%expr
                            Async.validateDependentFieldOfCollectionOnChange
                              ~input:nextInput
                              ~index:index'
                              ~fieldStatus:[%e field_status_expr]
                              ~validator:[%e validator_expr]
                              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
                        | Some () ->
                          [%expr
                            Async.validateDependentFieldOfCollectionOnChangeWithMetadata
                              ~input:nextInput
                              ~index:index'
                              ~fieldStatus:[%e field_status_expr]
                              ~validator:[%e validator_expr]
                              ~metadata
                              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
                    with
                    | Some result -> nextFieldsStatuses := result
                    | None -> ())
                  else ())])
       | `Field _ | `Collection _ | `FieldOfCollection (_, _) ->
         (match field.validator with
          | SyncValidator (Ok (Required | Optional (Some _)) | Error ()) ->
            [%expr
              Belt.Array.forEachWithIndex
                [%e collection_statuses_expr]
                (fun index' item ->
                  match
                    [%e
                      match metadata with
                      | None ->
                        [%expr
                          validateDependentFieldOfCollectionOnChange
                            ~input:nextInput
                            ~index:index'
                            ~fieldStatus:[%e field_status_expr]
                            ~validator:[%e validator_expr]
                            ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
                      | Some () ->
                        [%expr
                          validateDependentFieldOfCollectionOnChangeWithMetadata
                            ~input:nextInput
                            ~index:index'
                            ~fieldStatus:[%e field_status_expr]
                            ~validator:[%e validator_expr]
                            ~metadata
                            ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
                  with
                  | Some result -> nextFieldsStatuses := result
                  | None -> ())]
          | SyncValidator (Ok (Optional None)) -> [%expr ()]
          | AsyncValidator { mode = OnChange | OnBlur } ->
            [%expr
              Belt.Array.forEachWithIndex
                [%e collection_statuses_expr]
                (fun index' item ->
                  match
                    [%e
                      match metadata with
                      | None ->
                        [%expr
                          Async.validateDependentFieldOfCollectionOnChange
                            ~input:nextInput
                            ~index:index'
                            ~fieldStatus:[%e field_status_expr]
                            ~validator:[%e validator_expr]
                            ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
                      | Some () ->
                        [%expr
                          Async.validateDependentFieldOfCollectionOnChangeWithMetadata
                            ~input:nextInput
                            ~index:index'
                            ~fieldStatus:[%e field_status_expr]
                            ~validator:[%e validator_expr]
                            ~metadata
                            ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
                  with
                  | Some result -> nextFieldsStatuses := result
                  | None -> ())]))
  in
  deps |> E.seq ~exp:(dep |> validate_dep) ~make:validate_dep
;;
