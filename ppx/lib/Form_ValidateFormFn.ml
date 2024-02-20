open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let field_result_var ~(field : string) = field ^ "Result"
let field_result_visibility_var ~(field : string) = field ^ "ResultVisibility"

let fields_of_collection_result_var (collection : Collection.t) =
  collection.plural ^ "CollectionFieldsResult"
;;

let whole_collection_result_var (collection : Collection.t) =
  collection.plural ^ "CollectionResult"
;;

let collection_fields_statuses_var (collection : Collection.t) =
  collection.plural ^ "CollectionFieldsStatuses"
;;

let validate_field_without_validator ~(field : Scheme.field) ~loc =
  [%expr Ok [%e field.name |> E.field ~in_:"input" ~loc], Hidden]
;;

let validate_field_of_collection_without_validator
  ~(collection : Collection.t)
  ~(field : Scheme.field)
  ~loc
  =
  [%expr
    Ok
      [%e
        Exp.field
          [%expr
            Belt.Array.getUnsafe [%e collection.plural |> E.field ~in_:"input" ~loc] index]
          (Lident field.name |> lid ~loc)]
    , Hidden]
;;

let validate_field_with_sync_validator
  ~(field : Scheme.field)
  ~(metadata : unit option)
  ~loc
  =
  [%expr
    (match [%e field.name |> E.field ~in_:"fieldsStatuses" ~loc] with
     | Pristine ->
       [%e
         E.apply_field2
           ~in_:("validators", field.name)
           ~fn:"validate"
           ~args:
             (match metadata with
              | None -> [ Nolabel, [%expr input] ]
              | Some () -> [ Nolabel, [%expr input]; Nolabel, [%expr metadata] ])
           ~loc]
     | Dirty (result, _) -> result)
    , Shown]
;;

let validate_field_of_collection_with_sync_validator
  ~(field : Scheme.field)
  ~(collection : Collection.t)
  ~(metadata : unit option)
  ~loc
  =
  [%expr
    (match [%e field.name |> E.field ~in_:"fieldStatus" ~loc] with
     | Pristine ->
       [%e
         E.apply_field4
           ~in_:("validators", collection.plural, "fields", field.name)
           ~fn:"validate"
           ~args:
             (match metadata with
              | None -> [ Nolabel, [%expr input]; Labelled "at", [%expr index] ]
              | Some () ->
                [ Nolabel, [%expr input]
                ; Labelled "at", [%expr index]
                ; Labelled "metadata", [%expr metadata]
                ])
           ~loc]
     | Dirty (result, _) -> result)
    , Shown]
;;

let validate_field_with_async_validator
  ~(field : Scheme.field)
  ~(metadata : unit option)
  ~loc
  =
  [%expr
    (match [%e field.name |> E.field ~in_:"fieldsStatuses" ~loc] with
     | Validating value -> `Validating value
     | Pristine ->
       `Result
         [%e
           E.apply_field2
             ~in_:("validators", field.name)
             ~fn:"validate"
             ~args:
               (match metadata with
                | None -> [ Nolabel, [%expr input] ]
                | Some () -> [ Nolabel, [%expr input]; Nolabel, [%expr metadata] ])
             ~loc]
     | Dirty (result, _) -> `Result result)
    , Shown]
;;

let validate_field_of_collection_with_async_validator
  ~(field : Scheme.field)
  ~(collection : Collection.t)
  ~(metadata : unit option)
  ~loc
  =
  [%expr
    (match [%e field.name |> E.field ~in_:"fieldStatus" ~loc] with
     | Validating value -> `Validating value
     | Dirty (result, _) -> `Result result
     | Pristine ->
       `Result
         [%e
           E.apply_field4
             ~in_:("validators", collection.plural, "fields", field.name)
             ~fn:"validate"
             ~args:
               (match metadata with
                | None -> [ Nolabel, [%expr input]; Labelled "at", [%expr index] ]
                | Some () ->
                  [ Nolabel, [%expr input]
                  ; Labelled "at", [%expr index]
                  ; Labelled "metadata", [%expr metadata]
                  ])
             ~loc])
    , Shown]
;;

let validate_whole_collection ~(collection : Collection.t) ~(metadata : unit option) ~loc =
  E.apply_field2
    ~in_:("validators", collection.plural)
    ~fn:"collection"
    ~args:
      (match metadata with
       | None -> [ Nolabel, [%expr input] ]
       | Some () -> [ Nolabel, [%expr input]; Nolabel, [%expr metadata] ])
    ~loc
;;

let ok_pat_for_sync_field ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.alias
        (Pat.construct
           ~attrs:[ explicit_arity ~loc ]
           (Lident "Ok" |> lid ~loc)
           (Some (Pat.tuple [ Pat.var (field.name |> str ~loc) ])))
        (field_result_var ~field:field.name |> str ~loc)
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let ok_pat_for_async_field ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.variant
        "Result"
        (Some
           (Pat.alias
              (Pat.construct
                 ~attrs:[ explicit_arity ~loc ]
                 (Lident "Ok" |> lid ~loc)
                 (Some (Pat.tuple [ Pat.var (field.name |> str ~loc) ])))
              (field_result_var ~field:field.name |> str ~loc)))
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let ok_pat_for_fields_of_async_collection ~loc (collection : Collection.t) =
  Pat.variant
    "FieldsOfCollectionResult"
    (Some
       (Pat.tuple
          [ Pat.construct
              ~attrs:[ explicit_arity ~loc ]
              (Lident "Ok" |> lid ~loc)
              (Some (Pat.tuple [ Pat.var (collection.plural |> str ~loc) ]))
          ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
          ]))
;;

let ok_pat_for_collection ~loc (collection : Collection.t) =
  Pat.alias
    (Pat.construct
       ~attrs:[ explicit_arity ~loc ]
       (Lident "Ok" |> lid ~loc)
       (Some [%pat? ()]))
    (collection |> whole_collection_result_var |> str ~loc)
;;

let ok_pat_for_fields_of_collection ~loc (collection : Collection.t) =
  Pat.tuple
    [ Pat.construct
        ~attrs:[ explicit_arity ~loc ]
        (Lident "Ok" |> lid ~loc)
        (Some (Pat.tuple [ Pat.var (collection.plural |> str ~loc) ]))
    ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
    ]
;;

let result_pat_for_collection ~loc (collection : Collection.t) =
  Pat.var (collection |> whole_collection_result_var |> str ~loc)
;;

let error_pat_for_sync_field_in_single_field_form ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.alias
        (Pat.construct
           ~attrs:[ explicit_arity ~loc ]
           (Lident "Error" |> lid ~loc)
           (Some [%pat? _]))
        (field_result_var ~field:field.name |> str ~loc)
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let error_pat_for_async_field_in_single_field_form ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.variant
        "Result"
        (Some
           (Pat.alias
              (Pat.construct
                 ~attrs:[ explicit_arity ~loc ]
                 (Lident "Error" |> lid ~loc)
                 (Some [%pat? _]))
              (field_result_var ~field:field.name |> str ~loc)))
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let error_pat_for_sync_field_in_multi_field_form ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.or_
        (Pat.alias
           (Pat.construct
              ~attrs:[ explicit_arity ~loc ]
              (Lident "Ok" |> lid ~loc)
              (Some [%pat? _]))
           (field_result_var ~field:field.name |> str ~loc))
        (Pat.alias
           (Pat.construct
              ~attrs:[ explicit_arity ~loc ]
              (Lident "Error" |> lid ~loc)
              (Some [%pat? _]))
           (field_result_var ~field:field.name |> str ~loc))
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let error_pat_for_async_field_in_multi_field_form ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.variant
        "Result"
        (Some
           (Pat.or_
              (Pat.alias
                 (Pat.construct
                    ~attrs:[ explicit_arity ~loc ]
                    (Lident "Ok" |> lid ~loc)
                    (Some [%pat? _]))
                 (field_result_var ~field:field.name |> str ~loc))
              (Pat.alias
                 (Pat.construct
                    ~attrs:[ explicit_arity ~loc ]
                    (Lident "Error" |> lid ~loc)
                    (Some [%pat? _]))
                 (field_result_var ~field:field.name |> str ~loc))))
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let error_pat_for_fields_of_collection_in_single_field_form_without_collection_validator
  ~loc
  (collection : Collection.t)
  =
  Pat.tuple
    [ [%pat? Error _]
    ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
    ]
;;

let error_pat_for_fields_of_collection_in_multi_field_form_or_single_field_form_with_collection_validator
  ~loc
  (collection : Collection.t)
  =
  Pat.tuple
    [ [%pat? Ok _ | Error _]
    ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
    ]
;;

let error_pat_for_fields_of_collection_in_single_field_async_form_without_collection_validator
  ~loc
  (collection : Collection.t)
  =
  Pat.variant
    "FieldsOfCollectionResult"
    (Some
       (Pat.tuple
          [ [%pat? Error _]
          ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
          ]))
;;

let error_pat_for_fields_statuses_of_async_collection ~loc (collection : Collection.t) =
  Pat.variant
    "FieldsOfCollectionResult"
    (Some
       (Pat.tuple
          [ [%pat? Ok _ | Error _]
          ; Pat.var (collection |> collection_fields_statuses_var |> str ~loc)
          ]))
;;

let result_and_visibility_pat_for_field ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.var (field_result_var ~field:field.name |> str ~loc)
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let result_and_visibility_pat_for_async_field ~loc (field : Scheme.field) =
  Pat.tuple
    [ Pat.variant
        "Result"
        (Some (Pat.var (field_result_var ~field:field.name |> str ~loc)))
    ; Pat.var (field_result_visibility_var ~field:field.name |> str ~loc)
    ]
;;

let result_pat_for_fields_of_collection ~loc (collection : Collection.t) =
  Pat.var (collection |> fields_of_collection_result_var |> str ~loc)
;;

let output_field_record_field ~loc (field : Scheme.field) =
  Lident field.name |> lid ~loc, Exp.ident (Lident field.name |> lid ~loc)
;;

let output_collection_record_field ~loc (collection : Collection.t) =
  Lident collection.plural |> lid ~loc, Exp.ident (Lident collection.plural |> lid ~loc)
;;

let field_dirty_status_record_field ~loc (field : Scheme.field) =
  ( Lident field.name |> lid ~loc
  , [%expr
      Dirty
        ( [%e Exp.ident (Lident (field_result_var ~field:field.name) |> lid ~loc)]
        , [%e
            Exp.ident (Lident (field_result_visibility_var ~field:field.name) |> lid ~loc)]
        )] )
;;

let async_field_dirty_or_validating_status_record_field ~loc (field : Scheme.field) =
  ( Lident field.name |> lid ~loc
  , [%expr
      match [%e Exp.ident (Lident (field_result_var ~field:field.name) |> lid ~loc)] with
      | `Validating value -> Validating value
      | `Result result ->
        Dirty
          ( result
          , [%e
              Exp.ident
                (Lident (field_result_visibility_var ~field:field.name) |> lid ~loc)] )] )
;;

let collection_that_might_be_in_validating_state_status_record_field
  ~loc
  (collection : Collection.t)
  =
  ( Lident collection.plural |> lid ~loc
  , [%expr
      match
        [%e
          Exp.ident (Lident (collection |> fields_of_collection_result_var) |> lid ~loc)]
      with
      | `ValidatingFieldsOfCollection statuses -> statuses
      | `FieldsOfCollectionResult (_, statuses) -> statuses] )
;;

let collection_statuses_record_field ~loc (collection : Collection.t) =
  ( Lident collection.plural |> lid ~loc
  , Exp.ident (Lident (collection |> collection_fields_statuses_var) |> lid ~loc) )
;;

let collections_statuses_record ~loc (collections : Scheme.collection list) =
  Exp.record
    (collections
     |> List.rev
     |> List.rev_map (fun ({ collection; validator } : Scheme.collection) ->
       ( Lident collection.plural |> lid ~loc
       , match validator with
         | Ok (Some ()) | Error () ->
           [%expr
             Some
               [%e
                 Exp.ident (Lident (collection |> whole_collection_result_var) |> lid ~loc)]]
         | Ok None -> [%expr ()] )))
    None
;;

let validate_fields_of_collection_in_sync_form
  ~(collection : Collection.t)
  ~(fields : Scheme.field list)
  ~(output_type : ItemType.t)
  ~(metadata : unit option)
  ~(loc : Location.t)
  =
  let match_values =
    Exp.tuple
      ([%expr output]
       :: (fields
           |> List.rev
           |> List.rev_map (fun (field : Scheme.field) ->
             match field.validator with
             | SyncValidator (Ok (Required | Optional (Some _)) | Error ()) ->
               validate_field_of_collection_with_sync_validator
                 ~collection
                 ~field
                 ~metadata
                 ~loc
             | SyncValidator (Ok (Optional None)) ->
               validate_field_of_collection_without_validator ~collection ~field ~loc
             | AsyncValidator _ ->
               failwith
                 "Form that supposed to be without async validators has one. Please, \
                  file an issue with yoour use-case.")))
  in
  let ok_case =
    Exp.case
      (Pat.tuple
         (Pat.construct
            ~attrs:[ explicit_arity ~loc ]
            (Lident "Ok" |> lid ~loc)
            (Some (Pat.tuple [ Pat.var ("output" |> str ~loc) ]))
          :: (fields |> List.rev |> List.rev_map (ok_pat_for_sync_field ~loc))))
      [%expr
        ignore
          (Js.Array2.push
             output
             [%e
               Exp.record
                 (fields |> List.rev |> List.rev_map (output_field_record_field ~loc))
                 None]);
        ignore
          (Js.Array2.push
             statuses
             [%e
               Exp.record
                 (fields
                  |> List.rev
                  |> List.rev_map (field_dirty_status_record_field ~loc))
                 None]);
        Ok output, statuses]
  in
  let error_case =
    Exp.case
      (Pat.tuple
         ([%pat? Ok _ | Error _]
          :: (fields
              |> List.rev
              |> List.rev_map (result_and_visibility_pat_for_field ~loc))))
      [%expr
        ignore
          (Js.Array2.push
             statuses
             [%e
               Exp.record
                 (fields
                  |> List.rev
                  |> List.rev_map (field_dirty_status_record_field ~loc))
                 None]);
        Error (), statuses]
  in
  [%expr
    Belt.Array.reduceWithIndex
      [%e collection.plural |> E.field ~in_:"fieldsStatuses" ~loc]
      (Ok [||], [||])
      (fun ( (output : ([%t output_type |> ItemType.unpack] array, unit) result)
           , (statuses :
               [%t
                 Typ.constr
                   (Lident (collection |> CollectionPrinter.fields_statuses_type)
                    |> lid ~loc)
                   []]
               array) )
           fieldStatus
           index ->
        [%e
          Exp.match_
            ~attrs:[ warning_4_disable ~loc ]
            match_values
            [ ok_case; error_case ]])]
;;

let validate_fields_of_collection_in_async_form
  ~(collection : Collection.t)
  ~(fields : Scheme.field list)
  ~(output_type : ItemType.t)
  ~(metadata : unit option)
  ~(loc : Location.t)
  =
  let fields_statuses_type =
    Typ.constr
      (Lident (collection |> CollectionPrinter.fields_statuses_type) |> lid ~loc)
      []
  in
  let match_values =
    Exp.tuple
      ([%expr result]
       :: (fields
           |> List.rev
           |> List.rev_map (fun (field : Scheme.field) ->
             match field.validator with
             | SyncValidator (Ok (Required | Optional (Some _)) | Error ()) ->
               validate_field_of_collection_with_sync_validator
                 ~collection
                 ~field
                 ~metadata
                 ~loc
             | SyncValidator (Ok (Optional None)) ->
               validate_field_of_collection_without_validator ~collection ~field ~loc
             | AsyncValidator _ ->
               validate_field_of_collection_with_async_validator
                 ~collection
                 ~field
                 ~metadata
                 ~loc)))
  in
  let validating_case =
    Exp.case
      (P.or_
         ~pat:
           (Pat.tuple
              ([%pat? `ValidatingFieldsOfCollection statuses]
               :: (fields
                   |> List.rev
                   |> List.rev_map (result_and_visibility_pat_for_field ~loc))))
         ~make:(fun (field : Scheme.field) ->
           Pat.tuple
             ([%pat? `FieldsOfCollectionResult ((Ok _ | Error _), statuses)]
              :: (fields
                  |> List.rev
                  |> List.rev_map (fun (field' : Scheme.field) ->
                    if field'.name = field.name
                    then
                      Pat.tuple
                        [ Pat.alias
                            (Pat.variant "Validating" (Some (Pat.any ())))
                            (field_result_var ~field:field.name |> str ~loc)
                        ; Pat.var
                            (field_result_visibility_var ~field:field.name |> str ~loc)
                        ]
                    else field' |> result_and_visibility_pat_for_field ~loc))))
         (fields
          |> List.filter (fun (field : Scheme.field) ->
            match field.validator with
            | SyncValidator _ -> false
            | AsyncValidator _ -> true)))
      [%expr
        ignore
          (Js.Array2.push
             statuses
             [%e
               Exp.record
                 (fields
                  |> List.rev
                  |> List.rev_map (fun (field : Scheme.field) ->
                    match field.validator with
                    | SyncValidator _ -> field |> field_dirty_status_record_field ~loc
                    | AsyncValidator _ ->
                      field |> async_field_dirty_or_validating_status_record_field ~loc))
                 None]);
        `ValidatingFieldsOfCollection statuses]
  in
  let ok_case =
    Exp.case
      (Pat.tuple
         ([%pat? `FieldsOfCollectionResult (Ok output, statuses)]
          :: (fields
              |> List.rev
              |> List.rev_map (fun (field : Scheme.field) ->
                match field.validator with
                | SyncValidator _ -> field |> ok_pat_for_sync_field ~loc
                | AsyncValidator _ -> field |> ok_pat_for_async_field ~loc))))
      [%expr
        ignore
          (Js.Array2.push
             output
             [%e
               Exp.record
                 (fields |> List.rev |> List.rev_map (output_field_record_field ~loc))
                 None]);
        ignore
          (Js.Array2.push
             statuses
             [%e
               Exp.record
                 (fields
                  |> List.rev
                  |> List.rev_map (field_dirty_status_record_field ~loc))
                 None]);
        `FieldsOfCollectionResult (Ok output, statuses)]
  in
  let error_case =
    Exp.case
      (Pat.tuple
         ([%pat? `FieldsOfCollectionResult ((Ok _ | Error _), statuses)]
          :: (fields
              |> List.rev
              |> List.rev_map (result_and_visibility_pat_for_field ~loc))))
      [%expr
        ignore
          (Js.Array2.push
             statuses
             [%e
               Exp.record
                 (fields
                  |> List.rev
                  |> List.rev_map (fun (field : Scheme.field) ->
                    match field.validator with
                    | SyncValidator _ -> field |> field_dirty_status_record_field ~loc
                    | AsyncValidator _ ->
                      field |> async_field_dirty_or_validating_status_record_field ~loc))
                 None]);
        `FieldsOfCollectionResult (Error (), statuses)]
  in
  [%expr
    Belt.Array.reduceWithIndex
      [%e collection.plural |> E.field ~in_:"fieldsStatuses" ~loc]
      (`FieldsOfCollectionResult (Ok [||], [||]))
      (fun (result :
             [ `ValidatingFieldsOfCollection of [%t fields_statuses_type] array
             | `FieldsOfCollectionResult of
               ([%t output_type |> ItemType.unpack] array, unit) result
               * [%t fields_statuses_type] array
             ])
           fieldStatus
           index ->
        [%e
          Exp.match_
            ~attrs:[ warning_4_disable ~loc ]
            match_values
            [ validating_case; ok_case; error_case ]])]
;;

module Sync = struct
  let ast ~(scheme : Scheme.t) ~(metadata : unit option) ~loc =
    let anything_validatable =
      scheme
      |> List.exists (fun (entry : Scheme.entry) ->
        match entry with
        | Field
            { validator = SyncValidator (Ok (Required | Optional (Some ())) | Error ()) }
          -> true
        | Field { validator = SyncValidator (Ok (Optional None)) } -> false
        | Field { validator = AsyncValidator _ } -> true
        | Collection { validator = Ok (Some ()) | Error () } -> true
        | Collection { validator = Ok None; fields } ->
          fields
          |> List.exists (fun (field : Scheme.field) ->
            match field.validator with
            | SyncValidator (Ok (Required | Optional (Some ())) | Error ()) -> true
            | SyncValidator (Ok (Optional None)) -> false
            | AsyncValidator _ -> true))
    in
    let body =
      [%expr
        [%e
          let collections = scheme |> Scheme.collections in
          let match_values =
            let value (entry : Scheme.entry) =
              match entry with
              | Field
                  ({ validator =
                       SyncValidator (Ok (Required | Optional (Some _)) | Error ())
                   } as field) -> validate_field_with_sync_validator ~field ~metadata ~loc
              | Field ({ validator = SyncValidator (Ok (Optional None)) } as field) ->
                validate_field_without_validator ~field ~loc
              | Field { name = _; validator = AsyncValidator _ } ->
                failwith
                  "Form that supposed to be without async validators has one. Please, \
                   file an issue with yoour use-case."
              | Collection { collection; fields; validator; output_type } ->
                (match validator with
                 | Ok (Some ()) | Error () ->
                   [%expr
                     [%e validate_whole_collection ~collection ~metadata ~loc]
                     , [%e
                         validate_fields_of_collection_in_sync_form
                           ~collection
                           ~fields
                           ~output_type
                           ~metadata
                           ~loc]]
                 | Ok None ->
                   validate_fields_of_collection_in_sync_form
                     ~collection
                     ~fields
                     ~output_type
                     ~metadata
                     ~loc)
            in
            match scheme with
            | x :: [] -> x |> value
            | _ -> scheme |> List.rev |> List.rev_map value |> Exp.tuple
          in
          let ok_case =
            let pat =
              let entry (entry : Scheme.entry) =
                match entry with
                | Field field -> field |> ok_pat_for_sync_field ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> ok_pat_for_collection ~loc]
                       , [%p collection |> ok_pat_for_fields_of_collection ~loc] )]
                   | Ok None -> collection |> ok_pat_for_fields_of_collection ~loc)
              in
              match scheme with
              | x :: [] -> x |> entry
              | _ -> scheme |> List.rev |> List.rev_map entry |> Pat.tuple
            in
            let expr =
              let output =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> output_field_record_field ~loc
                     | Collection { collection } ->
                       collection |> output_collection_record_field ~loc))
                  None
              in
              let fields_statuses =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> field_dirty_status_record_field ~loc
                     | Collection { collection } ->
                       collection |> collection_statuses_record_field ~loc))
                  None
              in
              match collections with
              | [] ->
                [%expr
                  Valid
                    { output = [%e output]
                    ; fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses = ()
                    }]
              | collections ->
                [%expr
                  Valid
                    { output = [%e output]
                    ; fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses =
                        [%e collections |> collections_statuses_record ~loc]
                    }]
            in
            Exp.case pat expr
          in
          let error_case =
            let pat =
              let entry_of_one (entry : Scheme.entry) =
                match entry with
                | Field field ->
                  field |> error_pat_for_sync_field_in_single_field_form ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> result_pat_for_collection ~loc]
                       , [%p
                           collection
                           |> error_pat_for_fields_of_collection_in_multi_field_form_or_single_field_form_with_collection_validator
                                ~loc] )]
                   | Ok None ->
                     collection
                     |> error_pat_for_fields_of_collection_in_single_field_form_without_collection_validator
                          ~loc)
              in
              let entry_of_many (entry : Scheme.entry) =
                match entry with
                | Field field ->
                  field |> error_pat_for_sync_field_in_multi_field_form ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> result_pat_for_collection ~loc]
                       , [%p
                           collection
                           |> error_pat_for_fields_of_collection_in_multi_field_form_or_single_field_form_with_collection_validator
                                ~loc] )]
                   | Ok None ->
                     collection
                     |> error_pat_for_fields_of_collection_in_multi_field_form_or_single_field_form_with_collection_validator
                          ~loc)
              in
              match scheme with
              | x :: [] -> x |> entry_of_one
              | _ -> scheme |> List.rev |> List.rev_map entry_of_many |> Pat.tuple
            in
            let expr =
              let fields_statuses =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> field_dirty_status_record_field ~loc
                     | Collection { collection } ->
                       collection |> collection_statuses_record_field ~loc))
                  None
              in
              match collections with
              | [] ->
                [%expr
                  Invalid
                    { fieldsStatuses = [%e fields_statuses]; collectionsStatuses = () }]
              | _ ->
                [%expr
                  Invalid
                    { fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses =
                        [%e collections |> collections_statuses_record ~loc]
                    }]
            in
            Exp.case pat expr
          in
          Exp.match_
            ~attrs:[ warning_4_disable ~loc ]
            match_values
            [ ok_case; error_case ]]]
    in
    let return_type =
      [%type: (output, fieldsStatuses, collectionsStatuses) formValidationResult]
    in
    [%stri
      let validateForm =
        [%e
          Exp.fun_
            Nolabel
            None
            (Pat.constraint_ [%pat? input] [%type: input])
            (Exp.fun_
               (Labelled "validators")
               None
               (Pat.constraint_
                  (match anything_validatable with
                   | true -> [%pat? validators]
                   | false -> [%pat? _])
                  [%type: validators])
               (Exp.fun_
                  (Labelled "fieldsStatuses")
                  None
                  (Pat.constraint_
                     (match anything_validatable with
                      | true -> [%pat? fieldsStatuses]
                      | false -> [%pat? _])
                     [%type: fieldsStatuses])
                  (match metadata with
                   | None -> return_type |> Exp.constraint_ body
                   | Some () ->
                     Exp.fun_
                       (Labelled "metadata")
                       None
                       (Pat.constraint_
                          (match anything_validatable with
                           | true -> [%pat? metadata]
                           | false -> [%pat? _])
                          [%type: metadata])
                       (return_type |> Exp.constraint_ body))))]
      ;;]
  ;;
end

module Async = struct
  type validating_entry =
    [ `AsyncField of Scheme.field
    | `Collection of Collection.t
    ]

  let ast ~(scheme : Scheme.t) ~(metadata : unit option) ~loc =
    let body =
      [%expr
        [%e
          let collections = scheme |> Scheme.collections in
          let match_values =
            let value (entry : Scheme.entry) =
              match entry with
              | Field
                  ({ validator =
                       SyncValidator (Ok (Required | Optional (Some _)) | Error ())
                   } as field) -> validate_field_with_sync_validator ~field ~metadata ~loc
              | Field ({ validator = SyncValidator (Ok (Optional None)) } as field) ->
                validate_field_without_validator ~field ~loc
              | Field ({ validator = AsyncValidator _ } as field) ->
                validate_field_with_async_validator ~field ~metadata ~loc
              | Collection { collection; fields; validator; output_type } ->
                (match validator with
                 | Ok (Some ()) | Error () ->
                   [%expr
                     [%e validate_whole_collection ~collection ~metadata ~loc]
                     , [%e
                         validate_fields_of_collection_in_async_form
                           ~collection
                           ~fields
                           ~output_type
                           ~metadata
                           ~loc]]
                 | Ok None ->
                   validate_fields_of_collection_in_async_form
                     ~collection
                     ~fields
                     ~output_type
                     ~metadata
                     ~loc)
            in
            match scheme with
            | x :: [] -> x |> value
            | _ -> scheme |> List.rev |> List.rev_map value |> Exp.tuple
          in
          let validating_case =
            let pat =
              let entries_might_be_in_validating_state =
                (scheme
                 |> List.fold_left
                      (fun acc (entry : Scheme.entry) ->
                        match entry with
                        | Field { validator = SyncValidator _ } -> acc
                        | Field ({ validator = AsyncValidator _ } as field) ->
                          `AsyncField field :: acc
                        | Collection { collection } -> `Collection collection :: acc)
                      []
                  : validating_entry list)
              in
              let make (entry : validating_entry) =
                match entry with
                | `AsyncField current_field ->
                  let entry (entry : Scheme.entry) =
                    match entry with
                    | Field ({ validator = AsyncValidator _ } as field)
                      when field.name = current_field.name ->
                      Pat.tuple
                        [ Pat.alias
                            (Pat.variant "Validating" (Some (Pat.any ())))
                            (field_result_var ~field:field.name |> str ~loc)
                        ; Pat.var
                            (field_result_visibility_var ~field:field.name |> str ~loc)
                        ]
                    | Field ({ validator = SyncValidator _ | AsyncValidator _ } as field)
                      -> field |> result_and_visibility_pat_for_field ~loc
                    | Collection { collection; validator } ->
                      (match validator with
                       | Ok (Some ()) | Error () ->
                         [%pat?
                           ( [%p collection |> result_pat_for_collection ~loc]
                           , [%p collection |> result_pat_for_fields_of_collection ~loc] )]
                       | Ok None -> collection |> result_pat_for_fields_of_collection ~loc)
                  in
                  (match scheme with
                   | x :: [] -> x |> entry
                   | _ -> scheme |> List.rev |> List.rev_map entry |> Pat.tuple)
                | `Collection current_collection ->
                  let entry (entry : Scheme.entry) =
                    match entry with
                    | Field field -> field |> result_and_visibility_pat_for_field ~loc
                    | Collection { collection; validator }
                      when collection.plural = current_collection.plural ->
                      (match validator with
                       | Ok (Some ()) | Error () ->
                         [%pat?
                           ( [%p collection |> result_pat_for_collection ~loc]
                           , [%p
                               Pat.alias
                                 (Pat.variant
                                    "ValidatingFieldsOfCollection"
                                    (Some (Pat.any ())))
                                 (collection
                                  |> fields_of_collection_result_var
                                  |> str ~loc)] )]
                       | Ok None ->
                         Pat.alias
                           (Pat.variant
                              "ValidatingFieldsOfCollection"
                              (Some (Pat.any ())))
                           (collection |> fields_of_collection_result_var |> str ~loc))
                    | Collection { collection; validator } ->
                      (match validator with
                       | Ok (Some ()) | Error () ->
                         [%pat?
                           ( [%p collection |> result_pat_for_collection ~loc]
                           , [%p collection |> result_pat_for_fields_of_collection ~loc] )]
                       | Ok None -> collection |> result_pat_for_fields_of_collection ~loc)
                  in
                  (match scheme with
                   | x :: [] -> x |> entry
                   | _ -> scheme |> List.rev |> List.rev_map entry |> Pat.tuple)
              in
              match entries_might_be_in_validating_state with
              | [] ->
                failwith
                  "No entries found that might be in validating state. Please, file an \
                   issue with your use-case."
              | x :: [] -> x |> make
              | x :: rest -> P.or_ ~pat:(x |> make) ~make rest
            in
            let expr =
              let fields_statuses =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field ({ validator = SyncValidator _ } as field) ->
                       field |> field_dirty_status_record_field ~loc
                     | Field ({ validator = AsyncValidator _ } as field) ->
                       field |> async_field_dirty_or_validating_status_record_field ~loc
                     | Collection { collection } ->
                       collection
                       |> collection_that_might_be_in_validating_state_status_record_field
                            ~loc))
                  None
              in
              match collections with
              | [] ->
                [%expr
                  Validating
                    { fieldsStatuses = [%e fields_statuses]; collectionsStatuses = () }]
              | collections ->
                [%expr
                  Validating
                    { fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses =
                        [%e collections |> collections_statuses_record ~loc]
                    }]
            in
            Exp.case pat expr
          in
          let ok_case =
            let pat =
              let entry (entry : Scheme.entry) =
                match entry with
                | Field ({ validator = SyncValidator _ } as field) ->
                  field |> ok_pat_for_sync_field ~loc
                | Field ({ validator = AsyncValidator _ } as field) ->
                  field |> ok_pat_for_async_field ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> ok_pat_for_collection ~loc]
                       , [%p collection |> ok_pat_for_fields_of_async_collection ~loc] )]
                   | Ok None -> collection |> ok_pat_for_fields_of_async_collection ~loc)
              in
              match scheme with
              | x :: [] -> x |> entry
              | _ -> scheme |> List.rev |> List.rev_map entry |> Pat.tuple
            in
            let expr =
              let output =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> output_field_record_field ~loc
                     | Collection { collection } ->
                       collection |> output_collection_record_field ~loc))
                  None
              in
              let fields_statuses =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> field_dirty_status_record_field ~loc
                     | Collection { collection } ->
                       collection |> collection_statuses_record_field ~loc))
                  None
              in
              match collections with
              | [] ->
                [%expr
                  Valid
                    { output = [%e output]
                    ; fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses = ()
                    }]
              | collections ->
                [%expr
                  Valid
                    { output = [%e output]
                    ; fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses =
                        [%e collections |> collections_statuses_record ~loc]
                    }]
            in
            Exp.case pat expr
          in
          let error_case =
            let pat =
              let entry_of_one (entry : Scheme.entry) =
                match entry with
                | Field field ->
                  field |> error_pat_for_async_field_in_single_field_form ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> result_pat_for_collection ~loc]
                       , [%p
                           collection
                           |> error_pat_for_fields_statuses_of_async_collection ~loc] )]
                   | Ok None ->
                     collection
                     |> error_pat_for_fields_of_collection_in_single_field_async_form_without_collection_validator
                          ~loc)
              in
              let entry_of_many (entry : Scheme.entry) =
                match entry with
                | Field ({ validator = SyncValidator _ } as field) ->
                  field |> result_and_visibility_pat_for_field ~loc
                | Field ({ validator = AsyncValidator _ } as field) ->
                  field |> error_pat_for_async_field_in_multi_field_form ~loc
                | Collection { collection; validator } ->
                  (match validator with
                   | Ok (Some ()) | Error () ->
                     [%pat?
                       ( [%p collection |> result_pat_for_collection ~loc]
                       , [%p
                           collection
                           |> error_pat_for_fields_statuses_of_async_collection ~loc] )]
                   | Ok None ->
                     collection |> error_pat_for_fields_statuses_of_async_collection ~loc)
              in
              match scheme with
              | x :: [] -> x |> entry_of_one
              | _ -> scheme |> List.rev |> List.rev_map entry_of_many |> Pat.tuple
            in
            let expr =
              let fields_statuses =
                Exp.record
                  (scheme
                   |> List.rev
                   |> List.rev_map (fun (entry : Scheme.entry) ->
                     match entry with
                     | Field field -> field |> field_dirty_status_record_field ~loc
                     | Collection { collection } ->
                       collection |> collection_statuses_record_field ~loc))
                  None
              in
              match collections with
              | [] ->
                [%expr
                  Invalid
                    { fieldsStatuses = [%e fields_statuses]; collectionsStatuses = () }]
              | collections ->
                [%expr
                  Invalid
                    { fieldsStatuses = [%e fields_statuses]
                    ; collectionsStatuses =
                        [%e collections |> collections_statuses_record ~loc]
                    }]
            in
            Exp.case pat expr
          in
          Exp.match_
            ~attrs:[ warning_4_disable ~loc ]
            match_values
            [ validating_case; ok_case; error_case ]]]
    in
    let return_type =
      [%type: (output, fieldsStatuses, collectionsStatuses) Async.formValidationResult]
    in
    [%stri
      let validateForm =
        [%e
          Exp.fun_
            Nolabel
            None
            (Pat.constraint_ [%pat? input] [%type: input])
            (Exp.fun_
               (Labelled "validators")
               None
               (Pat.constraint_ [%pat? validators] [%type: validators])
               (Exp.fun_
                  (Labelled "fieldsStatuses")
                  None
                  (Pat.constraint_ [%pat? fieldsStatuses] [%type: fieldsStatuses])
                  (match metadata with
                   | None -> return_type |> Exp.constraint_ body
                   | Some () ->
                     Exp.fun_
                       (Labelled "metadata")
                       None
                       (Pat.constraint_ [%pat? metadata] [%type: metadata])
                       (return_type |> Exp.constraint_ body))))]
      ;;]
  ;;
end
