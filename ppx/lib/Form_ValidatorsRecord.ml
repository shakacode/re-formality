open Meta
open Ast
open Printer
open Ppxlib
open Ast_helper

let ensure_eq ~loc fields =
  if fields
     |> List.exists (fun ({ txt = lid }, _) ->
       match lid with
       | Lident "eq" -> true
       | _ -> false)
  then fields
  else (Lident "eq" |> lid ~loc, [%expr ( = )]) :: fields
;;

let update_async_validator_of_field
  ~(field : string)
  ~(output_type : ItemType.t)
  ~(async_mode : AsyncMode.t)
  ~(validator_loc : Location.t)
  ~(metadata : unit option)
  fields
  =
  fields
  |> ensure_eq ~loc:validator_loc
  |> List.rev
  |> List.rev_map (fun (v_lid, ({ pexp_loc = loc } as expr)) ->
    match v_lid with
    | { txt = Lident "validateAsync" } ->
      let fn =
        match metadata with
        | None ->
          [%expr
            fun (value, dispatch) ->
              let validate =
                ([%e expr]
                  : ([%t output_type |> ItemType.unpack], message) Async.validateAsyncFn)
              in
              Async.validateAsync ~value ~validate ~andThen:(fun res ->
                dispatch
                  [%e
                    Exp.construct
                      (Lident (FieldPrinter.apply_async_result_action ~field) |> lid ~loc)
                      (Some
                         (Exp.tuple
                            [ Exp.ident (Lident "value" |> lid ~loc)
                            ; Exp.ident (Lident "res" |> lid ~loc)
                            ]))])]
        | Some () ->
          [%expr
            fun (value, metadata, dispatch) ->
              let validate =
                ([%e expr]
                  : ( [%t output_type |> ItemType.unpack]
                    , message
                    , metadata )
                    Async.validateAsyncFnWithMetadata)
              in
              Async.validateAsyncWithMetadata
                ~value
                ~validate
                ~metadata
                ~andThen:(fun res ->
                  dispatch
                    [%e
                      Exp.construct
                        (Lident (FieldPrinter.apply_async_result_action ~field)
                         |> lid ~loc)
                        (Some
                           (Exp.tuple
                              [ Exp.ident (Lident "value" |> lid ~loc)
                              ; Exp.ident (Lident "res" |> lid ~loc)
                              ]))])]
      in
      ( v_lid
      , (match async_mode with
         | OnBlur -> fn
         | OnChange -> [%expr Debouncer.make ~wait:debounceInterval [%e fn]]) )
    | _ -> v_lid, expr)
;;

let update_async_validator_of_field_of_collection
  ~(field : string)
  ~(collection : Collection.t)
  ~(output_type : ItemType.t)
  ~(async_mode : AsyncMode.t)
  ~(validator_loc : Location.t)
  ~(metadata : unit option)
  fields
  =
  fields
  |> ensure_eq ~loc:validator_loc
  |> List.rev
  |> List.rev_map (fun (v_lid, ({ pexp_loc = loc } as expr)) ->
    match v_lid with
    | { txt = Lident "validateAsync" } ->
      let fn =
        match metadata with
        | None ->
          [%expr
            fun (value, index, dispatch) ->
              let validate =
                ([%e expr]
                  : ([%t output_type |> ItemType.unpack], message) Async.validateAsyncFn)
              in
              Async.validateAsync ~value ~validate ~andThen:(fun res ->
                dispatch
                  [%e
                    Exp.construct
                      (Lident
                         (FieldOfCollectionPrinter.apply_async_result_action
                            ~collection
                            ~field)
                       |> lid ~loc)
                      (Some
                         (Exp.tuple
                            [ Exp.ident (Lident "value" |> lid ~loc)
                            ; Exp.ident (Lident "index" |> lid ~loc)
                            ; Exp.ident (Lident "res" |> lid ~loc)
                            ]))])]
        | Some () ->
          [%expr
            fun (value, index, metadata, dispatch) ->
              let validate =
                ([%e expr]
                  : ( [%t output_type |> ItemType.unpack]
                    , message
                    , metadata )
                    Async.validateAsyncFnWithMetadata)
              in
              Async.validateAsyncWithMetadata
                ~value
                ~validate
                ~metadata
                ~andThen:(fun res ->
                  dispatch
                    [%e
                      Exp.construct
                        (Lident
                           (FieldOfCollectionPrinter.apply_async_result_action
                              ~collection
                              ~field)
                         |> lid ~loc)
                        (Some
                           (Exp.tuple
                              [ Exp.ident (Lident "value" |> lid ~loc)
                              ; Exp.ident (Lident "index" |> lid ~loc)
                              ; Exp.ident (Lident "res" |> lid ~loc)
                              ]))])]
      in
      ( v_lid
      , (match async_mode with
         | OnBlur -> fn
         | OnChange -> [%expr Debouncer.make ~wait:debounceInterval [%e fn]]) )
    | _ -> v_lid, expr)
;;

let ast
  ~(scheme : Scheme.t)
  ~(metadata : unit option)
  ~(validators : ValidatorsRecord.t)
  (value_binding : value_binding)
  =
  let fields =
    validators.fields
    |> List.rev
    |> List.rev_map (fun (f_lid, expr) ->
      match f_lid with
      | { txt = Lident key } ->
        let entry =
          scheme
          |> List.find_opt (function
            | Scheme.Field field -> field.name = key
            | Scheme.Collection { collection } -> collection.plural = key)
        in
        (match entry with
         | None -> f_lid, expr
         | Some (Field field) ->
           (match field.validator with
            | SyncValidator (Ok Required) -> f_lid, expr
            | SyncValidator (Ok (Optional (Some ()))) -> f_lid, expr
            | SyncValidator (Ok (Optional None)) ->
              let loc = expr.pexp_loc in
              f_lid, [%expr ()]
            | SyncValidator (Error ()) -> f_lid, expr
            | AsyncValidator { mode = async_mode } ->
              ( f_lid
              , (match expr with
                 | { pexp_desc = Pexp_record (fields, None)
                   ; pexp_loc
                   ; pexp_loc_stack
                   ; pexp_attributes
                   } ->
                   { pexp_desc =
                       Pexp_record
                         ( fields
                           |> update_async_validator_of_field
                                ~field:field.name
                                ~output_type:field.output_type
                                ~async_mode
                                ~validator_loc:pexp_loc
                                ~metadata
                         , None )
                   ; pexp_loc
                   ; pexp_loc_stack
                   ; pexp_attributes
                   }
                 | _ -> expr) ))
         | Some
             (Collection
               { collection
               ; fields = collection_fields
               ; validator = collection_validator
               }) ->
           ( f_lid
           , (match expr with
              | { pexp_desc = Pexp_record (collection_validator_fields, None)
                ; pexp_loc
                ; pexp_loc_stack
                ; pexp_attributes
                } ->
                let fields =
                  collection_validator_fields
                  |> List.rev
                  |> List.rev_map (fun (c_lid, expr) ->
                    match c_lid with
                    | { txt = Lident "collection" } ->
                      ( c_lid
                      , (match collection_validator with
                         | Ok (Some ()) | Error () -> expr
                         | Ok None ->
                           let loc = expr.pexp_loc in
                           [%expr ()]) )
                    | { txt = Lident "fields" } ->
                      ( c_lid
                      , (match expr with
                         | { pexp_desc = Pexp_record (field_validator_fields, None)
                           ; pexp_loc
                           ; pexp_loc_stack
                           ; pexp_attributes
                           } ->
                           let fields =
                             field_validator_fields
                             |> List.rev
                             |> List.rev_map (fun (f_lid, expr) ->
                               match f_lid with
                               | { txt = Lident key } ->
                                 let field =
                                   collection_fields
                                   |> List.find_opt (fun (field : Scheme.field) ->
                                     field.name = key)
                                 in
                                 (match field with
                                  | None -> f_lid, expr
                                  | Some { validator = SyncValidator (Ok Required) } ->
                                    f_lid, expr
                                  | Some
                                      { validator =
                                          SyncValidator (Ok (Optional (Some ())))
                                      } -> f_lid, expr
                                  | Some
                                      { validator = SyncValidator (Ok (Optional None)) }
                                    ->
                                    let loc = expr.pexp_loc in
                                    f_lid, [%expr ()]
                                  | Some { validator = SyncValidator (Error ()) } ->
                                    f_lid, expr
                                  | Some
                                      ({ validator = AsyncValidator { mode = async_mode }
                                       } as field) ->
                                    ( f_lid
                                    , (match expr with
                                       | { pexp_desc = Pexp_record (fields, None)
                                         ; pexp_loc
                                         ; pexp_loc_stack
                                         ; pexp_attributes
                                         } ->
                                         { pexp_desc =
                                             Pexp_record
                                               ( fields
                                                 |> update_async_validator_of_field_of_collection
                                                      ~field:field.name
                                                      ~collection
                                                      ~output_type:field.output_type
                                                      ~async_mode
                                                      ~validator_loc:pexp_loc
                                                      ~metadata
                                               , None )
                                         ; pexp_loc
                                         ; pexp_loc_stack
                                         ; pexp_attributes
                                         }
                                       | _ -> expr) ))
                               | { txt = _ } -> f_lid, expr)
                           in
                           { pexp_desc = Pexp_record (fields, None)
                           ; pexp_loc
                           ; pexp_loc_stack
                           ; pexp_attributes
                           }
                         | _ -> expr) )
                    | _ -> c_lid, expr)
                in
                { pexp_desc = Pexp_record (fields, None)
                ; pexp_loc
                ; pexp_loc_stack
                ; pexp_attributes
                }
              | _ -> expr) ))
      | _ -> f_lid, expr)
  in
  { value_binding with
    pvb_expr =
      { pexp_desc =
          Pexp_constraint
            ( { pexp_desc = Pexp_record (fields, None)
              ; pexp_loc = validators.record_metadata.pexp_loc
              ; pexp_loc_stack = validators.record_metadata.pexp_loc_stack
              ; pexp_attributes = validators.record_metadata.pexp_attributes
              }
            , validators.annotation )
      ; pexp_loc = validators.constraint_metadata.pexp_loc
      ; pexp_loc_stack = validators.constraint_metadata.pexp_loc_stack
      ; pexp_attributes = validators.constraint_metadata.pexp_attributes
      }
  }
;;
