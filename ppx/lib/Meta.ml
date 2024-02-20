open Ppxlib

module ItemType = struct
  module T : sig
    type t
  end = struct
    type t = core_type
  end

  type t = T.t

  external make : core_type -> t = "%identity"
  external unpack : t -> core_type = "%identity"

  let rec eq (t1 : core_type) (t2 : core_type) =
    match t1.ptyp_desc, t2.ptyp_desc with
    | Ptyp_constr ({ txt = lid1 }, list1), Ptyp_constr ({ txt = lid2 }, list2) ->
      eq_lid lid1 lid2 && eq_list list1 list2
    | Ptyp_var x1, Ptyp_var x2 -> x1 = x2
    | Ptyp_tuple l1, Ptyp_tuple l2 -> eq_list l1 l2
    | _ -> false

  and eq_lid (l1 : Longident.t) (l2 : Longident.t) =
    match l1, l2 with
    | Lident x1, Lident x2 -> x1 = x2
    | Ldot (l1, x1), Ldot (l2, x2) -> x1 = x2 && eq_lid l1 l2
    | Lapply (l1, l1'), Lapply (l2, l2') -> eq_lid l1 l2 && eq_lid l1' l2'
    | _ -> false

  and eq_list (l1 : core_type list) (l2 : core_type list) =
    if List.length l1 = List.length l2
    then List.for_all2 (fun t1 t2 -> eq t1 t2) l1 l2
    else false
  ;;

  let eq (x1 : t) (x2 : t) = eq (x1 |> unpack) (x2 |> unpack)
end

module Collection = struct
  type t =
    { singular : string
    ; plural : string
    }
end

module FieldDep = struct
  type t =
    | DepField of string
    | DepFieldOfCollection of
        { collection : Collection.t
        ; field : string
        }

  type unvalidated =
    | UnvalidatedDepField of
        { name : string
        ; loc : Location.t
        }
    | UnvalidatedDepFieldOfCollection of
        { collection : string
        ; field : string
        ; c_loc : Location.t
        ; f_loc : Location.t
        }
end

module FieldOptionality = struct
  type t =
    | OptionType
    | StringType
    | OptionStringType
end

module AsyncMode = struct
  type t =
    | OnChange
    | OnBlur

  let default = OnChange
end

module ValidatorsRecord = struct
  type t =
    { fields : fields
    ; rec_flag : rec_flag
    ; constraint_metadata : metadata
    ; record_metadata : metadata
    ; annotation : core_type
    }

  and fields = (Longident.t loc * expression) list

  and metadata =
    { pexp_loc : Location.t
    ; pexp_loc_stack : Location.t list
    ; pexp_attributes : attribute list
    }
end

module FieldValidator = struct
  type t =
    | SyncValidator of (sync, unit) result
    | AsyncValidator of
        { mode : AsyncMode.t
        ; optionality : FieldOptionality.t option
        }

  and sync =
    | Required
    | Optional of unit option
end

module CollectionValidator = struct
  type t = (unit option, unit) result
end

module Scheme = struct
  type t = entry list

  and entry =
    | Field of field
    | Collection of collection

  and field =
    { name : string
    ; input_type : ItemType.t
    ; output_type : ItemType.t
    ; validator : FieldValidator.t
    ; deps : FieldDep.t list
    }

  and collection =
    { collection : Collection.t
    ; fields : field list
    ; validator : CollectionValidator.t
    ; input_type : ItemType.t
    ; output_type : ItemType.t
    }

  let fields (scheme : t) =
    scheme
    |> List.fold_left
         (fun acc entry ->
           match entry with
           | Field field -> field :: acc
           | Collection _ -> acc)
         []
  ;;

  let collections (scheme : t) =
    scheme
    |> List.fold_left
         (fun acc entry ->
           match entry with
           | Field _ -> acc
           | Collection collection -> collection :: acc)
         []
  ;;
end

module InputFieldData = struct
  type unvalidated =
    { name : string
    ; typ : ItemType.t
    ; async : AsyncMode.t option
    ; deps : FieldDep.unvalidated list
    }

  type validated =
    { name : string
    ; typ : ItemType.t
    ; async : AsyncMode.t option
    ; deps : FieldDep.t list
    }

  let unvalidated ~async ~deps (field : label_declaration) : unvalidated =
    { name = field.pld_name.txt; typ = field.pld_type |> ItemType.make; async; deps }
  ;;

  let validated ~deps (field : unvalidated) : validated =
    { name = field.name; typ = field.typ; async = field.async; deps }
  ;;
end

module InputField = struct
  type unvalidated =
    | UnvalidatedInputField of InputFieldData.unvalidated
    | UnvalidatedInputFieldOfCollection of
        { collection : Collection.t
        ; field : InputFieldData.unvalidated
        }

  type validated =
    | ValidatedInputField of InputFieldData.validated
    | ValidatedInputFieldOfCollection of
        { collection : Collection.t
        ; field : InputFieldData.validated
        }
end

module OutputFieldData = struct
  type t =
    { name : string
    ; typ : ItemType.t
    ; loc : Location.t
    }
end

module OutputField = struct
  type t =
    | OutputField of OutputFieldData.t
    | OutputFieldOfCollection of
        { collection : Collection.t
        ; field : OutputFieldData.t
        }
end

module InputType = struct
  module T : sig
    type t
  end = struct
    type t = type_declaration
  end

  type t = T.t

  external make : type_declaration -> t = "%identity"
  external type_declaration : t -> type_declaration = "%identity"
end

module OutputType = struct
  module T : sig
    type t
  end = struct
    type t = type_declaration
  end

  type t = T.t

  external make : type_declaration -> t = "%identity"
  external type_declaration : t -> type_declaration = "%identity"

  let default ~loc = [%stri type output = input]
end

module MessageType = struct
  let default ~loc = [%stri type message = string]
end

module DebounceInterval = struct
  let default ~loc = [%stri let debounceInterval = 700]
end

module SubmissionErrorType = struct
  let default ~loc = [%stri type submissionError = unit]
end

module FieldOptionalityParser = struct
  let parse (typ : ItemType.t) : FieldOptionality.t option =
    match typ |> ItemType.unpack with
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Some StringType
    | { ptyp_desc =
          Ptyp_constr
            ( { txt = Lident "option" }
            , { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } :: [] )
      } -> Some OptionStringType
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, _) } -> Some OptionType
    | _ -> None
  ;;
end

module AsyncFieldParser = struct
  type error =
    | InvalidPayload of Location.t
    | InvalidAsyncMode of Location.t

  let attr field =
    field.pld_type.ptyp_attributes
    |> List.find_opt (fun attr ->
      match attr with
      | { attr_name = { txt = "field.async" } } -> true
      | _ -> false)
  ;;

  let parse attribute =
    match attribute with
    | { attr_payload = PStr []; attr_loc = _ } -> Ok AsyncMode.default
    | { attr_payload =
          PStr
            ({ pstr_desc =
                 Pstr_eval
                   ( { pexp_desc =
                         Pexp_record
                           ( ( { txt = Lident "mode" }
                             , { pexp_desc =
                                   Pexp_construct ({ txt = Lident mode; loc }, None)
                               } )
                             :: []
                           , None )
                     }
                   , _ )
             }
            :: [])
      ; attr_loc = _
      } ->
      (match mode with
       | "OnChange" -> Ok OnChange
       | "OnBlur" -> Ok OnBlur
       | _ -> Error (InvalidAsyncMode loc))
    | { attr_payload = PStr ({ pstr_loc } :: []) } -> Error (InvalidPayload pstr_loc)
    | { attr_loc } -> Error (InvalidPayload attr_loc)
  ;;

  let get field =
    match field |> attr with
    | None -> Ok None
    | Some attr ->
      (match attr |> parse with
       | Ok mode -> Ok (Some mode)
       | Error error -> Error error)
  ;;
end

module FieldDepsParser = struct
  type error =
    | DepsParseError of Location.t
    | DepNotFound of FieldDep.unvalidated
    | DepOfItself of [ `Field of string * Location.t ]
    | DepDuplicate of FieldDep.unvalidated

  let attr field =
    field.pld_type.ptyp_attributes
    |> List.find_opt (fun attr ->
      match attr with
      | { attr_name = { txt = "field.deps" } } -> true
      | _ -> false)
  ;;

  let parse (attribute : attribute) : (FieldDep.unvalidated list, error) result =
    match attribute with
    | { attr_payload = PStr ({ pstr_desc = Pstr_eval (exp, _) } :: []); attr_loc = _ } ->
      (match exp with
       | { pexp_desc = Pexp_ident { txt = Lident dep; loc } } ->
         Ok [ UnvalidatedDepField { name = dep; loc } ]
       | { pexp_desc =
             Pexp_field
               ( { pexp_desc = Pexp_ident { txt = Lident collection; loc = c_loc } }
               , { txt = Lident field; loc = f_loc } )
         } -> Ok [ UnvalidatedDepFieldOfCollection { collection; field; c_loc; f_loc } ]
       | { pexp_desc = Pexp_tuple exps } ->
         exps
         |> List.fold_left
              (fun (res : (FieldDep.unvalidated list, error) result) exp ->
                match res, exp with
                | Error error, _ -> Error error
                | Ok deps, { pexp_desc = Pexp_ident { txt = Lident dep; loc } } ->
                  Ok (UnvalidatedDepField { name = dep; loc } :: deps)
                | ( Ok deps
                  , { pexp_desc =
                        Pexp_field
                          ( { pexp_desc =
                                Pexp_ident { txt = Lident collection; loc = c_loc }
                            }
                          , { txt = Lident field; loc = f_loc } )
                    } ) ->
                  Ok
                    (UnvalidatedDepFieldOfCollection { collection; field; c_loc; f_loc }
                     :: deps)
                | Ok _, { pexp_loc } -> Error (DepsParseError pexp_loc))
              (Ok [])
       | { pexp_loc } -> Error (DepsParseError pexp_loc))
    | { attr_loc } -> Error (DepsParseError attr_loc)
  ;;

  let get field =
    match field |> attr with
    | None -> Ok []
    | Some attr -> attr |> parse
  ;;
end

module FieldCollectionParser = struct
  type result = (ok, error) Stdlib.result

  and ok =
    { collection : Collection.t
    ; fields : InputFieldData.unvalidated list
    ; input_type : ItemType.t
    }

  and error =
    | NotArray of Location.t
    | InvalidTypeRef of Location.t
    | RecordNotFound of Location.t
    | NotRecord of Location.t
    | InvalidAsyncField of AsyncFieldParser.error
    | InvalidFieldDeps of FieldDepsParser.error

  let attr (field : label_declaration) =
    field.pld_type.ptyp_attributes
    |> List.find_opt (fun attr ->
      match attr with
      | { attr_name = { txt = "field.collection" } } -> true
      | _ -> false)
  ;;

  let parse ~(structure : structure) (field : label_declaration) : result =
    match field.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "array"; loc = arr_loc }, payload) ->
      (match payload with
       | [] -> Error (InvalidTypeRef arr_loc)
       | ({ ptyp_desc = Ptyp_constr ({ txt = Lident typ_name }, []); ptyp_loc } as
          input_type)
         :: _ ->
         let record_type = ref None in
         structure
         |> List.iter (fun (item : structure_item) ->
           match item with
           | { pstr_desc = Pstr_type (_rec_flag, decls) } ->
             decls
             |> List.iter (fun (decl : type_declaration) ->
               match decl with
               | { ptype_name = { txt = name } } when name = typ_name ->
                 (match decl.ptype_kind with
                  | Ptype_record fields -> record_type := Some (Ok fields)
                  | _ -> record_type := Some (Error (NotRecord decl.ptype_loc)))
               | _ -> ())
           | _ -> ());
         (match !record_type with
          | None -> Error (RecordNotFound ptyp_loc)
          | Some (Error error) -> Error error
          | Some (Ok fields) ->
            let fields =
              fields
              |> List.fold_left
                   (fun res (field : label_declaration) ->
                     match res with
                     | Error error -> Error error
                     | Ok fields ->
                       (match
                          field |> AsyncFieldParser.get, field |> FieldDepsParser.get
                        with
                        | Ok async, Ok deps ->
                          Ok ((field |> InputFieldData.unvalidated ~async ~deps) :: fields)
                        | Error error, _ -> Error (InvalidAsyncField error)
                        | _, Error error -> Error (InvalidFieldDeps error)))
                   (Ok [])
            in
            (match fields with
             | Ok fields ->
               Ok
                 { collection = { plural = field.pld_name.txt; singular = typ_name }
                 ; fields
                 ; input_type = input_type |> ItemType.make
                 }
             | Error error -> Error error))
       | { ptyp_loc } :: _ -> Error (InvalidTypeRef ptyp_loc))
    | _ -> Error (NotArray field.pld_loc)
  ;;
end

module FieldAttributesParser = struct
  type result = (ok option, error) Stdlib.result

  and ok =
    | Collection of FieldCollectionParser.ok
    | AsyncDeps of
        { async : AsyncMode.t option
        ; deps : FieldDep.unvalidated list
        }

  and error =
    | Conflict of
        [ `AsyncWithCollection of Location.t | `DepsWithCollection of Location.t ]
    | InvalidCollectionField of FieldCollectionParser.error
    | InvalidAsyncField of AsyncFieldParser.error
    | InvalidFieldDeps of FieldDepsParser.error

  let parse ~(structure : structure) (field : label_declaration) =
    match
      ( field |> FieldCollectionParser.attr
      , field |> AsyncFieldParser.attr
      , field |> FieldDepsParser.attr )
    with
    | Some _, None, None ->
      (match field |> FieldCollectionParser.parse ~structure with
       | Ok collection -> Ok (Some (Collection collection))
       | Error error -> Error (InvalidCollectionField error))
    | None, Some async_attr, Some deps_attr ->
      (match async_attr |> AsyncFieldParser.parse, deps_attr |> FieldDepsParser.parse with
       | Ok async, Ok deps -> Ok (Some (AsyncDeps { async = Some async; deps }))
       | Error error, _ -> Error (InvalidAsyncField error)
       | _, Error error -> Error (InvalidFieldDeps error))
    | None, Some async_attr, None ->
      (match async_attr |> AsyncFieldParser.parse with
       | Ok async -> Ok (Some (AsyncDeps { async = Some async; deps = [] }))
       | Error error -> Error (InvalidAsyncField error))
    | None, None, Some deps_attr ->
      (match deps_attr |> FieldDepsParser.parse with
       | Ok deps -> Ok (Some (AsyncDeps { async = None; deps }))
       | Error error -> Error (InvalidFieldDeps error))
    | None, None, None -> Ok None
    | Some _, Some { attr_loc }, _ -> Error (Conflict (`AsyncWithCollection attr_loc))
    | Some _, _, Some { attr_loc } -> Error (Conflict (`DepsWithCollection attr_loc))
  ;;
end

module InputTypeParser = struct
  type result = (ok, error) Stdlib.result

  and ok =
    { entries : unvalidated_entry list
    ; type_declaration : InputType.t
    }

  and unvalidated_entry =
    | UnvalidatedInputField of InputFieldData.unvalidated
    | UnvalidatedInputCollection of
        { collection : Collection.t
        ; fields : InputFieldData.unvalidated list
        ; input_type : ItemType.t
        }

  and validated_entry =
    | ValidatedInputField of InputFieldData.validated
    | ValidatedInputCollection of
        { collection : Collection.t
        ; fields : InputFieldData.validated list
        ; input_type : ItemType.t
        }

  and error =
    | NotFound
    | NotRecord of Location.t
    | InvalidAttributes of FieldAttributesParser.error

  let parse ~decl ~structure ~loc:_ fields =
    let entries =
      fields
      |> List.rev
      |> List.fold_left
           (fun res field ->
             match res, field |> FieldAttributesParser.parse ~structure with
             | ( Ok entries
               , ((Ok (Some (Collection { collection; fields; input_type }))) [@explicit_arity
                                                                                ]) ) ->
               Ok
                 ((UnvalidatedInputCollection { collection; fields; input_type } [@explicit_arity
                                                                                   ])
                  :: entries)
             | Ok entries, Ok (Some (AsyncDeps { async; deps })) ->
               Ok
                 (UnvalidatedInputField (field |> InputFieldData.unvalidated ~async ~deps)
                  :: entries)
             | Ok entries, Ok None ->
               Ok
                 ((UnvalidatedInputField
                     (field |> InputFieldData.unvalidated ~async:None ~deps:[]) [@explicit_arity
                                                                                  ])
                  :: entries)
             | Error error, _ -> Error error
             | _, Error error -> Error (InvalidAttributes error))
           (Ok [])
    in
    match entries with
    | Error error -> Error error
    | Ok entries -> Ok { entries; type_declaration = decl |> InputType.make }
  ;;

  let validate (unvalidated_entries : unvalidated_entry list)
    : (validated_entry list, FieldDepsParser.error) Stdlib.result
    =
    let dup (deps : FieldDep.unvalidated list) (dep : FieldDep.unvalidated) =
      match
        deps
        |> List.find_all (fun (dep' : FieldDep.unvalidated) ->
          match dep, dep' with
          | UnvalidatedDepField { name = dep }, UnvalidatedDepField { name = dep' } ->
            dep = dep'
          | ( UnvalidatedDepFieldOfCollection { collection; field }
            , UnvalidatedDepFieldOfCollection { collection = collection'; field = field' }
            ) -> collection = collection' && field = field'
          | UnvalidatedDepField _, UnvalidatedDepFieldOfCollection _
          | UnvalidatedDepFieldOfCollection _, UnvalidatedDepField _ -> false)
        |> List.length
      with
      | 0 | 1 -> None
      | _ -> Some ()
    in
    unvalidated_entries
    |> List.fold_left
         (fun (res : (validated_entry list, FieldDepsParser.error) Stdlib.result)
              (unvalidated_entry : unvalidated_entry) ->
           match res, unvalidated_entry with
           | Error error, _ -> Error error
           | Ok validated_entries, UnvalidatedInputField field ->
             let deps =
               field.deps
               |> List.fold_left
                    (fun (res : (FieldDep.t list, FieldDepsParser.error) Stdlib.result)
                         dep ->
                      match res with
                      | Error error -> Error error
                      | Ok validated_deps ->
                        (match dep |> dup field.deps with
                         | Some () -> Error (FieldDepsParser.DepDuplicate dep)
                         | None ->
                           (match
                              unvalidated_entries
                              |> List.fold_left
                                   (fun (res :
                                          ( FieldDep.t
                                          , FieldDepsParser.error )
                                          Stdlib.result
                                          option)
                                        entry ->
                                     match res, dep, entry with
                                     | (Some _ as res), _, _ -> res
                                     | ( None
                                       , UnvalidatedDepField dep'
                                       , UnvalidatedInputField field' ) ->
                                       if field.name = field'.name
                                          && field'.name = dep'.name
                                       then
                                         Some
                                           (Error
                                              (FieldDepsParser.DepOfItself
                                                 (`Field (dep'.name, dep'.loc)) [@explicit_arity
                                                                                  ]) [@explicit_arity
                                                                                       ]) [@explicit_arity
                                                                                          ]
                                       else if field'.name = dep'.name
                                       then Some (Ok (DepField dep'.name))
                                       else None
                                     | ( None
                                       , ((UnvalidatedDepFieldOfCollection dep') [@explicit_arity
                                                                                   ])
                                       , ((UnvalidatedInputCollection entry') [@explicit_arity
                                                                                ]) ) ->
                                       if dep'.collection <> entry'.collection.singular
                                       then None
                                       else (
                                         match
                                           entry'.fields
                                           |> List.find_opt
                                                (fun
                                                    (field : InputFieldData.unvalidated)
                                                  -> dep'.field = field.name)
                                         with
                                         | None ->
                                           Some
                                             (Error
                                                (FieldDepsParser.DepNotFound dep [@explicit_arity
                                                                                   ]) [@explicit_arity
                                                                                        ])
                                         | Some field ->
                                           Some
                                             (Ok
                                                (DepFieldOfCollection
                                                   { collection = entry'.collection
                                                   ; field = field.name
                                                   })))
                                     | ( None
                                       , UnvalidatedDepField _
                                       , UnvalidatedInputCollection _ )
                                     | ( None
                                       , UnvalidatedDepFieldOfCollection _
                                       , UnvalidatedInputField _ ) -> None)
                                   None
                            with
                            | None -> Error (FieldDepsParser.DepNotFound dep)
                            | Some (Error error) -> Error error
                            | Some (Ok dep_entry) -> Ok (dep_entry :: validated_deps))))
                    (Ok [])
             in
             (match deps with
              | Error error -> Error error
              | Ok deps ->
                Ok
                  ((ValidatedInputField (field |> InputFieldData.validated ~deps) [@explicit_arity
                                                                                    ])
                   :: validated_entries))
           | ( Ok validated_entries
             , ((UnvalidatedInputCollection
                  { collection; fields = unvalidated_fields; input_type }) [@explicit_arity
                                                                             ]) ) ->
             let validated_fields =
               unvalidated_fields
               |> List.fold_left
                    (fun (res :
                           ( InputFieldData.validated list
                           , FieldDepsParser.error )
                           Stdlib.result)
                         (field : InputFieldData.unvalidated) ->
                      match res with
                      | Error error -> Error error
                      | Ok validated_fields ->
                        let deps =
                          field.deps
                          |> List.fold_left
                               (fun (res :
                                      ( FieldDep.t list
                                      , FieldDepsParser.error )
                                      Stdlib.result)
                                    dep ->
                                 match res with
                                 | Error error -> Error error
                                 | Ok validated_deps ->
                                   (match dep |> dup field.deps with
                                    | Some () -> Error (FieldDepsParser.DepDuplicate dep)
                                    | None ->
                                      (match
                                         unvalidated_entries
                                         |> List.fold_left
                                              (fun (res :
                                                     ( FieldDep.t
                                                     , FieldDepsParser.error )
                                                     Stdlib.result
                                                     option)
                                                   entry ->
                                                match res, dep, entry with
                                                | (Some _ as res), _, _ -> res
                                                | ( None
                                                  , ((UnvalidatedDepField dep') [@explicit_arity
                                                                                  ])
                                                  , ((UnvalidatedInputField field') [@explicit_arity
                                                                                      ]) )
                                                  ->
                                                  if field'.name = dep'.name
                                                  then
                                                    Some
                                                      (Ok
                                                         (DepField dep'.name [@explicit_arity
                                                                               ]) [@explicit_arity
                                                                                    ]) [@explicit_arity
                                                                                         ]
                                                  else None
                                                | ( None
                                                  , UnvalidatedDepFieldOfCollection dep'
                                                  , ((UnvalidatedInputCollection entry') [@explicit_arity
                                                                                          ])
                                                  ) ->
                                                  if dep'.collection
                                                     <> entry'.collection.singular
                                                  then None
                                                  else (
                                                    match
                                                      entry'.fields
                                                      |> List.fold_left
                                                           (fun (res :
                                                                  ( FieldDep.t
                                                                  , FieldDepsParser.error
                                                                  )
                                                                  Stdlib.result
                                                                  option)
                                                                (field :
                                                                  InputFieldData
                                                                  .unvalidated) ->
                                                             match res with
                                                             | Some _ -> res
                                                             | None ->
                                                               if dep'.field = field.name
                                                               then
                                                                 Some
                                                                   (Ok
                                                                      (DepFieldOfCollection
                                                                         { collection =
                                                                             entry'
                                                                               .collection
                                                                         ; field =
                                                                             field.name
                                                                         } [@explicit_arity
                                                                             ]) [@explicit_arity
                                                                                  ]) [@explicit_arity
                                                                                       ]
                                                               else None)
                                                           None
                                                    with
                                                    | None ->
                                                      Some
                                                        (Error
                                                           (FieldDepsParser.DepNotFound
                                                              dep))
                                                    | Some (Error error) ->
                                                      Some (Error error)
                                                    | Some (Ok dep) -> Some (Ok dep))
                                                | ( None
                                                  , UnvalidatedDepField _
                                                  , UnvalidatedInputCollection _ )
                                                | ( None
                                                  , UnvalidatedDepFieldOfCollection _
                                                  , UnvalidatedInputField _ ) -> None)
                                              None
                                       with
                                       | None ->
                                         Error
                                           (FieldDepsParser.DepNotFound dep [@explicit_arity
                                                                              ]) [@explicit_arity
                                                                                   ]
                                       | Some (Error error) -> Error error
                                       | Some (Ok dep_entry) ->
                                         Ok (dep_entry :: validated_deps))))
                               (Ok [])
                        in
                        (match deps with
                         | Error error -> Error error
                         | Ok deps ->
                           Ok
                             ((field |> InputFieldData.validated ~deps)
                              :: validated_fields)))
                    (Ok [])
             in
             (match validated_fields with
              | Error error -> Error error
              | Ok validated_fields ->
                Ok
                  ((ValidatedInputCollection
                      { collection; fields = validated_fields; input_type } [@explicit_arity
                                                                              ])
                   :: validated_entries)))
         (Ok [])
  ;;

  let in_deps_of (entries : validated_entry list) (field : InputField.validated)
    : InputField.validated option
    =
    entries
    |> List.fold_left
         (fun res (entry : validated_entry) ->
           match res, field, entry with
           | Some _, _, _ -> res
           | None, ValidatedInputField subject_field, ValidatedInputField entry_field ->
             entry_field.deps
             |> List.fold_left
                  (fun (res : InputField.validated option) (dep : FieldDep.t) ->
                    match res, dep with
                    | Some _, _ -> res
                    | None, DepField dep ->
                      if dep = subject_field.name
                      then Some (ValidatedInputField entry_field)
                      else None
                    | None, DepFieldOfCollection _ -> None)
                  None
           | ( None
             , ValidatedInputField subject_field
             , ((ValidatedInputCollection
                  { collection = entry_collection; fields = entry_fields }) [@explicit_arity
                                                                              ]) ) ->
             entry_fields
             |> List.fold_left
                  (fun (_res : InputField.validated option)
                       (entry_field : InputFieldData.validated) ->
                    entry_field.deps
                    |> List.fold_left
                         (fun (res : InputField.validated option) (dep : FieldDep.t) ->
                           match res, dep with
                           | Some _, _ -> res
                           | None, DepField dep ->
                             if dep = subject_field.name
                             then
                               Some
                                 (ValidatedInputFieldOfCollection
                                    { collection = entry_collection; field = entry_field })
                             else None
                           | None, DepFieldOfCollection _ -> None)
                         None)
                  None
           | ( None
             , ((ValidatedInputFieldOfCollection
                  { collection = subject_collection; field = subject_field }) [@explicit_arity
                                                                                ])
             , ValidatedInputField entry_field ) ->
             entry_field.deps
             |> List.fold_left
                  (fun (res : InputField.validated option) (dep : FieldDep.t) ->
                    match res, dep with
                    | Some _, _ -> res
                    | None, DepField _dep -> None
                    | ( None
                      , ((DepFieldOfCollection
                           { collection = dep_collection; field = dep_field }) [@explicit_arity
                                                                                 ]) ) ->
                      if dep_collection.singular = subject_collection.singular
                         && dep_field = subject_field.name
                      then Some (ValidatedInputField entry_field)
                      else None)
                  None
           | ( None
             , ((ValidatedInputFieldOfCollection
                  { collection = subject_collection; field = subject_field }) [@explicit_arity
                                                                                ])
             , ((ValidatedInputCollection
                  { collection = entry_collection; fields = entry_fields }) [@explicit_arity
                                                                              ]) ) ->
             entry_fields
             |> List.fold_left
                  (fun (_res : InputField.validated option)
                       (entry_field : InputFieldData.validated) ->
                    entry_field.deps
                    |> List.fold_left
                         (fun (res : InputField.validated option) (dep : FieldDep.t) ->
                           match res, dep with
                           | Some _, _ -> res
                           | None, DepField _dep -> None
                           | ( None
                             , ((DepFieldOfCollection
                                  { collection = dep_collection; field = dep_field }) [@explicit_arity
                                                                                        ])
                             ) ->
                             if subject_collection.singular = dep_collection.singular
                                && subject_field.name = dep_field
                             then
                               Some
                                 (ValidatedInputFieldOfCollection
                                    { collection = entry_collection; field = entry_field })
                             else None)
                         None)
                  None)
         None
  ;;
end

module OutputTypeParser = struct
  type result = (ok, error) Stdlib.result

  and ok =
    | NotProvided
    | AliasOfInput
    | Record of
        { entries : entry list
        ; loc : Location.t
        }

  and entry =
    | OutputField of OutputFieldData.t
    | OutputCollection of
        { collection : Collection.t
        ; fields : OutputFieldData.t list
        ; output_type : ItemType.t
        }

  and error =
    | InputNotAvailable of Location.t
    | NotRecord of Location.t
    | BadTypeAlias of
        { alias : string
        ; loc : Location.t
        }
    | OutputCollectionNotFound of
        { input_collection : Collection.t
        ; loc : Location.t
        }
    | InvalidCollection of collection_error

  and collection_error =
    | InvalidCollectionTypeRef of Location.t
    | CollectionTypeNotRecord of Location.t
    | CollectionTypeNotFound of Location.t
    | CollectionOutputNotArray of Location.t

  let flatten (entries : entry list) : OutputField.t list =
    entries
    |> List.rev
    |> List.fold_left
         (fun acc entry ->
           match entry with
           | OutputField field -> OutputField.OutputField field :: acc
           | OutputCollection { collection; fields } ->
             fields
             |> List.rev
             |> List.fold_left
                  (fun acc field ->
                    (OutputField.OutputFieldOfCollection { collection; field } [@explicit_arity
                                                                                 ])
                    :: acc)
                  acc)
         []
  ;;

  let parse
    ~structure
    ~(input_collections : Collection.t list)
    ~loc
    (fields : label_declaration list)
    =
    match input_collections with
    | [] ->
      Ok
        (Record
           { loc
           ; entries =
               fields
               |> List.rev
               |> List.fold_left
                    (fun acc field ->
                      OutputField
                        { name = field.pld_name.txt
                        ; typ = field.pld_type |> ItemType.make
                        ; loc = field.pld_loc
                        }
                      :: acc)
                    []
           })
    | _ ->
      let entries =
        fields
        |> List.rev
        |> List.fold_left
             (fun acc field ->
               match acc with
               | Error error -> Error error
               | Ok entries ->
                 let field_name = field.pld_name.txt in
                 (match
                    input_collections
                    |> List.find_opt (fun (collection : Collection.t) ->
                      collection.plural = field_name)
                  with
                  | None ->
                    Ok
                      (OutputField
                         { name = field_name
                         ; typ = field.pld_type |> ItemType.make
                         ; loc = field.pld_loc
                         }
                       :: entries)
                  | Some _input_collection ->
                    (match field.pld_type.ptyp_desc with
                     | Ptyp_constr ({ txt = Lident "array"; loc = arr_loc }, payload) ->
                       (match payload with
                        | [] -> Error (InvalidCollectionTypeRef arr_loc)
                        | ({ ptyp_desc =
                               ((Ptyp_constr ({ txt = Lident type_name }, [])) [@explicit_arity
                                                                                 ])
                           ; ptyp_loc
                           } as output_type)
                          :: _ ->
                          let record_type = ref None in
                          structure
                          |> List.iter (fun (item : structure_item) ->
                            match item with
                            | { pstr_desc = Pstr_type (_rec_flag, decls) } ->
                              decls
                              |> List.iter (fun (decl : type_declaration) ->
                                match decl with
                                | { ptype_name = { txt = name } } when name = type_name ->
                                  (match decl.ptype_kind with
                                   | Ptype_record fields ->
                                     record_type := Some (Ok fields)
                                   | _ ->
                                     record_type
                                       := Some
                                            (Error
                                               (CollectionTypeNotRecord decl.ptype_loc [@explicit_arity
                                                                                         ])))
                                | _ -> ())
                            | _ -> ());
                          (match !record_type with
                           | None -> Error (CollectionTypeNotFound ptyp_loc)
                           | Some (Error error) -> Error error
                           | Some (Ok fields) ->
                             Ok
                               (OutputCollection
                                  { collection =
                                      { plural = field_name; singular = type_name }
                                  ; fields =
                                      fields
                                      |> List.rev
                                      |> List.rev_map (fun (field : label_declaration) ->
                                        let open OutputFieldData in
                                        { name = field.pld_name.txt
                                        ; typ = field.pld_type |> ItemType.make
                                        ; loc = field.pld_loc
                                        })
                                  ; output_type = output_type |> ItemType.make
                                  }
                                :: entries))
                        | { ptyp_loc } :: _ -> Error (InvalidCollectionTypeRef ptyp_loc))
                     | _ -> Error (CollectionOutputNotArray field.pld_loc))))
             (Ok [])
      in
      (match entries with
       | Ok entries -> Ok (Record { loc; entries })
       | Error error -> Error (InvalidCollection error))
  ;;
end

module DebounceIntervalParser = struct
  let exists (values : value_binding list) =
    values
    |> List.exists (function
      | { pvb_pat = { ppat_desc = Ppat_var { txt = "debounceInterval" } } } -> true
      | _ -> false)
  ;;
end

module ValidatorsRecordParser = struct
  type result = (ValidatorsRecord.t, error) Stdlib.result

  and error =
    | NotFound
    | NotRecord of Location.t
    | BadTypeAnnotation of Location.t
    | ValidatorError of
        [ `BadRequiredValidator of
          InputField.validated
          * [ `Some of Location.t | `None of Location.t ]
          * [ `IncludedInDeps of InputField.validated
            | `DifferentIO of ItemType.t * ItemType.t
            ]
        ]
    | RecordParseError of Location.t

  let exists (values : value_binding list) =
    values
    |> List.exists (function
      | { pvb_pat = { ppat_desc = Ppat_var { txt = "validators" } } } -> true
      | _ -> false)
  ;;

  let parse ~rec_flag (values : value_binding list) : result option =
    values
    |> List.fold_left
         (fun res value ->
           match res with
           | Some _ -> res
           | None ->
             (match value with
              | { pvb_pat = { ppat_desc = Ppat_var { txt = "validators" } }
                ; pvb_expr =
                    { pexp_desc =
                        Pexp_constraint
                          ( expr
                          , ({ ptyp_desc = Ptyp_constr (typ, args); ptyp_loc } as
                             annotation) )
                    ; pexp_loc = constraint_pexp_loc
                    ; pexp_loc_stack = constraint_pexp_loc_stack
                    ; pexp_attributes = constraint_pexp_attributes
                    }
                } ->
                (match typ, args with
                 | { txt = Lident "validators" }, [] ->
                   (match expr with
                    | { pexp_desc = Pexp_record (fields, None)
                      ; pexp_loc = record_pexp_loc
                      ; pexp_loc_stack = record_pexp_loc_stack
                      ; pexp_attributes = record_pexp_attributes
                      } ->
                      Some
                        (Ok
                           (let open ValidatorsRecord in
                            { fields
                            ; rec_flag
                            ; annotation
                            ; constraint_metadata =
                                { pexp_loc = constraint_pexp_loc
                                ; pexp_loc_stack = constraint_pexp_loc_stack
                                ; pexp_attributes = constraint_pexp_attributes
                                }
                            ; record_metadata =
                                { pexp_loc = record_pexp_loc
                                ; pexp_loc_stack = record_pexp_loc_stack
                                ; pexp_attributes = record_pexp_attributes
                                }
                            }))
                    | { pexp_loc } -> Some (Error (NotRecord pexp_loc)))
                 | { txt = _ }, _ -> Some (Error (BadTypeAnnotation ptyp_loc)))
              | { pvb_pat = { ppat_desc = Ppat_var { txt = "validators" } }
                ; pvb_expr = { pexp_loc } as expr
                } ->
                (match expr with
                 | { pexp_desc = Pexp_record (fields, None)
                   ; pexp_loc = loc
                   ; pexp_loc_stack
                   ; pexp_attributes
                   } ->
                   Some
                     (Ok
                        { fields
                        ; rec_flag
                        ; annotation = [%type: validators]
                        ; constraint_metadata =
                            { pexp_loc; pexp_loc_stack; pexp_attributes }
                        ; record_metadata = { pexp_loc; pexp_loc_stack; pexp_attributes }
                        })
                 | { pexp_loc } -> Some (Error (NotRecord pexp_loc)))
              | _ -> None))
         None
  ;;

  let find_field (field : InputField.validated) (validators : ValidatorsRecord.fields) =
    validators
    |> List.fold_left
         (fun res validator ->
           match res, field, validator with
           | Some _, _, _ -> res
           | None, ValidatedInputField field, ({ txt = Lident key }, _) ->
             (match field.name = key with
              | true -> Some validator
              | false -> None)
           | ( None
             , ValidatedInputFieldOfCollection { collection; field }
             , ({ txt = Lident key }, { pexp_desc = Pexp_record (fields, None) }) ) ->
             if collection.plural = key
             then
               fields
               |> List.fold_left
                    (fun res entry ->
                      match res, entry with
                      | Some _, _ -> res
                      | ( None
                        , ( { txt = Lident "fields" }
                          , { pexp_desc = Pexp_record (fields, None) } ) ) ->
                        fields
                        |> List.find_opt (fun entry ->
                          match entry with
                          | { txt = Lident key }, _ -> key = field.name
                          | _ -> false)
                      | None, _ -> None)
                    None
             else None
           | ( None
             , ((ValidatedInputFieldOfCollection { collection = _; field = _ }) [@explicit_arity
                                                                                  ])
             , ({ txt = _ }, _) ) -> None
           | None, ValidatedInputField _, ({ txt = _ }, _) -> None)
         None
  ;;

  let find_collection (collection : Collection.t) (validators : ValidatorsRecord.fields) =
    validators
    |> List.fold_left
         (fun res validator ->
           match res, validator with
           | Some _, _ -> res
           | None, ({ txt = Lident key }, { pexp_desc = Pexp_record (fields, None) })
             when collection.plural = key ->
             fields
             |> List.fold_left
                  (fun res entry ->
                    match res, entry with
                    | Some _, _ -> res
                    | None, ({ txt = Lident "collection" }, exp) -> Some exp
                    | None, _ -> None)
                  None
           | None, ({ txt = _ }, _) -> None)
         None
  ;;

  let required (field : InputField.validated) (validators : ValidatorsRecord.fields) =
    match validators |> find_field field with
    | Some (_, { pexp_desc = Pexp_record _ }) -> Ok ()
    | Some
        ( _
        , { pexp_desc =
              Pexp_construct ({ txt = Lident "Some" }, Some { pexp_desc = Pexp_record _ })
          ; pexp_loc
          } ) -> Error (`Some pexp_loc)
    | Some (_, { pexp_desc = Pexp_construct ({ txt = Lident "None" }, None); pexp_loc })
      -> Error (`None pexp_loc)
    | Some _ -> Error `BadValue
    | None -> Error `NotFound
  ;;

  let optional (field : InputField.validated) (validators : ValidatorsRecord.fields) =
    match validators |> find_field field with
    | Some (_, { pexp_desc = Pexp_record _ }) -> Ok (Some ())
    | Some (_, { pexp_desc = Pexp_construct ({ txt = Lident "None" }, None) }) -> Ok None
    | Some _ -> Error `BadValue
    | None -> Error `NotFound
  ;;

  let collection (collection : Collection.t) (validators : ValidatorsRecord.fields) =
    match validators |> find_collection collection with
    | Some { pexp_desc = Pexp_fun _ } -> Ok (Some ())
    | Some { pexp_desc = Pexp_construct ({ txt = Lident "None" }, None) } -> Ok None
    | Some _ -> Error `BadValue
    | None -> Error `NotFound
  ;;
end

module Metadata = struct
  type t =
    { scheme : Scheme.t
    ; async : bool
    ; output_type : OutputTypeParser.ok
    ; validators_record : ValidatorsRecord.t
    ; message_type : unit option
    ; submission_error_type : unit option
    ; metadata : unit option
    ; debounce_interval : unit option
    }

  type error =
    | InputTypeParseError of InputTypeParser.error
    | OutputTypeParseError of OutputTypeParser.error
    | ValidatorsRecordParseError of ValidatorsRecordParser.error
    | IOMismatch of io_mismatch

  and io_mismatch =
    | InputFieldsNotInOutput of
        { fields : InputField.validated list
        ; loc : Location.t
        }
    | OutputFieldsNotInInput of { fields : OutputField.t list }
    | Both of
        { input_fields_not_in_output : InputField.validated list
        ; output_fields_not_in_input : OutputField.t list
        ; loc : Location.t
        }

  let make (structure : structure) =
    let input_parsing_result = (ref None : InputTypeParser.result option ref) in
    let output_parsing_result =
      (ref (Ok OutputTypeParser.NotProvided) : OutputTypeParser.result ref)
    in
    let validators_record_parsing_result =
      (ref None : ValidatorsRecordParser.result option ref)
    in
    let message_type = (ref None : unit option ref) in
    let submission_error_type = (ref None : unit option ref) in
    let metadata = (ref None : unit option ref) in
    let debounce_interval_value = (ref None : unit option ref) in
    structure
    |> List.iter (function
      | { pstr_desc = Pstr_type (_rec_flag, decls) } ->
        decls
        |> List.iter (function
          | { ptype_name = { txt = "input" }
            ; ptype_kind = Ptype_record fields
            ; ptype_loc
            } as decl ->
            input_parsing_result
              := (Some (fields |> InputTypeParser.parse ~decl ~structure ~loc:ptype_loc) [@explicit_arity
                                                                                          ])
          | { ptype_name = { txt = "input" }; ptype_loc } ->
            input_parsing_result := Some (Error (InputTypeParser.NotRecord ptype_loc))
          | { ptype_name = { txt = "output" }
            ; ptype_kind = Ptype_record fields
            ; ptype_loc
            } ->
            (match !input_parsing_result with
             | None -> output_parsing_result := Error (InputNotAvailable ptype_loc)
             | Some (Ok { entries }) ->
               output_parsing_result
                 := fields
                    |> OutputTypeParser.parse
                         ~structure
                         ~loc:ptype_loc
                         ~input_collections:
                           (entries
                            |> List.fold_left
                                 (fun acc (entry : InputTypeParser.unvalidated_entry) ->
                                   match entry with
                                   | UnvalidatedInputField _ -> acc
                                   | ((UnvalidatedInputCollection { collection }) [@explicit_arity
                                                                                    ]) ->
                                     collection :: acc)
                                 [])
             | Some (Error _) -> ())
          | { ptype_name = { txt = "output" }
            ; ptype_kind = Ptype_abstract
            ; ptype_loc = _
            ; ptype_manifest =
                Some { ptyp_desc = Ptyp_constr ({ txt = Lident "input" }, []) }
            } -> output_parsing_result := Ok AliasOfInput
          | { ptype_name = { txt = "output" }
            ; ptype_kind = Ptype_abstract
            ; ptype_manifest =
                Some { ptyp_desc = Ptyp_constr ({ txt = Lident alias; loc }, []) }
            } ->
            output_parsing_result := Error (OutputTypeParser.BadTypeAlias { alias; loc })
          | { ptype_name = { txt = "output" }; ptype_loc } ->
            output_parsing_result := Error (OutputTypeParser.NotRecord ptype_loc)
          | { ptype_name = { txt = "message" }; ptype_loc = _ } -> message_type := Some ()
          | { ptype_name = { txt = "metadata" }; ptype_loc = _ } -> metadata := Some ()
          | { ptype_name = { txt = "submissionError" }; ptype_loc = _ } ->
            submission_error_type := Some ()
          | _ -> ())
      | { pstr_desc = Pstr_value (rec_flag, values) } ->
        if values |> DebounceIntervalParser.exists then debounce_interval_value := Some ();
        (match values |> ValidatorsRecordParser.parse ~rec_flag with
         | Some x -> validators_record_parsing_result := Some x
         | None -> ())
      | _ -> ());
    match
      !input_parsing_result, !output_parsing_result, !validators_record_parsing_result
    with
    | Some (Error error), _, _ -> Error (InputTypeParseError error)
    | None, _, _ -> Error (InputTypeParseError NotFound)
    | _, Error error, _ -> Error (OutputTypeParseError error)
    | _, _, None -> Error (ValidatorsRecordParseError NotFound)
    | _, _, Some (Error error) -> Error (ValidatorsRecordParseError error)
    | Some (Ok input_data), Ok output_result, Some (Ok validators_record) ->
      (match input_data.entries |> InputTypeParser.validate with
       | Error error ->
         Error
           (InputTypeParseError (InvalidAttributes (InvalidFieldDeps error)) [@explicit_arity
                                                                               ]) [@explicit_arity
                                                                                    ]
       | Ok validated_input_entries ->
         let scheme =
           ((match output_result with
             | NotProvided | AliasOfInput ->
               let validator
                 ~(field : InputField.validated)
                 ~(entries : InputTypeParser.validated_entry list)
                 ~(validators_record : ValidatorsRecord.t)
                 ~(async_mode : AsyncMode.t option)
                 ~(output_type : ItemType.t)
                 : (FieldValidator.t, error) result
                 =
                 match async_mode with
                 | None ->
                   (match field |> InputTypeParser.in_deps_of entries with
                    | Some in_deps_of_entry ->
                      (match
                         validators_record.fields |> ValidatorsRecordParser.required field
                       with
                       | Ok () -> Ok (SyncValidator (Ok Required))
                       | Error (`NotFound | `BadValue) -> Ok (SyncValidator (Error ()))
                       | ((Error ((`Some _ as reason) | (`None _ as reason))) [@explicit_arity
                                                                                ]) ->
                         Error
                           (ValidatorsRecordParseError
                              (ValidatorError
                                 (`BadRequiredValidator
                                   (field, reason, `IncludedInDeps in_deps_of_entry)) [@explicit_arity
                                                                                        ])))
                    | None ->
                      (match
                         validators_record.fields |> ValidatorsRecordParser.optional field
                       with
                       | Ok res -> Ok (SyncValidator (Ok (Optional res)))
                       | Error (`NotFound | `BadValue) -> Ok (SyncValidator (Error ()))))
                 | Some mode ->
                   Ok
                     (AsyncValidator
                        { mode
                        ; optionality = output_type |> FieldOptionalityParser.parse
                        })
               in
               validated_input_entries
               |> List.fold_left
                    (fun res (entry : InputTypeParser.validated_entry) ->
                      match res, entry with
                      | Error error, _ -> Error error
                      | Ok scheme, ValidatedInputField field ->
                        let validator =
                          validator
                            ~field:(ValidatedInputField field)
                            ~entries:validated_input_entries
                            ~validators_record
                            ~async_mode:field.async
                            ~output_type:field.typ
                        in
                        (match validator with
                         | Ok validator ->
                           Ok
                             (Scheme.Field
                                { name = field.name
                                ; input_type = field.typ
                                ; output_type = field.typ
                                ; validator
                                ; deps = field.deps
                                }
                              :: scheme)
                         | Error error -> Error error)
                      | ( Ok scheme
                        , ((ValidatedInputCollection { collection; fields; input_type }) [@explicit_arity
                                                                                          ])
                        ) ->
                        let fields =
                          fields
                          |> List.fold_left
                               (fun res field ->
                                 match res with
                                 | Error error -> Error error
                                 | Ok fields ->
                                   let validator =
                                     validator
                                       ~field:
                                         (ValidatedInputFieldOfCollection
                                            { collection; field })
                                       ~entries:validated_input_entries
                                       ~validators_record
                                       ~async_mode:field.async
                                       ~output_type:field.typ
                                   in
                                   (match validator with
                                    | Ok validator ->
                                      Ok
                                        ((let open Scheme in
                                          { name = field.name
                                          ; input_type = field.typ
                                          ; output_type = field.typ
                                          ; validator
                                          ; deps = field.deps
                                          })
                                         :: fields)
                                    | Error error -> Error error))
                               (Ok [])
                        in
                        (match fields with
                         | Error error -> Error error
                         | Ok fields ->
                           Ok
                             (Scheme.Collection
                                { collection
                                ; fields
                                ; input_type
                                ; output_type = input_type
                                ; validator =
                                    (match
                                       validators_record.fields
                                       |> ValidatorsRecordParser.collection collection
                                     with
                                     | Ok res -> Ok res
                                     | Error _ -> Error ())
                                }
                              :: scheme)))
                    (Ok [])
             | Record { entries = output_entries; loc = output_loc } ->
               let validator
                 ~(input_field : InputField.validated)
                 ~(input_field_data : InputFieldData.validated)
                 ~(output_field_data : OutputFieldData.t)
                 ~(input_entries : InputTypeParser.validated_entry list)
                 ~(validators_record : ValidatorsRecord.t)
                 : (FieldValidator.t, error) result
                 =
                 match input_field_data.async with
                 | None ->
                   (match input_field |> InputTypeParser.in_deps_of input_entries with
                    | Some in_deps_of_field ->
                      (match
                         validators_record.fields
                         |> ValidatorsRecordParser.required input_field
                       with
                       | Ok () -> Ok (SyncValidator (Ok Required))
                       | Error (`NotFound | `BadValue) -> Ok (SyncValidator (Error ()))
                       | ((Error ((`Some _ as reason) | (`None _ as reason))) [@explicit_arity
                                                                                ]) ->
                         Error
                           (ValidatorsRecordParseError
                              (ValidatorError
                                 (`BadRequiredValidator
                                   (input_field, reason, `IncludedInDeps in_deps_of_field)))))
                    | None ->
                      if ItemType.eq input_field_data.typ output_field_data.typ
                      then (
                        match
                          validators_record.fields
                          |> ValidatorsRecordParser.optional input_field
                        with
                        | Ok res -> Ok (SyncValidator (Ok (Optional res)))
                        | Error (`NotFound | `BadValue) -> Ok (SyncValidator (Error ())))
                      else (
                        match
                          validators_record.fields
                          |> ValidatorsRecordParser.required input_field
                        with
                        | Ok () -> Ok (SyncValidator (Ok Required))
                        | Error (`NotFound | `BadValue) -> Ok (SyncValidator (Error ()))
                        | ((Error ((`Some _ as reason) | (`None _ as reason))) [@explicit_arity
                                                                                 ]) ->
                          Error
                            (ValidatorsRecordParseError
                               (ValidatorError
                                  (`BadRequiredValidator
                                    ( input_field
                                    , reason
                                    , `DifferentIO
                                        (input_field_data.typ, output_field_data.typ) )) [@explicit_arity
                                                                                          ]))))
                 | Some mode ->
                   Ok
                     (AsyncValidator
                        { mode
                        ; optionality =
                            output_field_data.typ |> FieldOptionalityParser.parse
                        })
               in
               let result, input_fields_not_in_output, output_fields_not_in_input =
                 validated_input_entries
                 |> List.rev
                 |> List.fold_left
                      (fun ( (result : (Scheme.t, error) result)
                           , (input_fields_not_in_output : InputField.validated list)
                           , (output_fields_not_in_input : OutputField.t list) )
                           (input_entry : InputTypeParser.validated_entry) ->
                        match input_entry with
                        | ValidatedInputField input_field_data ->
                          let output_field_data =
                            output_entries
                            |> List.fold_left
                                 (fun res (output_entry : OutputTypeParser.entry) ->
                                   match res, output_entry with
                                   | Some _, _ -> res
                                   | None, OutputField output_field_data ->
                                     (match
                                        input_field_data.name = output_field_data.name
                                      with
                                      | true -> Some output_field_data
                                      | false -> None)
                                   | None, OutputCollection _ -> None)
                                 None
                          in
                          (match result, output_field_data with
                           | _, None ->
                             ( result
                             , ValidatedInputField input_field_data
                               :: input_fields_not_in_output
                             , output_fields_not_in_input )
                           | Error error, Some _output_field_data ->
                             ( Error error
                             , input_fields_not_in_output
                             , output_fields_not_in_input
                               |> List.filter (fun (output_field : OutputField.t) ->
                                 match output_field with
                                 | OutputField output_field_data ->
                                   output_field_data.name <> input_field_data.name
                                 | OutputFieldOfCollection _ -> true) )
                           | Ok scheme, Some output_field_data ->
                             let validator =
                               validator
                                 ~input_field:(ValidatedInputField input_field_data)
                                 ~input_field_data
                                 ~output_field_data
                                 ~input_entries:validated_input_entries
                                 ~validators_record
                             in
                             ( (match validator with
                                | Error error -> Error error
                                | Ok validator ->
                                  Ok
                                    (Scheme.Field
                                       { name = input_field_data.name
                                       ; input_type = input_field_data.typ
                                       ; output_type = output_field_data.typ
                                       ; validator
                                       ; deps = input_field_data.deps
                                       }
                                     :: scheme))
                             , input_fields_not_in_output
                             , output_fields_not_in_input
                               |> List.filter (fun (output_field : OutputField.t) ->
                                 match output_field with
                                 | OutputField output_field_data ->
                                   output_field_data.name <> input_field_data.name
                                 | OutputFieldOfCollection _ -> true) ))
                        | ValidatedInputCollection
                            { collection = input_collection
                            ; fields = input_fields
                            ; input_type = input_collection_type
                            } ->
                          let output_collection =
                            output_entries
                            |> List.fold_left
                                 (fun res (output_entry : OutputTypeParser.entry) ->
                                   match res, output_entry with
                                   | Some _, _ -> res
                                   | None, OutputField _ -> res
                                   | ( None
                                     , OutputCollection
                                         { collection = output_collection
                                         ; fields
                                         ; output_type
                                         } ) ->
                                     if output_collection.plural = input_collection.plural
                                     then Some (output_collection, fields, output_type)
                                     else None)
                                 None
                          in
                          (match output_collection with
                           | None ->
                             ( (Error
                                  (OutputTypeParseError
                                     (OutputCollectionNotFound
                                        { input_collection; loc = output_loc } [@explicit_arity
                                                                                 ]) [@explicit_arity
                                                                                      ]) [@explicit_arity
                                                                                          ])
                             , input_fields
                               |> List.fold_left
                                    (fun (acc : InputField.validated list) field ->
                                      (ValidatedInputFieldOfCollection
                                         { collection = input_collection; field } [@explicit_arity
                                                                                    ])
                                      :: acc)
                                    input_fields_not_in_output
                             , output_fields_not_in_input )
                           | Some (_output_collection, output_fields, output_type) ->
                             let ( fields
                                 , input_fields_not_in_output
                                 , output_fields_not_in_input )
                               =
                               input_fields
                               |> List.rev
                               |> List.fold_left
                                    (fun ( (res : (Scheme.field list, error) result)
                                         , (input_fields_not_in_output :
                                             InputField.validated list)
                                         , (output_fields_not_in_input :
                                             OutputField.t list) )
                                         (input_field_data : InputFieldData.validated) ->
                                      let output_field_data =
                                        output_fields
                                        |> List.find_opt
                                             (fun
                                                 (output_field_data : OutputFieldData.t)
                                               ->
                                               output_field_data.name
                                               = input_field_data.name)
                                      in
                                      match res, output_field_data with
                                      | _, None ->
                                        ( res
                                        , ValidatedInputFieldOfCollection
                                            { collection = input_collection
                                            ; field = input_field_data
                                            }
                                          :: input_fields_not_in_output
                                        , output_fields_not_in_input )
                                      | Error error, Some output_field_data ->
                                        ( Error error
                                        , input_fields_not_in_output
                                        , output_fields_not_in_input
                                          |> List.filter
                                               (fun (output_field : OutputField.t) ->
                                                 match output_field with
                                                 | OutputField _ -> true
                                                 | ((OutputFieldOfCollection
                                                      { collection; field }) [@explicit_arity
                                                                               ]) ->
                                                   not
                                                     (input_collection.plural
                                                      = collection.plural
                                                      && output_field_data.name
                                                         = field.name)) )
                                      | Ok fields, Some output_field_data ->
                                        let validator =
                                          validator
                                            ~input_field:
                                              (ValidatedInputFieldOfCollection
                                                 { collection = input_collection
                                                 ; field = input_field_data
                                                 })
                                            ~input_field_data
                                            ~output_field_data
                                            ~input_entries:validated_input_entries
                                            ~validators_record
                                        in
                                        ( (match validator with
                                           | Error error -> Error error
                                           | Ok validator ->
                                             Ok
                                               ({ name = input_field_data.name
                                                ; input_type = input_field_data.typ
                                                ; output_type = output_field_data.typ
                                                ; validator
                                                ; deps = input_field_data.deps
                                                }
                                                :: fields))
                                        , input_fields_not_in_output
                                        , output_fields_not_in_input
                                          |> List.filter
                                               (fun (output_field : OutputField.t) ->
                                                 match output_field with
                                                 | OutputField _ -> true
                                                 | ((OutputFieldOfCollection
                                                      { collection; field }) [@explicit_arity
                                                                               ]) ->
                                                   not
                                                     (input_collection.plural
                                                      = collection.plural
                                                      && output_field_data.name
                                                         = field.name)) ))
                                    ( Ok []
                                    , input_fields_not_in_output
                                    , output_fields_not_in_input )
                             in
                             (match result, fields with
                              | Error _error, _ ->
                                ( result
                                , input_fields_not_in_output
                                , output_fields_not_in_input )
                              | Ok _, Error error ->
                                ( Error error
                                , input_fields_not_in_output
                                , output_fields_not_in_input )
                              | Ok scheme, Ok fields ->
                                ( Ok
                                    (Scheme.Collection
                                       { collection = input_collection
                                       ; fields
                                       ; input_type = input_collection_type
                                       ; output_type
                                       ; validator =
                                           (match
                                              validators_record.fields
                                              |> ValidatorsRecordParser.collection
                                                   input_collection
                                            with
                                            | Ok res -> Ok res
                                            | Error _ -> Error ())
                                       }
                                     :: scheme)
                                , input_fields_not_in_output
                                , output_fields_not_in_input ))))
                      (Ok [], [], output_entries |> OutputTypeParser.flatten)
               in
               (match input_fields_not_in_output, output_fields_not_in_input with
                | [], [] -> result
                | input_fields_not_in_output, [] ->
                  Error
                    (IOMismatch
                       (InputFieldsNotInOutput
                          { fields = input_fields_not_in_output; loc = output_loc } [@explicit_arity
                                                                                      ]) [@explicit_arity
                                                                                          ])
                | [], _output_entries_not_in_input ->
                  Error
                    (IOMismatch
                       (OutputFieldsNotInInput { fields = output_fields_not_in_input } [@explicit_arity
                                                                                         ]))
                | input_fields_not_in_output, output_fields_not_in_input ->
                  Error
                    (IOMismatch
                       (Both
                          { input_fields_not_in_output
                          ; output_fields_not_in_input
                          ; loc = output_loc
                          }))))
             : (Scheme.t, error) result)
         in
         (match scheme with
          | Ok scheme ->
            Ok
              { scheme
              ; async =
                  scheme
                  |> List.exists (fun (entry : Scheme.entry) ->
                    match entry with
                    | Field { validator = AsyncValidator _ } -> true
                    | Field { validator = SyncValidator _ } -> false
                    | Collection { fields } ->
                      fields
                      |> List.exists (fun (field : Scheme.field) ->
                        match field with
                        | { validator = AsyncValidator _ } -> true
                        | { validator = SyncValidator _ } -> false))
              ; output_type = output_result
              ; validators_record
              ; message_type = !message_type
              ; submission_error_type = !submission_error_type
              ; metadata = !metadata
              ; debounce_interval = !debounce_interval_value
              }
          | Error error -> Error error))
  ;;
end
