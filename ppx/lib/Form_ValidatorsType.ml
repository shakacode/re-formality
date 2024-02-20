open Meta
open Ast
open Printer
open Ppxlib
open Ast_helper

let field_type ~loc ~metadata (field : Scheme.field) =
  Type.field
    (field.name |> str ~loc)
    (match field.validator with
     | SyncValidator (Ok Required)
     | SyncValidator (Ok (Optional (Some _)))
     | SyncValidator (Error ()) ->
       (match metadata with
        | None ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message )
            singleValueValidator]
        | Some () ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , metadata )
            singleValueValidatorWithMetadata])
     | SyncValidator (Ok (Optional None)) -> [%type: unit]
     | AsyncValidator _ ->
       (match metadata with
        | None ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , action )
            Async.singleValueValidator]
        | Some () ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , metadata
            , action )
            Async.singleValueValidatorWithMetadata]))
;;

let collection_type
  ~loc
  ~(validator : CollectionValidator.t)
  ~(metadata : unit option)
  (collection : Collection.t)
  =
  Type.field
    (collection.plural |> str ~loc)
    (match validator with
     | Ok (Some ()) | Error () ->
       (match metadata with
        | None ->
          [%type:
            ( input
            , message
            , [%t
                Typ.constr
                  (Lident (collection |> CollectionPrinter.validator_type) |> lid ~loc)
                  []] )
            collectionValidatorWithWholeCollectionValidator]
        | Some () ->
          [%type:
            ( input
            , message
            , [%t
                Typ.constr
                  (Lident (collection |> CollectionPrinter.validator_type) |> lid ~loc)
                  []]
            , metadata )
            collectionValidatorWithWholeCollectionValidatorAndMetadata])
     | Ok None ->
       [%type:
         [%t
           Typ.constr
             (Lident (collection |> CollectionPrinter.validator_type) |> lid ~loc)
             []]
         collectionValidatorWithoutWholeCollectionValidator])
;;

let field_of_collection_type ~loc ~(metadata : unit option) (field : Scheme.field) =
  Type.field
    (field.name |> str ~loc)
    (match field.validator with
     | SyncValidator (Ok Required)
     | SyncValidator (Ok (Optional (Some _)))
     | SyncValidator (Error ()) ->
       (match metadata with
        | None ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message )
            valueOfCollectionValidator]
        | Some () ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , metadata )
            valueOfCollectionValidatorWithMetadata])
     | SyncValidator (Ok (Optional None)) -> [%type: unit]
     | AsyncValidator _ ->
       (match metadata with
        | None ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , action )
            Async.valueOfCollectionValidator]
        | Some () ->
          [%type:
            ( input
            , [%t field.output_type |> ItemType.unpack]
            , message
            , metadata
            , action )
            Async.valueOfCollectionValidatorWithMetadata]))
;;

let ast ~(scheme : Scheme.t) ~(metadata : unit option) ~loc =
  let main_decl =
    "validators"
    |> str ~loc
    |> Type.mk
         ~kind:
           (Ptype_record
              (scheme
               |> List.rev
               |> List.rev_map (fun (entry : Scheme.entry) ->
                 match entry with
                 | Field field -> field |> field_type ~loc ~metadata
                 | Collection { collection; validator } ->
                   collection |> collection_type ~validator ~metadata ~loc)))
  in
  let collections_decls =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field _ -> acc
           | Collection { collection; fields } ->
             (collection
              |> CollectionPrinter.validator_type
              |> str ~loc
              |> Type.mk
                   ~kind:
                     (Ptype_record
                        (fields
                         |> List.rev
                         |> List.rev_map (field_of_collection_type ~loc ~metadata))))
             :: acc)
         []
  in
  Str.type_ ~loc Recursive (main_decl :: collections_decls)
;;
