open Meta
open Ast
open Printer
open Ppxlib
open Ast_helper

let field_type ~loc (field : Scheme.field) =
  Type.field
    (field.name |> str ~loc)
    (match field.validator with
     | SyncValidator _ ->
       [%type: ([%t field.output_type |> ItemType.unpack], message) fieldStatus]
     | AsyncValidator _ ->
       [%type: ([%t field.output_type |> ItemType.unpack], message) Async.fieldStatus])
;;

let collection_type ~loc (collection : Collection.t) =
  Type.field
    (collection.plural |> str ~loc)
    [%type:
      [%t
        Typ.constr
          (Lident (collection |> CollectionPrinter.fields_statuses_type) |> lid ~loc)
          []]
      array]
;;

let ast ~(scheme : Scheme.t) ~loc =
  let main_decl =
    "fieldsStatuses"
    |> str ~loc
    |> Type.mk
         ~kind:
           (Ptype_record
              (scheme
               |> List.rev
               |> List.rev_map (fun (entry : Scheme.entry) ->
                 match entry with
                 | Field field -> field |> field_type ~loc
                 | Collection { collection } -> collection |> collection_type ~loc)))
  in
  let collections_decls =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field _ -> acc
           | Collection { collection; fields } ->
             (collection
              |> CollectionPrinter.fields_statuses_type
              |> str ~loc
              |> Type.mk
                   ~kind:
                     (Ptype_record (fields |> List.rev |> List.rev_map (field_type ~loc)))
             )
             :: acc)
         []
  in
  Str.type_ ~loc Recursive (main_decl :: collections_decls)
;;
