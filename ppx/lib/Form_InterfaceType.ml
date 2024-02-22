open Meta
open Ast
open Printer
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~(async : bool) ~loc =
  let f x t = t |> Type.field (x |> str ~loc) in
  let base =
    [ f "input" [%type: input]
    ; f "status" [%type: submissionError formStatus]
    ; f "dirty" (Uncurried.ty ~loc ~arity:1 [%type: unit -> bool])
    ; f
        "valid"
        (match async with
         | true -> Uncurried.ty ~loc ~arity:1 [%type: unit -> bool option]
         | false -> Uncurried.ty ~loc ~arity:1 [%type: unit -> bool])
    ; f "submitting" [%type: bool]
    ; f "submit" (Uncurried.ty ~loc ~arity:1 [%type: unit -> unit])
    ; f "dismissSubmissionError" (Uncurried.ty ~loc ~arity:1 [%type: unit -> unit])
    ; f "dismissSubmissionResult" (Uncurried.ty ~loc ~arity:1 [%type: unit -> unit])
    ; f
        "mapSubmissionError"
        (Uncurried.ty
           ~loc
           ~arity:1
           [%type:
             [%t Uncurried.ty ~loc ~arity:1 [%type: submissionError -> submissionError]]
             -> unit])
    ; f "reset" (Uncurried.ty ~loc ~arity:1 [%type: unit -> unit])
    ]
  in
  let update_fns =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field field ->
             f
               (FieldPrinter.update_fn ~field:field.name)
               (Uncurried.ty
                  ~loc
                  ~arity:2
                  [%type:
                    [%t
                      Uncurried.ty
                        ~loc
                        ~arity:2
                        [%type:
                          input -> [%t field.input_type |> ItemType.unpack] -> input]]
                    -> [%t field.input_type |> ItemType.unpack]
                    -> unit])
             :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    f
                      (FieldOfCollectionPrinter.update_fn ~collection ~field:field.name)
                      (Uncurried.ty
                         ~loc
                         ~arity:3
                         [%type:
                           at:index
                           -> [%t
                                Uncurried.ty
                                  ~loc
                                  ~arity:2
                                  [%type:
                                    input
                                    -> [%t field.input_type |> ItemType.unpack]
                                    -> input]]
                           -> [%t field.input_type |> ItemType.unpack]
                           -> unit])
                    :: acc)
                  acc)
         []
  in
  let blur_fns =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field field ->
             f
               (FieldPrinter.blur_fn ~field:field.name)
               (Uncurried.ty ~loc ~arity:1 [%type: unit -> unit])
             :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    f
                      (FieldOfCollectionPrinter.blur_fn ~collection ~field:field.name)
                      (Uncurried.ty ~loc ~arity:1 [%type: at:index -> unit])
                    :: acc)
                  acc)
         []
  in
  let result_entries =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field field ->
             f
               (FieldPrinter.result_value ~field:field.name)
               (match field.validator with
                | SyncValidator _ ->
                  [%type:
                    ([%t field.output_type |> ItemType.unpack], message) result option]
                | AsyncValidator _ ->
                  [%type:
                    ( [%t field.output_type |> ItemType.unpack]
                    , message )
                    Async.exposedFieldStatus
                    option])
             :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    f
                      (FieldOfCollectionPrinter.result_fn ~collection ~field:field.name)
                      (match field.validator with
                       | SyncValidator _ ->
                         Uncurried.ty
                           ~loc
                           ~arity:1
                           [%type:
                             at:index
                             -> ( [%t field.output_type |> ItemType.unpack]
                                , message )
                                result
                                option]
                       | AsyncValidator _ ->
                         Uncurried.ty
                           ~loc
                           ~arity:1
                           [%type:
                             at:index
                             -> ( [%t field.output_type |> ItemType.unpack]
                                , message )
                                Async.exposedFieldStatus
                                option])
                    :: acc)
                  acc)
         []
  in
  let collection_entries =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field _ -> acc
           | Collection { collection; validator; input_type } ->
             let add_fn =
               f
                 (collection |> CollectionPrinter.add_fn)
                 (Uncurried.ty
                    ~loc
                    ~arity:1
                    [%type: [%t input_type |> ItemType.unpack] -> unit])
             in
             let remove_fn =
               f
                 (collection |> CollectionPrinter.remove_fn)
                 (Uncurried.ty ~loc ~arity:1 [%type: at:index -> unit])
             in
             let result_value =
               match validator with
               | Ok (Some ()) | Error () ->
                 Some
                   (f
                      (collection |> CollectionPrinter.result_value)
                      [%type: message collectionStatus option])
               | Ok None -> None
             in
             (match result_value with
              | Some result_value -> result_value :: remove_fn :: add_fn :: acc
              | None -> remove_fn :: add_fn :: acc))
         []
  in
  Str.type_
    ~loc
    Recursive
    [ "interface"
      |> str ~loc
      |> Type.mk
           ~kind:
             (Ptype_record
                (base
                 |> List.rev_append collection_entries
                 |> List.rev_append result_entries
                 |> List.rev_append blur_fns
                 |> List.rev_append update_fns))
    ]
;;
