open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~loc =
  let update_actions =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field field ->
             (FieldPrinter.update_action ~field:field.name
              |> T.constructor ~args:[ [%type: input -> input] ] ~loc)
             :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    (FieldOfCollectionPrinter.update_action ~collection ~field:field.name
                     |> T.constructor
                          ~args:[ [%type: input -> input]; [%type: index] ]
                          ~loc)
                    :: acc)
                  acc)
         []
  in
  let blur_actions =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field field ->
             (FieldPrinter.blur_action ~field:field.name |> T.constructor ~loc) :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    (FieldOfCollectionPrinter.blur_action ~collection ~field:field.name
                     |> T.constructor ~args:[ [%type: index] ] ~loc)
                    :: acc)
                  acc)
         []
  in
  let apply_async_result_actions =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field { validator = SyncValidator _ } -> acc
           | Field ({ validator = AsyncValidator _ } as field) ->
             (FieldPrinter.apply_async_result_action ~field:field.name
              |> T.constructor
                   ~args:
                     [ field.output_type |> ItemType.unpack
                     ; Typ.constr
                         (Lident "result" |> lid ~loc)
                         [ field.output_type |> ItemType.unpack
                         ; Typ.constr (Lident "message" |> lid ~loc) []
                         ]
                     ]
                   ~loc)
             :: acc
           | Collection { collection; fields } ->
             fields
             |> List.fold_left
                  (fun acc (field : Scheme.field) ->
                    match field with
                    | { validator = SyncValidator _ } -> acc
                    | { validator = AsyncValidator _ } as field ->
                      (FieldOfCollectionPrinter.apply_async_result_action
                         ~collection
                         ~field:field.name
                       |> T.constructor
                            ~args:
                              [ field.output_type |> ItemType.unpack
                              ; [%type: index]
                              ; Typ.constr
                                  (Lident "result" |> lid ~loc)
                                  [ field.output_type |> ItemType.unpack
                                  ; Typ.constr (Lident "message" |> lid ~loc) []
                                  ]
                              ]
                            ~loc)
                      :: acc)
                  acc)
         []
  in
  let collections_actions =
    scheme
    |> List.fold_left
         (fun acc (entry : Scheme.entry) ->
           match entry with
           | Field _ -> acc
           | Collection { collection; input_type } ->
             (collection
              |> CollectionPrinter.remove_action
              |> T.constructor ~args:[ [%type: index] ] ~loc)
             :: (collection
                 |> CollectionPrinter.add_action
                 |> T.constructor ~args:[ input_type |> ItemType.unpack ] ~loc)
             :: acc)
         []
  in
  let rest_actions =
    [ "Submit" |> T.constructor ~loc
    ; "SetSubmittedStatus" |> T.constructor ~args:[ [%type: input option] ] ~loc
    ; "SetSubmissionFailedStatus" |> T.constructor ~args:[ [%type: submissionError] ] ~loc
    ; "MapSubmissionError"
      |> T.constructor ~args:[ [%type: submissionError -> submissionError] ] ~loc
    ; "DismissSubmissionError" |> T.constructor ~loc
    ; "DismissSubmissionResult" |> T.constructor ~loc
    ; "Reset" |> T.constructor ~loc
    ]
  in
  Str.type_
    ~loc
    Recursive
    [ "action"
      |> str ~loc
      |> Type.mk
           ~kind:
             (Ptype_variant
                (rest_actions
                 |> List.rev_append collections_actions
                 |> List.rev_append apply_async_result_actions
                 |> List.rev_append blur_actions
                 |> List.rev_append update_actions))
    ]
;;
