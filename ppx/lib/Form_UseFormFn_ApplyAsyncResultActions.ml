open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let ast ~loc (scheme : Scheme.t) =
  scheme
  |> List.fold_left
       (fun acc (entry : Scheme.entry) ->
         match entry with
         | Field { validator = SyncValidator _ } -> acc
         | Field ({ validator = AsyncValidator _ } as field) ->
           Exp.case
             (Pat.construct
                (Lident (FieldPrinter.apply_async_result_action ~field:field.name)
                 |> lid ~loc)
                (Some
                   (Pat.tuple
                      [ Pat.var ("value" |> str ~loc); Pat.var ("result" |> str ~loc) ])))
             [%expr
               let validator = [%e field.name |> E.field ~in_:"validators" ~loc] in
               match
                 [%e field.name |> E.field2 ~in_:("state", "fieldsStatuses") ~loc]
               with
               | Validating x when validator.eq x value ->
                 Update
                   { state with
                     fieldsStatuses =
                       [%e
                         field.name
                         |> E.update_field2
                              ~in_:("state", "fieldsStatuses")
                              ~with_:[%expr Dirty (result, Shown)]
                              ~loc]
                   }
               | Validating _ | Pristine | Dirty (_, (Shown | Hidden)) -> NoUpdate]
           :: acc
         | Collection { collection; fields } ->
           fields
           |> List.fold_left
                (fun acc (field : Scheme.field) ->
                  match field.validator with
                  | SyncValidator _ -> acc
                  | AsyncValidator _ ->
                    Exp.case
                      (Pat.construct
                         (Lident
                            (FieldOfCollectionPrinter.apply_async_result_action
                               ~collection
                               ~field:field.name)
                          |> lid ~loc)
                         (Some
                            (Pat.tuple
                               [ Pat.var ("value" |> str ~loc)
                               ; Pat.var ("index" |> str ~loc)
                               ; Pat.var ("result" |> str ~loc)
                               ])))
                      [%expr
                        let validator =
                          [%e
                            field.name
                            |> E.field_of_collection_validator
                                 ~validators:"validators"
                                 ~collection
                                 ~loc]
                        in
                        match
                          [%e
                            field.name
                            |> E.field_of_collection2
                                 ~in_:("state", "fieldsStatuses")
                                 ~collection
                                 ~loc]
                        with
                        | Validating x when validator.eq x value ->
                          Update
                            { state with
                              fieldsStatuses =
                                [%e
                                  field.name
                                  |> E.update_field_of_collection2
                                       ~in_:("state", "fieldsStatuses")
                                       ~collection
                                       ~with_:[%expr Dirty (result, Shown)]
                                       ~loc]
                            }
                        | Validating _ | Pristine | Dirty (_, (Shown | Hidden)) ->
                          NoUpdate]
                    :: acc)
                acc)
       []
;;
