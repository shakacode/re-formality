open Meta
open Ast
open AstHelpers
open Printer
open Ppxlib
open Ast_helper

let ast ~loc ~(metadata : unit option) (scheme : Scheme.t) =
  scheme
  |> List.fold_left
       (fun acc (entry : Scheme.entry) ->
         match entry with
         | Field field ->
           Exp.case
             (Pat.construct
                ~attrs:[ explicit_arity ~loc ]
                (Lident (FieldPrinter.update_action ~field:field.name) |> lid ~loc)
                (Some (Pat.tuple [ Pat.var ("nextInputFn" |> str ~loc) ])))
             (match field.deps with
              | [] ->
                let field_status_expr =
                  field.name |> E.field2 ~in_:("state", "fieldsStatuses") ~loc
                in
                let field_input_expr = field.name |> E.field ~in_:"nextInput" ~loc in
                let validator_expr = field.name |> E.field ~in_:"validators" ~loc in
                let set_status_expr =
                  field.name
                  |> E.update_field2
                       ~in_:("state", "fieldsStatuses")
                       ~with_:[%expr status]
                       ~loc
                in
                [%expr
                  let nextInput = nextInputFn state.input in
                  [%e
                    match field.validator with
                    | SyncValidator validator ->
                      Form_UseFormFn_UpdateActions_SyncField.ast
                        ~loc
                        ~validator
                        ~metadata
                        ~field_status_expr
                        ~field_input_expr
                        ~validator_expr
                        ~set_status_expr
                    | AsyncValidator { mode = OnBlur; optionality } ->
                      Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast
                        ~loc
                        ~metadata
                        ~optionality
                        ~field_status_expr
                        ~validator_expr
                        ~set_status_expr
                    | AsyncValidator { mode = OnChange; optionality } ->
                      Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast
                        ~loc
                        ~field
                        ~metadata
                        ~optionality
                        ~field_status_expr
                        ~validator_expr
                        ~set_status_expr]]
              | dep :: deps ->
                [%expr
                  let nextInput = nextInputFn state.input in
                  let nextFieldsStatuses = ref state.fieldsStatuses in
                  [%e
                    scheme
                    |> Form_UseFormFn_DependentFields.ast
                         ~loc
                         ~dep
                         ~deps
                         ~trigger:(`Field field.name)
                         ~metadata];
                  [%e
                    let field_status_expr =
                      field.name |> E.ref_field ~in_:"nextFieldsStatuses" ~loc
                    in
                    let field_input_expr = field.name |> E.field ~in_:"nextInput" ~loc in
                    let validator_expr = field.name |> E.field ~in_:"validators" ~loc in
                    let set_status_expr =
                      field.name
                      |> E.update_ref_field
                           ~in_:"nextFieldsStatuses"
                           ~with_:[%expr status]
                           ~loc
                    in
                    match field.validator with
                    | SyncValidator validator ->
                      Form_UseFormFn_UpdateActions_SyncField.ast
                        ~loc
                        ~validator
                        ~metadata
                        ~field_status_expr
                        ~field_input_expr
                        ~validator_expr
                        ~set_status_expr
                    | AsyncValidator { mode = OnBlur; optionality } ->
                      Form_UseFormFn_UpdateActions_AsyncFieldInOnBlurMode.ast
                        ~loc
                        ~metadata
                        ~optionality
                        ~field_status_expr
                        ~validator_expr
                        ~set_status_expr
                    | AsyncValidator { mode = OnChange; optionality } ->
                      Form_UseFormFn_UpdateActions_AsyncFieldInOnChangeMode.ast
                        ~loc
                        ~field
                        ~metadata
                        ~optionality
                        ~field_status_expr
                        ~validator_expr
                        ~set_status_expr]])
           :: acc
         | Collection { collection; fields } ->
           fields
           |> List.fold_left
                (fun acc (field : Scheme.field) ->
                  Exp.case
                    (Pat.construct
                       ~attrs:[ explicit_arity ~loc ]
                       (Lident
                          (FieldOfCollectionPrinter.update_action
                             ~collection
                             ~field:field.name)
                        |> lid ~loc)
                       (Some
                          (Pat.tuple
                             [ Pat.var ("nextInputFn" |> str ~loc)
                             ; Pat.var ("index" |> str ~loc)
                             ])))
                    (match field.deps with
                     | [] ->
                       let field_status_expr =
                         field.name
                         |> E.field_of_collection2
                              ~in_:("state", "fieldsStatuses")
                              ~collection
                              ~loc
                       in
                       let field_input_expr =
                         field.name
                         |> E.field_of_collection ~in_:"nextInput" ~collection ~loc
                       in
                       let validator_expr =
                         field.name
                         |> E.field_of_collection_validator
                              ~validators:"validators"
                              ~collection
                              ~loc
                       in
                       let set_status_expr =
                         field.name
                         |> E.update_field_of_collection2
                              ~in_:("state", "fieldsStatuses")
                              ~collection
                              ~with_:[%expr status]
                              ~loc
                       in
                       [%expr
                         let nextInput = nextInputFn state.input in
                         [%e
                           match field.validator with
                           | SyncValidator validator ->
                             Form_UseFormFn_UpdateActions_SyncFieldOfCollection.ast
                               ~loc
                               ~validator
                               ~metadata
                               ~field_status_expr
                               ~field_input_expr
                               ~validator_expr
                               ~set_status_expr
                           | AsyncValidator { mode = OnBlur; optionality } ->
                             Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnBlurMode
                             .ast
                               ~loc
                               ~metadata
                               ~optionality
                               ~field_status_expr
                               ~validator_expr
                               ~set_status_expr
                           | AsyncValidator { mode = OnChange; optionality } ->
                             Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnChangeMode
                             .ast
                               ~loc
                               ~field
                               ~collection
                               ~metadata
                               ~optionality
                               ~field_status_expr
                               ~validator_expr
                               ~set_status_expr]]
                     | dep :: deps ->
                       [%expr
                         let nextInput = nextInputFn state.input in
                         let nextFieldsStatuses = ref state.fieldsStatuses in
                         [%e
                           scheme
                           |> Form_UseFormFn_DependentFields.ast
                                ~loc
                                ~dep
                                ~deps
                                ~trigger:(`FieldOfCollection (collection, field.name))
                                ~metadata];
                         [%e
                           let field_status_expr =
                             field.name
                             |> E.ref_field_of_collection
                                  ~in_:"nextFieldsStatuses"
                                  ~collection
                                  ~loc
                           in
                           let field_input_expr =
                             field.name
                             |> E.field_of_collection ~in_:"nextInput" ~collection ~loc
                           in
                           let validator_expr =
                             field.name
                             |> E.field_of_collection_validator
                                  ~validators:"validators"
                                  ~collection
                                  ~loc
                           in
                           let set_status_expr =
                             field.name
                             |> E.update_ref_field_of_collection
                                  ~in_:"nextFieldsStatuses"
                                  ~collection
                                  ~with_:[%expr status]
                                  ~loc
                           in
                           match field.validator with
                           | SyncValidator validator ->
                             Form_UseFormFn_UpdateActions_SyncFieldOfCollection.ast
                               ~loc
                               ~validator
                               ~metadata
                               ~field_status_expr
                               ~field_input_expr
                               ~validator_expr
                               ~set_status_expr
                           | AsyncValidator { mode = OnBlur; optionality } ->
                             Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnBlurMode
                             .ast
                               ~loc
                               ~metadata
                               ~optionality
                               ~field_status_expr
                               ~validator_expr
                               ~set_status_expr
                           | AsyncValidator { mode = OnChange; optionality } ->
                             Form_UseFormFn_UpdateActions_AsyncFieldOfCollectionInOnChangeMode
                             .ast
                               ~loc
                               ~field
                               ~collection
                               ~metadata
                               ~optionality
                               ~field_status_expr
                               ~validator_expr
                               ~set_status_expr]])
                  :: acc)
                acc)
       []
;;
