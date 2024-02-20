open Meta
open Ppxlib

let ast
  ~loc
  ~(validator : (FieldValidator.sync, unit) result)
  ~(metadata : unit option)
  ~(field_status_expr : expression)
  ~(field_input_expr : expression)
  ~(validator_expr : expression)
  ~(set_status_expr : expression)
  =
  [%expr
    let result =
      [%e
        match validator with
        | Ok (Required | Optional (Some _)) | Error () ->
          (match metadata with
           | None ->
             [%expr
               validateFieldOfCollectionOnBlurWithValidator
                 ~input:state.input
                 ~index
                 ~fieldStatus:[%e field_status_expr]
                 ~validator:[%e validator_expr]
                 ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
           | Some () ->
             [%expr
               validateFieldOfCollectionOnBlurWithValidatorAndMetadata
                 ~input:state.input
                 ~index
                 ~fieldStatus:[%e field_status_expr]
                 ~validator:[%e validator_expr]
                 ~metadata
                 ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]])
        | Ok (Optional None) ->
          [%expr
            validateFieldOnBlurWithoutValidator
              ~fieldInput:[%e field_input_expr]
              ~fieldStatus:[%e field_status_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
    in
    match result with
    | Some fieldsStatuses -> Update { state with fieldsStatuses }
    | None -> NoUpdate]
;;
