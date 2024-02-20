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
    Update
      { state with
        input = nextInput
      ; fieldsStatuses =
          [%e
            match validator with
            | Ok (Required | Optional (Some _)) | Error () ->
              (match metadata with
               | None ->
                 [%expr
                   validateFieldOnChangeWithValidator
                     ~input:nextInput
                     ~fieldStatus:[%e field_status_expr]
                     ~submissionStatus:state.submissionStatus
                     ~validator:[%e validator_expr]
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
               | Some () ->
                 [%expr
                   validateFieldOnChangeWithValidatorAndMetadata
                     ~input:nextInput
                     ~metadata
                     ~fieldStatus:[%e field_status_expr]
                     ~submissionStatus:state.submissionStatus
                     ~validator:[%e validator_expr]
                     ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]])
            | Ok (Optional None) ->
              [%expr
                validateFieldOnChangeWithoutValidator
                  ~fieldInput:[%e field_input_expr]
                  ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
      }]
;;
