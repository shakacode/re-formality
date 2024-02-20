open Meta
open AstHelpers
open Ppxlib

let ast
  ~loc
  ~(field : Scheme.field)
  ~(metadata : unit option)
  ~(optionality : FieldOptionality.t option)
  ~(field_status_expr : expression)
  ~(validator_expr : expression)
  ~(set_status_expr : expression)
  =
  [%expr
    let result =
      [%e
        match metadata, optionality with
        | None, None ->
          [%expr
            Async.validateFieldOnBlur
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionType ->
          [%expr
            Async.validateFieldOfOptionTypeOnBlur
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some StringType ->
          [%expr
            Async.validateFieldOfStringTypeOnBlur
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionStringType ->
          [%expr
            Async.validateFieldOfOptionStringTypeOnBlur
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), None ->
          [%expr
            Async.validateFieldOnBlurWithMetadata
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionType ->
          [%expr
            Async.validateFieldOfOptionTypeOnBlurWithMetadata
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some StringType ->
          [%expr
            Async.validateFieldOfStringTypeOnBlurWithMetadata
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionStringType ->
          [%expr
            Async.validateFieldOfOptionStringTypeOnBlurWithMetadata
              ~input:state.input
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
    in
    match result with
    | None -> NoUpdate
    | Some fieldsStatuses ->
      (match [%e field.name |> E.field ~in_:"fieldsStatuses" ~loc] with
       | Validating value ->
         UpdateWithSideEffects
           ( { state with fieldsStatuses }
           , fun { state = _; dispatch } ->
               [%e
                 E.apply_field2
                   ~in_:("validators", field.name)
                   ~fn:"validateAsync"
                   ~args:
                     [ ( Nolabel
                       , match metadata with
                         | None -> [%expr value, dispatch]
                         | Some () -> [%expr value, metadata, dispatch] )
                     ]
                   ~loc] )
       | Pristine | Dirty (_, (Shown | Hidden)) -> Update { state with fieldsStatuses })]
;;
