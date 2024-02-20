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
    let nextFieldsStatuses =
      [%e
        match metadata, optionality with
        | None, None ->
          [%expr
            Async.validateFieldOnChangeInOnChangeMode
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionType ->
          [%expr
            Async.validateFieldOfOptionTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some StringType ->
          [%expr
            Async.validateFieldOfStringTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionStringType ->
          [%expr
            Async.validateFieldOfOptionStringTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), None ->
          [%expr
            Async.validateFieldOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionType ->
          [%expr
            Async.validateFieldOfOptionTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some StringType ->
          [%expr
            Async.validateFieldOfStringTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionStringType ->
          [%expr
            Async.validateFieldOfOptionStringTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
    in
    match [%e field.name |> E.field ~in_:"nextFieldsStatuses" ~loc] with
    | Validating value ->
      UpdateWithSideEffects
        ( { state with input = nextInput; fieldsStatuses = nextFieldsStatuses }
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
    | Pristine | Dirty (_, (Shown | Hidden)) ->
      Update { state with input = nextInput; fieldsStatuses = nextFieldsStatuses }]
;;
