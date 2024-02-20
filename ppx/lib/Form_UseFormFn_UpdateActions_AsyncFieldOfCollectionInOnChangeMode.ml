open Meta
open AstHelpers
open Ppxlib

let ast
  ~loc
  ~(field : Scheme.field)
  ~(collection : Collection.t)
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
            Async.validateFieldOfCollectionOnChangeInOnChangeMode
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some StringType ->
          [%expr
            Async.validateFieldOfCollectionOfStringTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionStringType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionStringTypeOnChangeInOnChangeMode
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), None ->
          [%expr
            Async.validateFieldOfCollectionOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some StringType ->
          [%expr
            Async.validateFieldOfCollectionOfStringTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionStringType ->
          [%expr
            Async
            .validateFieldOfCollectionOfOptionStringTypeOnChangeInOnChangeModeWithMetadata
              ~input:nextInput
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~submissionStatus:state.submissionStatus
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
    in
    match
      [%e field.name |> E.field_of_collection ~in_:"nextFieldsStatuses" ~collection ~loc]
    with
    | Validating value ->
      UpdateWithSideEffects
        ( { state with input = nextInput; fieldsStatuses = nextFieldsStatuses }
        , fun { state = _; dispatch } ->
            [%e
              E.apply_field4
                ~in_:("validators", collection.plural, "fields", field.name)
                ~fn:"validateAsync"
                ~args:
                  [ ( Nolabel
                    , match metadata with
                      | None -> [%expr value, index, dispatch]
                      | Some () -> [%expr value, index, metadata, dispatch] )
                  ]
                ~loc] )
    | Pristine | Dirty (_, (Shown | Hidden)) ->
      Update { state with input = nextInput; fieldsStatuses = nextFieldsStatuses }]
;;
