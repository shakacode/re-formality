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
    let result =
      [%e
        match metadata, optionality with
        | None, None ->
          [%expr
            Async.validateFieldOfCollectionOnBlur
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionTypeOnBlur
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some StringType ->
          [%expr
            Async.validateFieldOfCollectionOfStringTypeOnBlur
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | None, Some OptionStringType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionStringTypeOnBlur
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), None ->
          [%expr
            Async.validateFieldOfCollectionOnBlurWithMetadata
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionTypeOnBlurWithMetadata
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some StringType ->
          [%expr
            Async.validateFieldOfCollectionOfStringTypeOnBlurWithMetadata
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]
        | Some (), Some OptionStringType ->
          [%expr
            Async.validateFieldOfCollectionOfOptionStringTypeOnBlurWithMetadata
              ~input:state.input
              ~index
              ~fieldStatus:[%e field_status_expr]
              ~validator:[%e validator_expr]
              ~metadata
              ~setStatus:[%e [%expr fun status -> [%e set_status_expr]]]]]
    in
    match result with
    | None -> NoUpdate
    | Some fieldsStatuses ->
      (match
         [%e field.name |> E.field_of_collection ~in_:"fieldsStatuses" ~collection ~loc]
       with
       | Validating value ->
         UpdateWithSideEffects
           ( { state with fieldsStatuses }
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
       | Pristine | Dirty (_, (Shown | Hidden)) -> Update { state with fieldsStatuses })]
;;
