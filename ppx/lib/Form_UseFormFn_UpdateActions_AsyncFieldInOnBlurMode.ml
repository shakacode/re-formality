open Meta
open Ppxlib

let ast
  ~loc
  ~(metadata : unit option)
  ~(optionality : FieldOptionality.t option)
  ~(field_status_expr : expression)
  ~(validator_expr : expression)
  ~(set_status_expr : expression)
  =
  [%expr
    Update
      { state with
        input = nextInput
      ; fieldsStatuses =
          [%e
            match metadata, optionality with
            | None, None ->
              [%expr
                Async.validateFieldOnChangeInOnBlurMode
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | None, Some OptionType ->
              [%expr
                Async.validateFieldOfOptionTypeOnChangeInOnBlurMode
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | None, Some StringType ->
              [%expr
                Async.validateFieldOfStringTypeOnChangeInOnBlurMode
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | None, Some OptionStringType ->
              [%expr
                Async.validateFieldOfOptionStringTypeOnChangeInOnBlurMode
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | Some (), None ->
              [%expr
                Async.validateFieldOnChangeInOnBlurModeWithMetadata
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~metadata
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | Some (), Some OptionType ->
              [%expr
                Async.validateFieldOfOptionTypeOnChangeInOnBlurModeWithMetadata
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~metadata
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | Some (), Some StringType ->
              [%expr
                Async.validateFieldOfStringTypeOnChangeInOnBlurModeWithMetadata
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~metadata
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]
            | Some (), Some OptionStringType ->
              [%expr
                Async.validateFieldOfOptionStringTypeOnChangeInOnBlurModeWithMetadata
                  ~input:nextInput
                  ~fieldStatus:[%e field_status_expr]
                  ~submissionStatus:state.submissionStatus
                  ~validator:[%e validator_expr]
                  ~metadata
                  ~setStatus:
                    [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr fun status -> [%e set_status_expr]]] [@res.uapp]]]
      }]
;;
