open Ppxlib
open Ast_helper

let ast ~loc ~async ~metadata =
  [ (if async
     then
       Exp.case
         [%pat? Submit]
         [%expr
           match state.formStatus with
           | Submitting _ -> NoUpdate
           | Editing | Submitted | SubmissionFailed _ ->
             (match
                [%e
                  match metadata with
                  | None ->
                    [%expr
                      validateForm
                        state.input
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses [@res.uapp]]
                  | Some () ->
                    [%expr
                      validateForm
                        state.input
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses
                        ~metadata [@res.uapp]]]
              with
              | Validating { fieldsStatuses; collectionsStatuses } ->
                Update { state with fieldsStatuses; collectionsStatuses }
              | Valid { output; fieldsStatuses; collectionsStatuses } ->
                UpdateWithSideEffects
                  ( { state with
                      fieldsStatuses
                    ; collectionsStatuses
                    ; formStatus =
                        Submitting
                          (match state.formStatus with
                           | SubmissionFailed error -> Some error
                           | Editing | Submitted | Submitting _ -> None)
                    ; submissionStatus = AttemptedToSubmit
                    }
                  , [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr
                          fun { state = _; dispatch } ->
                            (onSubmit
                               output
                               { notifyOnSuccess =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun input ->
                                           (dispatch
                                              (SetSubmittedStatus input) [@res.uapp])]]
                               ; notifyOnFailure =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun error ->
                                           (dispatch
                                              (SetSubmissionFailedStatus error)
                                            [@res.uapp])]]
                               ; reset =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr fun () -> (dispatch Reset [@res.uapp])]]
                               ; dismissSubmissionResult =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun () ->
                                           (dispatch DismissSubmissionResult [@res.uapp])]]
                               } [@res.uapp])]] )
              | Invalid { fieldsStatuses; collectionsStatuses } ->
                Update
                  { state with
                    fieldsStatuses
                  ; collectionsStatuses
                  ; formStatus = Editing
                  ; submissionStatus = AttemptedToSubmit
                  })]
     else
       Exp.case
         [%pat? Submit]
         [%expr
           match state.formStatus with
           | Submitting _ -> NoUpdate
           | Editing | Submitted | SubmissionFailed _ ->
             (match
                [%e
                  match metadata with
                  | None ->
                    [%expr
                      validateForm
                        state.input
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses [@res.uapp]]
                  | Some () ->
                    [%expr
                      validateForm
                        state.input
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses
                        ~metadata [@res.uapp]]]
              with
              | Valid { output; fieldsStatuses; collectionsStatuses } ->
                UpdateWithSideEffects
                  ( { state with
                      fieldsStatuses
                    ; collectionsStatuses
                    ; formStatus =
                        Submitting
                          (match state.formStatus with
                           | SubmissionFailed error -> Some error
                           | Editing | Submitted | Submitting _ -> None)
                    ; submissionStatus = AttemptedToSubmit
                    }
                  , [%e
                      Uncurried.fn
                        ~loc
                        ~arity:1
                        [%expr
                          fun { state = _; dispatch } ->
                            (onSubmit
                               output
                               { notifyOnSuccess =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun input ->
                                           (dispatch
                                              (SetSubmittedStatus input) [@res.uapp])]]
                               ; notifyOnFailure =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun error ->
                                           (dispatch
                                              (SetSubmissionFailedStatus error)
                                            [@res.uapp])]]
                               ; reset =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr fun () -> (dispatch Reset [@res.uapp])]]
                               ; dismissSubmissionResult =
                                   [%e
                                     Uncurried.fn
                                       ~loc
                                       ~arity:1
                                       [%expr
                                         fun () ->
                                           (dispatch DismissSubmissionResult [@res.uapp])]]
                               } [@res.uapp])]] )
              | Invalid { fieldsStatuses; collectionsStatuses } ->
                Update
                  { state with
                    fieldsStatuses
                  ; collectionsStatuses
                  ; formStatus = Editing
                  ; submissionStatus = AttemptedToSubmit
                  })])
  ; Exp.case
      [%pat? SetSubmittedStatus input]
      [%expr
        match input with
        | Some input ->
          Update
            { state with
              input
            ; formStatus = Submitted
            ; fieldsStatuses = initialFieldsStatuses input [@res.uapp]
            }
        | None ->
          Update
            { state with
              formStatus = Submitted
            ; fieldsStatuses = initialFieldsStatuses state.input [@res.uapp]
            }]
  ; Exp.case
      [%pat? SetSubmissionFailedStatus error]
      [%expr Update { state with formStatus = SubmissionFailed error }]
  ; Exp.case
      [%pat? MapSubmissionError map]
      [%expr
        match state.formStatus with
        | Submitting (Some error) ->
          Update { state with formStatus = Submitting (Some (map error [@res.uapp])) }
        | SubmissionFailed error ->
          Update { state with formStatus = SubmissionFailed (map error [@res.uapp]) }
        | Editing | Submitting None | Submitted -> NoUpdate]
  ; Exp.case
      [%pat? DismissSubmissionError]
      [%expr
        match state.formStatus with
        | Editing | Submitting _ | Submitted -> NoUpdate
        | SubmissionFailed _ -> Update { state with formStatus = Editing }]
  ; Exp.case
      [%pat? DismissSubmissionResult]
      [%expr
        match state.formStatus with
        | Editing | Submitting _ -> NoUpdate
        | Submitted | SubmissionFailed _ -> Update { state with formStatus = Editing }]
  ; Exp.case [%pat? Reset] [%expr Update (initialState initialInput [@res.uapp])]
  ]
;;
